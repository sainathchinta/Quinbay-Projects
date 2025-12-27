package com.gdn.x.product.service.impl;

import static com.gdn.x.product.enums.ProductFieldNames.UPC_CODE;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anySet;
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
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.product.domain.event.model.BusinessHourVOEventModel;
import com.gdn.x.product.domain.event.model.ContactPersonVOEventModel;
import com.gdn.x.product.domain.event.model.GeolocationVOEventModel;
import com.gdn.x.product.domain.event.model.PickupPointFlagVOEventModel;
import com.gdn.x.product.domain.event.model.PickupPointVOEventModel;
import com.gdn.x.product.enums.DayOfWeek;
import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.domain.event.model.OdooCreationEventModel;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import org.apache.commons.collections.CollectionUtils;
import org.dozer.DozerBeanMapper;
import org.joda.time.DateTime;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.base.mapper.impl.DozerMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.businesspartner.domain.event.model.CompanyVO;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.dto.ResponsiblePersonDTO;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.model.DiscountPriceModel;
import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.PriceModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.ProductEventModel;
import com.gdn.x.product.domain.event.model.SalesCatalogModel;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.AdjustmentType;
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
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.B2bFieldsVo;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboRuleVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCatalogVOV2;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemV2;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfferedSummaryVo;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.PristineItemDetailAndMappingVo;
import com.gdn.x.product.model.vo.PristineItemVO;
import com.gdn.x.product.model.vo.PristineSimilarItemVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.SimpleAsyncMasterDataItemVO;
import com.gdn.x.product.model.vo.SimpleItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataItemVO;
import com.gdn.x.product.model.vo.SimpleMasterDataProductVO;
import com.gdn.x.product.model.vo.SimpleProductAndItemsAndItemPickupPointV0;
import com.gdn.x.product.model.vo.SimpleProductAndItemsVO;
import com.gdn.x.product.model.vo.WholesaleRuleVO;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.util.ItemSummaryUtil;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import com.gdn.x.promotion.rest.web.model.dto.request.SimpleSetStringRequest;
import com.gdn.x.promotion.rest.web.model.promo.bundling.WholesaleRule;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

public class ObjectConverterServiceImplTest {

  private static final String PRISTINE_ID = "pristineId";
  private static final String BIG_PRODUCT = "Big Product";
  private static final String PRISTINE_PRODUCT_NAME = "pristineProductName";
  private static final String ATTRIBUTE_VALUE = "attributeValue";
  private static final String CATALOG_CODE = "12051";
  private static final String CATEGORY_CODE = "category-code";
  private static final String CATEGORY_CODE_C1 = "category-code-c1";
  private static final String CATEGORY_CODE_CN = "category-code-cn";
  private static final String CATEGORY_CODE_CN_2 = "category-code-cn-2";
  private static final String CATEGORY_CODE1 = "category-code1";
  private static final String CATEGORY_NAME = "category-name";
  private static final String BRAND = "brand";
  private static final String MODEL = "NEW";
  private static final int LEVEL = 1;
  private static final String CATALOG_ID = "catalog-id";
  private static final String MERCHANT_CODE = "merchant-code";
  private static final String PRODUCT_TYPE_CODE = "product-type-code";
  private static final String PRODUCT_TYPE_NAME = "product-type-name";
  private static final String PRODUCT_SKU2 = "product-sku";
  private static final String PRODUCT_CODE = "product-code";
  private static final String HALAL_PRODUCT = "halal-product";
  private static final String ITEM_CODE_2 = "item-code-2";
  private static final String ITEM_CODE_1 = "item-code-1";
  private static final String ITEM_SKU_4 = "item-sku-4";
  private static final String ITEM_SKU_3 = "item-sku-3";
  private static final String ITEM_SKU_2 = "item-sku-2";
  private static final String ITEM_SKU_1 = "item-sku-1";
  private static final String PRODUCT_SKU_2 = "product-sku-2";
  private static final String PRODUCT_SKU_1 = "product-sku-1";
  private static final String DEFAULT_CHANNEL = "default channel";
  private static final String OTHER_CHANNEL = "other channel";
  private static final String CATEGORY_CODE_MASTER_C1 = "category-code-master-c1";
  private static final String CATEGORY_NAME_MASTER_C1 = "category-name-master-c1";
  private static final String CATEGORY_CODE_MASTER_C2 = "category-code-master-c2";
  private static final String CATEGORY_NAME_MASTER_C2 = "category-name-master-c2";
  private static final String CATEGORY_CODE_MASTER_C3 = "category-code-master-c3";
  private static final String CATEGORY_NAME_MASTER_C3 = "category-name-master-c3";
  private static final String CATEGORY_NAME_SALES_A1_C1 = "category-name-sales-a1-c1";
  private static final String CATEGORY_CODE_SALES_A1_C1 = "category-code-sales-a1-c1";
  private static final String CATEGORY_NAME_SALES_A1_C2 = "category-name-sales-a1-c2";
  private static final String CATEGORY_CODE_SALES_A1_C2 = "category-code-sales-a1-c2";
  private static final String CATEGORY_NAME_SALES_A2_C1 = "category-name-sales-a2-c1";
  private static final String CATEGORY_CODE_SALES_A2_C1 = "category-code-sales-a2-c1";
  private static final String CATEGORY_NAME_SALES_A2_C2 = "category-name-sales-a2-c2";
  private static final String CATEGORY_CODE_SALES_A2_C2 = "category-code-sales-a2-c2";
  private static final String CATEGORY_NAME_SALES_B1_C1 = "category-name-sales-b1-c1";
  private static final String CATEGORY_CODE_SALES_B1_C1 = "category-code-sales-b1-c1";
  private static final String CATEGORY_NAME_SALES_B1_C2 = "category-name-sales-b1-c2";
  private static final String CATEGORY_CODE_SALES_B1_C2 = "category-code-sales-b1-c2";
  private static final String CATEGORY_NAME_SALES_B2_C1 = "category-name-sales-b2-c1";
  private static final String CATEGORY_CODE_SALES_B2_C1 = "category-code-sales-b2-c1";
  private static final String CATEGORY_NAME_SALES_B2_C2 = "category-name-sales-b2-c2";
  private static final String CATEGORY_CODE_SALES_B2_C2 = "category-code-sales-b2-c2";
  private static final String CATALOG_CODE_MASTER = ObjectConverterServiceImplTest.CATALOG_CODE;
  private static final String CATALOG_CODE_SALES_A = "catalog-code-sales-a";
  private static final String CATALOG_CODE_SALES_B = "catalog-code-sales-b";
  private static final String MERCHANT_ID = "merchant-id";
  private static final ProductType PRODUCT_TYPE = ProductType.REGULAR;
  private static final String PRODUCT_CATENTRY_ID = "product-catentry-id";
  private static final String PRODUCT_SKU = ObjectConverterServiceImplTest.PRODUCT_SKU2;
  private static final String ITEM_SKU = "item-sku";
  private static final String MERCHANT_SKU = "merchant-sku";
  private static final String CATENTRY_ID = "catentry-id";
  private static final double OFFER_PRICE = 10000;
  private static final String GENERATED_ITEM_NAME = "item-name";
  private static final double ITEM_LENGTH = 50;
  private static final double ITEM_WIDTH = 30;
  private static final double ITEM_HEIGHT = 165;
  private static final double ITEM_WEIGHT = 100;
  private static final double PRICE_1 = 10.0;
  private static final double DELIVERY_WEIGHT = 100;
  private static final String CITY_NAME = "city-name";
  private static final String ADDRESS1 = "address1";
  private static final String PROVINCE_NAME = "province-name";
  private static final String ZIP_CODE = "zip-code";
  private static final String BUSINESS_PARTNER_NAME = "business-partner-name";
  private static final String NAME = "name";
  private static final String PRISTINE_NAME = "pristineNamexz";
  private static final String PHONE = "phone";
  private static final String EMAIL = "email";
  private static final String PURCHASE_TERM = "purchase-term";
  private static final String RESPONSIBLE_PERSON_NAME = "responsible-person-name";
  private static final String CHANNEL = ChannelName.MOBILE_WEB.toString();
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTRIBUTE_NAME = "attribute-name";
  private static final String DEFINING_ATTRIBUTE = "DEFINING_ATTRIBUTE";
  private static final boolean SEARCHABLE = true;
  private static final boolean IS_SKU_VALUE = true;
  private static final boolean IS_BASIC_VIEW = true;
  private static final boolean MANDATORY = true;
  private static final boolean ONLINE = false;
  private static final boolean BUYABLE = false;
  private static final boolean DISCOVERABLE = false;
  private static final boolean CNC_ACTIVE = false;
  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String DESCRIPTION = "description";
  private static final Date START_DATE_TIME = new Date();
  private static final Date END_DATE_TIME = new Date();
  private static final double SALE_PRICE = 0;
  private static final String IDR = "IDR";
  private static final String GARANSI = "garansi";
  private static final String GUARANTEE = "guarantee";
  private static final String LAMA_GARANSI = "lama garansi";
  private static final String GUARANTEE_TYPE = "distributor guarantee";
  private static final String GUARANTEE_DURATION = "1 Tahun";
  private static final String COLOUR_TYPE = "Warna";
  private static final String COLOUR_VALUE = "Brick Red";
  private static final String BLANK = " ";
  private static final String GUARANTEE_INFO = GUARANTEE_TYPE + BLANK + GUARANTEE_DURATION;
  private static final String ITEM_CODE = "item-code";
  private static final String PROMO_BUNDLING_ID = "promo-bundling-id";
  private static final String PROMO_BUNDLING_TYPE = "promo-bundling-type";
  private static final String PROMO_BUNDLING_TYPE_WHOLESALE = "WHOLESALE";
  private static final String PROMO_BUNDLING_NAME = "promo-bundling-name";
  private static final String PRODUCT_NAME = "product-name";
  private static final String CATEGORY_ID = "category-id";
  private static final String CATEGORY_ID1 = "category-id-1";
  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code2";
  private static final String BUSINESS_PARTNER_CODE = "bpCode";
  private static final String BUSINESS_PARTNER_TYPE = "bpType";
  private static final String LINKED_BUSINESS_PARTNER = "linkedBp";
  private static final String ALIAS_BUSINESS_PARTNER = "alias";
  private static final String INVENTORY_TYPE = "inventoryType";
  private static final String STATUS = "status";
  private static final String TYPE = "type";
  private static final String MERCHANT_DELIVERY_TYPE = "merchantDeliveryType";
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptive-attribute";
  private static final String USER_NAME = "userName";
  private static final String CAMPAIGN_CODE = "campaign-code";
  private static final String ADJUSTMENT_NAME = "adjustment-name";
  private static final String ID = "ID";
  private static int MIN_QUANTITY = 4;
  private static int MAX_QUANTITY = 6;
  private static double DISCOUNT_PERCENTAGE = 10.0;
  private static final String LOCATION_PATH1 = "location_path_1";
  private static final String LOCATION_PATH2 = "location_path_2";
  private static final String LOCATION_PATH3 = "location_path_3";
  private static final String LOCATION_PATH4 = "location_path_4";
  private static final String DOCUMENT_TYPE = "DT1, DT2";
  private static final String PREORDER_TYPE = "DAYS";
  private static final Integer PREORDER_VALUE = 10;
  private static final String IMAGE_PATH = "image.jpg";
  private static final  String ITEM_ATTRIBUTE_VALUE = "attribute-value";
  private static final double TOTAL_SCORE = 90;
  private static final double DISCOUNT_PRICE = 10;
  private static final String MASTER_CATEGORY = "10001#_#category-code";
  private static final String SALES_CATEGORY = "10001#_#catalog-code-sales-a";
  private static final AdjustmentType ADJUSTMENT_TYPE = AdjustmentType.BLIBLI;
  private static final String CODE = "code";
  private static final String EXTERNAL_PICK_UP_POINT_CODE = "externalCode";
  private static final String ADDRESS = "address";
  private static final String COUNTRY_CODE = "countryCode";
  private static final String PROVINCE_CODE = "provinceCode";
  private static final String CITY_CODE = "cityCode";
  private static final String DISTRICT_CODE = "districtCode";
  private static final String DISTRICT_NAME = "districtCode";
  private static final String SUB_DISTRICT_NAME = "SubDistrictName";
  private static final String SUB_DISTRICT_CODE = "SubDistrictCode";
  private static final String TELEPHONE = "TELEPHONE";
  private static final String B2B_SALES = "12052";
  private static final String WAREHOUSE_ID = "wareHouseId";
  private static final String ORIGIN_ID = "originId";
  private static final String CONTACT_PERSON = "contactPerson";
  private static final String PLACE_ID = "placeId";
  private static final String COVERAGE_AREA_SETTINGS = "coverageAreaSettings";
  private static final String FAX = "fax";
  private static final String ADDITIONAL_INFO = "additionalInfo";
  private static final String LOCATION_ID = "locationId";
  private static final String SALES_CATEGORY_DB = "catalog-code-sales-a";
  private static final String WAREHOUSE = "WH";
  private static final String MARKETPLACE = "MKT";
  private static final String MASTER_SKU = "masterSku";
  private static final String SIZE_CHART_CODE = "sizeChartCode";
  private static final String FETCH_VIEW_CONFIGS_BY_CHANNEL = "DEFAULT, CNC";
  private static final List<String> IMEI_ATTRIBUTE_VALUES = Arrays.asList("YES","Yes","yes");
  public static final String IMEI_ATTRIBUTE_CODE = "IM-2100000003";
  public static final String IMEI_ATTRIBUTE_VALUE = "YES";

  private static final Date PREVIOUS_DATE = new DateTime(new Date()).minusDays(1).toDate();
  private static final Date NEXT_DATE = new DateTime(new Date()).plusDays(1).toDate();
  @InjectMocks
  private ObjectConverterServiceImpl objectConverterServiceImpl;


  @Mock
  private MasterDataConstructorService masterDataConstructorService;

  @Mock
  private GdnMapper gdnMapper;

  @Mock
  private ItemSummaryUtil itemSummaryUtil;

  @Mock
  private ItemHelperService itemHelperService;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Mock
  private ProductCategoryBaseOutbound productCategoryBaseClient;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ChannelService channelService;

  @Mock
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Mock
  private ItemService itemService;


  private GdnMapper mapper = new DozerMapper(new DozerBeanMapper());

  private List<List<CategoryResponse>> listOfCategoriesList;

  private ProductSpecialAttribute productSpecialAttribute1;

  private ProductSpecialAttribute productSpecialAttribute2;

  private ProductSpecialAttribute productSpecialAttribute3;

  private ProductSpecialAttribute productSpecialAttribute4;

  private ProductSpecialAttribute productSpecialAttribute5;

  private Product product;

  private Product product1;

  private Product product2;

  private Product product3;

  private Product product4;

  private Item item;
  private ProfileResponse merchantProfile;
  private List<CategoryResponse> categoriesListMaster;
  private CategoryResponse categoryMasterC1;
  private CategoryResponse categoryMasterC2;
  private CategoryResponse categoryMasterC3;
  private List<CategoryResponse> categoriesListSalesA1;
  private CategoryResponse categorySalesA1C1;
  private CategoryResponse categorySalesA1C2;
  private List<CategoryResponse> categoriesListSalesA2;
  private CategoryResponse categorySalesA2C1;
  private CategoryResponse categorySalesA2C2;
  private List<CategoryResponse> categoriesListSalesB1;
  private CategoryResponse categorySalesB1C1;
  private CategoryResponse categorySalesB1C2;
  private List<CategoryResponse> categoriesListSalesB2;
  private CategoryResponse categorySalesB2C1;
  private CategoryResponse categorySalesB2C2;
  private CatalogResponse masterCatalog;
  private CatalogResponse salesCatalogA;
  private CatalogResponse salesCatalogB;
  private Map<String, List<List<ItemCategoryVO>>> mapOfListOfItemCategoryList;
  private MasterDataItem masterDataItem;
  private MasterDataAttribute masterDataAttribute;
  private List<MasterDataItemAttributeValue> masterDataItemAttributeValues;
  private List<ItemPickupPoint> itemPickupPoints;
  private MasterDataItemAttributeValue masterDataItemAttributeValue;
  private Price price;
  private BusinessPartnerChange businessPartnerChange;
  private CompanyVO companyVO;
  private CategoryNamesResponse categoryNamesResponse;
  private PristineDataItem pristineDataItem;
  private ItemPickupPoint itemPickupPoint;
  private ItemPickupPoint itemPickupPoint1;
  private PickupPointVOEventModel pickupPointVO;
  private PickupPointVOEventModel pickupPointChange;
  private ContactPersonVOEventModel contactPersonVO;
  private GeolocationVOEventModel geolocationVO;
  private BusinessHourVOEventModel businessHourVO;

  private CompanyDTO company;

  private ResponsiblePersonDTO responsiblePerson;

  private AttributeResponse attributeResponse;

  private DiscountPrice discountPrice;

  private List<DiscountPrice> discountPriceList;

  private List<ItemCatalogVO> itemCatalogs;

  private ItemViewConfig itemViewConfig;

  private Set<ItemViewConfig> itemViewConfigs;

  private MasterDataItemImage masterDataItemImage;

  private List<MasterDataItemImage> masterDataItemImages;

  private PromoBundlingDetailResponseVO promoBundlingDetailResponseWholesaleVO;

  private List<PromoBundlingDetailResponseVO> promoBundlingDetailResponsesWholesaleVO;

  private WholesaleVO wholesaleVO;

  private WholesaleRuleVO wholesaleRuleVO;

  private List<WholesaleRuleVO> wholesaleRuleVOList;

  private WholesaleRule wholesaleRule;

  private List<WholesaleRule> wholesaleRuleList;

  private PromoBundlingDetailResponseVO promoBundlingDetailResponseVO;

  private SimpleSetStringRequest request;

  private Set<String> itemCodes;

  private ComboVO comboVO;

  private List<ComboVO> comboVOList;

  private ComboItemVO comboItemVO;

  private List<ComboItemVO> comboItemVOList;

  private ComboRuleVO comboRule;

  private List<ComboRuleVO> comboRules;

  private MasterDataProduct masterDataProduct;

  private List<PromoBundlingDetailResponseVO> promoBundlingDetailResponses;

  private OfflineItemDetailVo offlineItemDetailVo;

  private List<OfflineItemDetailVo> offlineItemDetailVos;

  private PreOrder preOrder;

  private NeedCorrectionItemActivationRequest itemActivationRequest;

  private ProductMasterDataResponse productMasterDataResponse = new ProductMasterDataResponse();
  private ProductItemResponse productItemResponse = new ProductItemResponse();
  private Image image = new Image();
  private ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
  private ProductEventModel productEventModel;
  private ItemEventModel itemEventModel;
  private ProductAndItemSolr productAndItemSolr;
  private ProductAndItemSolr productAndItemSolr1;
  private List<ProductAndItemSolr> productAndItemSolrs;
  private Map<String, MasterDataItem> masterDataItems;
  private Map<String, Item> dbItemMap;
  private Map<String, ItemPickupPoint> itemPickupPointMap;
  private Map<String, Product> productMap;
  private ProductAttributeDomainEventModel productAttributeDomainEventModel;
  private ProductAttributeDetail productAttributeDetail;

  private ItemDiscoverableSchedule itemDiscoverableSchedule = new ItemDiscoverableSchedule();
  private ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
  private ImageResponse imageResponse;
  private List<ImageResponse> imageResponses = new ArrayList<>();
  private ItemResponse itemResponse;
  private List<ItemResponse> itemResponses;
  private PriceDTO priceDTO;
  private Set<PriceDTO> priceDTOS;
  private MasterDataProductImageDTO masterDataProductImageDTO;
  private BusinessPartnerPromo businessPartnerPromo = new BusinessPartnerPromo();

  @Test
  public void convertAndValidateMasterDataExistsPristineTest() {
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    List<Product> products = Arrays.asList(product1, product2);
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    List<Item> items = Arrays.asList(item1, item2, item3, item4);
    ProductAndItemsVO response1 = new ProductAndItemsVO(product1, Arrays.asList(item1, item2));
    ProductAndItemsVO response2 = new ProductAndItemsVO(product2, Arrays.asList(item3, item4));
    Map<String, MasterDataProduct> masterDataProductMap = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    masterDataProductMap.put(ObjectConverterServiceImplTest.PRODUCT_CODE, masterDataProduct);
    List<ProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateMasterDataExistsForPristine(products, items, masterDataProductMap,
            new HashMap<String, MasterDataItem>());
    assertTrue(responses.contains(response1));
    assertTrue(responses.contains(response2));
  }

  @Test
  public void convertAndValidateMasterDataExistsPristine_whenMasterDataProductNullTest() {
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    List<Product> products = Arrays.asList(product1, product2);
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    List<Item> items = Arrays.asList(item1, item2, item3, item4);
    ProductAndItemsVO response1 = new ProductAndItemsVO(product1, Arrays.asList(item1, item2));
    ProductAndItemsVO response2 = new ProductAndItemsVO(product2, Arrays.asList(item3, item4));
    Map<String, MasterDataProduct> masterDataProductMap = new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> masterDataItemMap = new HashMap<String, MasterDataItem>();
    List<ProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateMasterDataExistsForPristine(products, items, masterDataProductMap,
            new HashMap<String, MasterDataItem>());
    assertTrue(responses.contains(response1));
    assertTrue(responses.contains(response2));
    assertEquals(null, responses.get(0).getProduct().getMasterDataProduct());
  }

  @Test
  public void convertPristineAttributeToProductAttributeTest() {
    Map<String, String> pristineListingAttributes = new HashMap<>();
    pristineListingAttributes.put(ObjectConverterServiceImplTest.ATTRIBUTE_NAME,
        ObjectConverterServiceImplTest.ATTRIBUTE_VALUE);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineListingAttributes(pristineListingAttributes);
    pristineDataItem.setPristineId(ObjectConverterServiceImplTest.PRISTINE_ID);

    ProductAttribute expectedResult = new ProductAttribute();
    ProductAttributeDetail detail = new ProductAttributeDetail();
    detail.setAttributeCode(ObjectConverterServiceImplTest.ATTRIBUTE_NAME);
    detail.setAttributeName(ObjectConverterServiceImplTest.ATTRIBUTE_NAME);
    detail.setAttributeValue(ObjectConverterServiceImplTest.ATTRIBUTE_VALUE);
    expectedResult.setProductAttributeDetails(Stream.of(detail).collect(Collectors.toList()));
    expectedResult.setItemSku(ObjectConverterServiceImplTest.PRISTINE_ID);
    ProductAttribute result =
        objectConverterServiceImpl.convertPristineAttributeToProductAttribute(pristineDataItem);
    assertEquals(expectedResult, result);
  }

  @Test
  public void convertPromoBundlingDetailResponseToWholesaleRuleVO_success() {
    List<WholesaleRuleVO> result = this.objectConverterServiceImpl
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(promoBundlingDetailResponseWholesaleVO);

    assertEquals(wholesaleRuleVOList, result);
  }

  @Test
  public void convertPromoBundlingDetailResponseToWholesaleRuleVOWithEmptyWholeSaleRules_success() {
    promoBundlingDetailResponseWholesaleVO.setWholesaleRules(null);

    List<WholesaleRuleVO> result = this.objectConverterServiceImpl
        .convertPromoBundlingDetailResponseToWholesaleRuleVO(promoBundlingDetailResponseWholesaleVO);

    assertEquals(Collections.emptyList(), result);
  }

  @Test
  public void convertPromoBundlingDetailResponseToWholesaleVO_success() {
    PromoBundlingByItemSkuAndItemCodesResponseVO wholesaleResponse = new PromoBundlingByItemSkuAndItemCodesResponseVO();
    wholesaleResponse.setPromoBundling(promoBundlingDetailResponseVO);
    wholesaleResponse.setTotalWholesaleRule(1);

    when(itemHelperService.getDiscountPrice(item.getPrice().stream().findFirst().get()))
        .thenReturn(Double.valueOf(10));

    this.item.setItemSku(ITEM_SKU_1);
    this.item.setItemCode(ITEM_CODE_1);
    WholesaleVO result = this.objectConverterServiceImpl
        .convertPromoBundlingDetailResponseToWholesaleVO(wholesaleResponse, item);
    verify(itemHelperService).getDiscountPrice(item.getPrice().stream().findFirst().get());
    assertEquals(result.getTotal(), 1);
    assertEquals(((int) result.getProductAdjustmentPrice()), 10);
  }


  @Test
  public void convertToAddProductAndItemsVoResponseVo() {
    Item item1 = new Item();
    item1.setItemCode(ObjectConverterServiceImplTest.ITEM_CODE_1);
    item1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    Item item2 = new Item();
    item2.setItemCode(ObjectConverterServiceImplTest.ITEM_CODE_2);
    item2.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_2);
    List<Item> items = Arrays.asList(item1, item2);
    Product product = new Product();
    product.setProductCode(ObjectConverterServiceImplTest.PRODUCT_CODE);
    product.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU2);
    product.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_CODE);
    AddProductAndItemsResponseVo responseVo =
        this.objectConverterServiceImpl.convertToAddProductAndItemsResponseVo(product, items);
    assertEquals(responseVo.getMerchantCode(), ObjectConverterServiceImplTest.MERCHANT_CODE);
    assertEquals(responseVo.getProductCode(), ObjectConverterServiceImplTest.PRODUCT_CODE);
    assertEquals(responseVo.getProductSku(), ObjectConverterServiceImplTest.PRODUCT_SKU);
    assertEquals(
        responseVo.getMapOfItemSkuByItemCode().get(ObjectConverterServiceImplTest.ITEM_CODE_2),
        ObjectConverterServiceImplTest.ITEM_SKU_2);
    assertEquals(
        responseVo.getMapOfItemSkuByItemCode().get(ObjectConverterServiceImplTest.ITEM_CODE_1),
        ObjectConverterServiceImplTest.ITEM_SKU_1);
  }

  @Test
  public void convertToItemSummaryTest() {
    Item item1 = new Item();
    item1.setItemCode(ObjectConverterServiceImplTest.ITEM_CODE_1);
    item1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    item1.setSynchronized(true);
    Item item2 = new Item();
    item2.setItemCode(ObjectConverterServiceImplTest.ITEM_CODE_2);
    item2.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_2);
    item2.setSynchronized(true);
    List<Item> items = Arrays.asList(item1, item2);
    Product product = new Product();
    product.setProductCode(ObjectConverterServiceImplTest.PRODUCT_CODE);
    product.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU2);
    product.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_CODE);
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setBrand(BRAND);
    product.setMasterDataProduct(masterDataProduct);

    objectConverterServiceImpl.convertToItemSummary(new ProductAndItemsVO(product, items));
  }

  @Test
  public void convertToListOfItemCatalogTestSuccess() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setName(ObjectConverterServiceImplTest.CATEGORY_NAME);
    categoryResponse.setDisplay(true);
    categoryResponse.setActivated(true);
    categoryResponse.setCatalog(
        new CatalogResponse("master", ObjectConverterServiceImplTest.CATALOG_CODE, "master"));
    categoryResponse.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE);
    List<ItemCatalogVO> result = this.objectConverterServiceImpl
        .convertToListOfItemCatalog(Arrays.asList(Arrays.asList(categoryResponse)));
    assertEquals(result.get(0).getCatalogId(), ObjectConverterServiceImplTest.CATALOG_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategoryId(),
        ObjectConverterServiceImplTest.CATEGORY_CODE);
    assertTrue(result.get(0).getItemCategories().get(0).isDisplay());
    assertTrue(result.get(0).getItemCategories().get(0).isCategoryActive());
    assertEquals(result.get(0).getItemCategories().get(0).getCategory(),
        ObjectConverterServiceImplTest.CATEGORY_NAME);
    assertEquals(result.get(0).getItemCategories().get(0).getLevel(), 1);
  }

  @Test
  public void convertToListOfItemCatalogTestWithNullParams() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl.convertToListOfItemCatalog(null));
  }

  @Test
  public void convertToListOfItemCatalogV2TestSuccess() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setName(ObjectConverterServiceImplTest.CATEGORY_NAME);
    categoryResponse.setActivated(true);
    categoryResponse.setDisplay(false);
    categoryResponse.setCatalog(
        new CatalogResponse("master", ObjectConverterServiceImplTest.CATALOG_CODE, "master"));
    categoryResponse.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE);
    List<ItemCatalogVOV2> result = this.objectConverterServiceImpl
        .convertToListOfItemCatalogV2(Arrays.asList(Arrays.asList(categoryResponse)));
    assertEquals(result.get(0).getCatalogId(), ObjectConverterServiceImplTest.CATALOG_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategoryId(),
        ObjectConverterServiceImplTest.CATEGORY_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategory(),
        ObjectConverterServiceImplTest.CATEGORY_NAME);
    assertEquals(result.get(0).getItemCategories().get(0).getLevel(), 1);
    assertTrue(result.get(0).getItemCategories().get(0).isCategoryActive());
    assertFalse(result.get(0).getItemCategories().get(0).isDisplay());
  }

  @Test
  public void convertToListOfItemCatalogV2TestSuccess2() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setName(ObjectConverterServiceImplTest.CATEGORY_NAME);
    categoryResponse.setActivated(true);
    categoryResponse.setDisplay(true);
    categoryResponse.setCatalog(
        new CatalogResponse("master", ObjectConverterServiceImplTest.CATALOG_CODE, "master"));
    categoryResponse.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE);
    List<ItemCatalogVOV2> result = this.objectConverterServiceImpl
        .convertToListOfItemCatalogV2(Arrays.asList(Arrays.asList(categoryResponse)));
    assertEquals(result.get(0).getCatalogId(), ObjectConverterServiceImplTest.CATALOG_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategoryId(),
        ObjectConverterServiceImplTest.CATEGORY_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategory(),
        ObjectConverterServiceImplTest.CATEGORY_NAME);
    assertEquals(result.get(0).getItemCategories().get(0).getLevel(), 1);
    assertTrue(result.get(0).getItemCategories().get(0).isCategoryActive());
    assertTrue(result.get(0).getItemCategories().get(0).isDisplay());
  }

  @Test
  public void convertToListOfItemCatalogV2TestSuccess3() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setName(ObjectConverterServiceImplTest.CATEGORY_NAME);
    categoryResponse.setActivated(false);
    categoryResponse.setDisplay(true);
    categoryResponse.setCatalog(
        new CatalogResponse("master", ObjectConverterServiceImplTest.CATALOG_CODE, "master"));
    categoryResponse.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE);
    List<ItemCatalogVOV2> result = this.objectConverterServiceImpl
        .convertToListOfItemCatalogV2(Arrays.asList(Arrays.asList(categoryResponse)));
    assertEquals(result.get(0).getCatalogId(), ObjectConverterServiceImplTest.CATALOG_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategoryId(),
        ObjectConverterServiceImplTest.CATEGORY_CODE);
    assertEquals(result.get(0).getItemCategories().get(0).getCategory(),
        ObjectConverterServiceImplTest.CATEGORY_NAME);
    assertEquals(result.get(0).getItemCategories().get(0).getLevel(), 1);
    assertFalse(result.get(0).getItemCategories().get(0).isCategoryActive());
    assertTrue(result.get(0).getItemCategories().get(0).isDisplay());
  }

  @Test
  public void convertToListOfItemCatalogV2_WhenCategoryListEmpty() {
    List<ItemCatalogVOV2> result = this.objectConverterServiceImpl
        .convertToListOfItemCatalogV2(Arrays.asList(Collections.EMPTY_LIST));
    assertEquals(result.size(), 0);
  }

  @Test
  public void convertToListOfItemCatalogV2TestWithNullParams() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.objectConverterServiceImpl.convertToListOfItemCatalogV2(null));
  }

  @Test
  public void convertToListOfProductAndItemsResponseVoTest() {
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    List<Product> products = Arrays.asList(product1, product2);

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setDangerousLevel(LEVEL);

    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setMasterDataItem(null);
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item2.setMasterDataItem(masterDataItem);
    item2.setItemCode(ITEM_CODE_1);
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item3.setMasterDataItem(masterDataItem);
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    List<Item> items = Arrays.asList(item1, item2, item3, item4);
    ProductAndItemsVO response1 = new ProductAndItemsVO(product1, Arrays.asList(item1, item2));
    ProductAndItemsVO response2 = new ProductAndItemsVO(product2, Arrays.asList(item3, item4));

    Map<String, MasterDataItem> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE_1, masterDataItem);

    List<ProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateMasterDataExists(products, items,
            new HashMap<String, MasterDataProduct>(), masterDataItemMap);
    assertTrue(responses.contains(response1));
    assertTrue(responses.contains(response2));
  }

  private Map<String, SimpleMasterDataItemVO> createMasterDataItemAndResponses(
      List<Product> products, List<Item> items) {
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1, PRODUCT_CODE);
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2, PRODUCT_CODE);
    product1.setMasterDataProduct(new MasterDataProduct());
    product1.setB2bActivated(true);
    product1.setB2cActivated(true);
    product2.setB2bActivated(true);
    product2.setB2cActivated(true);
    MasterDataProductImage masterDataProductImage1 = new MasterDataProductImage();
    masterDataProductImage1.setLocationPath(LOCATION_PATH1);
    MasterDataProductImage masterDataProductImage2 = new MasterDataProductImage();
    masterDataProductImage2.setLocationPath(LOCATION_PATH2);
    product1.getMasterDataProduct()
        .setMasterDataProductImages(Arrays.asList(masterDataProductImage1, masterDataProductImage2));
    products.add(product1);
    products.add(product2);
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setMasterDataItem(null);
    Set<String> set = new HashSet<>();
    set.addAll(Arrays.asList(new String[] {MARKETPLACE, WAREHOUSE}));
    item1.setPreferredSubscriptionType(set);
    item1.setWeight(ITEM_WEIGHT);
    item1.setHeight(ITEM_HEIGHT);
    item1.setWidth(ITEM_WIDTH);
    item1.setLength(ITEM_LENGTH);
    item1.setShippingWeight(ITEM_WEIGHT);
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item2.setMasterDataItem(masterDataItem);
    item2.setCategoryCode(CATEGORY_CODE);
    MasterDataItemImage masterDataItemImage1 = new MasterDataItemImage();
    masterDataItemImage1.setLocationPath(LOCATION_PATH3);
    MasterDataItemImage masterDataItemImage2 = new MasterDataItemImage();
    masterDataItemImage2.setLocationPath(LOCATION_PATH4);
    masterDataItem.setMasterDataItemImages(Arrays.asList(masterDataItemImage1, masterDataItemImage2));
    item2.setItemCode(ITEM_CODE_1);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineMasterId(PRISTINE_ID);
    item2.setSynchronized(false);
    item2.setPristineDataItem(pristineDataItem);
    item2.setPreferredSubscriptionType(set);
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item3.setMasterDataItem(masterDataItem);
    item3.setPreferredSubscriptionType(set);
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    items.add(item1);
    items.add(item2);
    items.add(item3);
    items.add(item4);
    SimpleItemVO simpleItemVO1 = new SimpleItemVO(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    simpleItemVO1.setSimpleAsyncMasterDataItem(null);
    SimpleItemVO simpleItemVO2 = new SimpleItemVO(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    simpleItemVO2.setSimpleAsyncMasterDataItem(
        SimpleAsyncMasterDataItemVO.toSimpleAsyncMasterDataItemVO(item1));
    simpleItemVO1.setItemCode(ITEM_CODE_1);
    SimpleItemVO simpleItemVO3 = new SimpleItemVO(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    simpleItemVO3.setSimpleAsyncMasterDataItem(
        SimpleAsyncMasterDataItemVO.toSimpleAsyncMasterDataItemVO(item3));
    SimpleMasterDataItemVO simpleMasterDataItemVO = new SimpleMasterDataItemVO();
    simpleMasterDataItemVO.setMasterDataItemAttributeValues(
        masterDataItem.getMasterDataItemAttributeValues());
    simpleMasterDataItemVO.setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE_1, simpleMasterDataItemVO);
    return masterDataItemMap;
  }

  private Map<String, SimpleMasterDataItemVO> createMasterDataItemAndResponses1(
      List<Product> products, List<Item> items) {
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1, PRODUCT_CODE);
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2, PRODUCT_CODE);
    product1.setMasterDataProduct(new MasterDataProduct());
    MasterDataProductImage masterDataProductImage1 = new MasterDataProductImage();
    masterDataProductImage1.setLocationPath(LOCATION_PATH1);
    MasterDataProductImage masterDataProductImage2 = new MasterDataProductImage();
    masterDataProductImage2.setLocationPath(LOCATION_PATH2);
    product1.getMasterDataProduct()
        .setMasterDataProductImages(Arrays.asList(masterDataProductImage1, masterDataProductImage2));
    products.add(product1);
    products.add(product2);
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setMasterDataItem(null);
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item2.setMasterDataItem(masterDataItem);
    MasterDataItemImage masterDataItemImage1 = new MasterDataItemImage();
    masterDataItemImage1.setLocationPath(LOCATION_PATH3);
    MasterDataItemImage masterDataItemImage2 = new MasterDataItemImage();
    masterDataItemImage2.setLocationPath(LOCATION_PATH4);
    masterDataItem.setMasterDataItemImages(Arrays.asList(masterDataItemImage1, masterDataItemImage2));
    item2.setItemCode(ITEM_CODE_1);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setPristineBrand(BRAND);
    item2.setSynchronized(false);
    item2.setPristineDataItem(pristineDataItem);
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item3.setMasterDataItem(masterDataItem);
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    items.add(item1);
    items.add(item2);
    items.add(item3);
    items.add(item4);
    SimpleItemVO simpleItemVO1 = new SimpleItemVO(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    simpleItemVO1.setSimpleAsyncMasterDataItem(null);
    SimpleItemVO simpleItemVO2 = new SimpleItemVO(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    simpleItemVO2.setSimpleAsyncMasterDataItem(
        SimpleAsyncMasterDataItemVO.toSimpleAsyncMasterDataItemVO(item1));
    simpleItemVO1.setItemCode(ITEM_CODE_1);
    SimpleItemVO simpleItemVO3 = new SimpleItemVO(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    simpleItemVO3.setSimpleAsyncMasterDataItem(
        SimpleAsyncMasterDataItemVO.toSimpleAsyncMasterDataItemVO(item3));
    SimpleMasterDataItemVO simpleMasterDataItemVO = new SimpleMasterDataItemVO();
    simpleMasterDataItemVO.setMasterDataItemAttributeValues(
        masterDataItem.getMasterDataItemAttributeValues());
    simpleMasterDataItemVO.setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = new HashMap<>();
    masterDataItemMap.put(ITEM_CODE_1, simpleMasterDataItemVO);
    return masterDataItemMap;
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.isNull())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertTrue(responses.get(1).getSimpleProduct().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(Constants.DAYS, responses.get(1).getSimpleProduct().getPreOrder().getPreOrderType());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
    Mockito.verify(productCategoryBaseClient).getCategoryNames(Mockito.anyList());
    Mockito.verify(productAndItemSolrRepository).findOne(Mockito.anyString(), Mockito.isNull());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsWithPreOrderDateGreaterThanCurrentDateTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(Constants.DATE);
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.isNull())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertTrue(responses.get(1).getSimpleProduct().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(Constants.DATE, responses.get(1).getSimpleProduct().getPreOrder().getPreOrderType());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
    Mockito.verify(productCategoryBaseClient).getCategoryNames(Mockito.anyList());
    Mockito.verify(productAndItemSolrRepository).findOne(Mockito.anyString(), Mockito.isNull());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsProductScoreTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    products.get(0).setProductScore(new ProductScore());
    products.get(0).getProductScore().setTotalScore(90);
    preOrder.setPreOrderType(Constants.WEEK);
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.isNull())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    Assertions.assertEquals(
        responses.get(1).getSimpleProduct().getProductScoreTotal(), 90, 0.0);

    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
    Mockito.verify(productCategoryBaseClient).getCategoryNames(Mockito.anyList());
    Mockito.verify(productAndItemSolrRepository).findOne(Mockito.anyString(), Mockito.isNull());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsNonPristineUnSyncTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap =
        createMasterDataItemAndResponses(products, items);
    items.get(1).setPristineDataItem(null);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items,
            new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNotNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
  }

  @Test
  public void convertToListOfProductAndItemsResponseVoWhenItemIsNull() {
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    List<Product> products = Arrays.asList(product1, product2);

    List<ProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateMasterDataExists(products, new ArrayList<>(),
            new HashMap<String, MasterDataProduct>(), new HashMap<String, MasterDataItem>());

    assertNotNull(responses);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExists_WhenMasterDataProductNullTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap =
        createMasterDataItemAndResponses(products, items);
    products.get(0).setSynchronized(true);
    products.get(1).setSynchronized(true);
    Map<String, SimpleMasterDataProductVO> masterDatProduct = new HashMap<String, SimpleMasterDataProductVO>();
    masterDatProduct.put(PRODUCT_CODE, null);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items,
            new HashMap<String, SimpleMasterDataProductVO>(), masterDataItemMap);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void convertToListOfProductAndItemsResponseVo_whenMasterDataProductNullTest() {
    ReflectionTestUtils.setField(objectConverterServiceImpl, "isProductVisibilityEnabled", true);
    Map<String, MasterDataProduct> pcMasterDataProductMap =
        new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> itemCodeMasterDataItemMap = new HashMap<String, MasterDataItem>();
    pcMasterDataProductMap
        .put(ObjectConverterServiceImplTest.PRODUCT_CODE, new MasterDataProduct());
    itemCodeMasterDataItemMap.put("ITEM_CODE_1", new MasterDataItem());
    itemCodeMasterDataItemMap.put("ITEM_CODE_4", new MasterDataItem());
    itemCodeMasterDataItemMap.put("ITEM_CODE_2", new MasterDataItem());
    itemCodeMasterDataItemMap.put("ITEM_CODE_3", new MasterDataItem());
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    product1.setSynchronized(true);
    product1.setMasterDataProduct(new MasterDataProduct());
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2, "product-code2");
    product2.setSynchronized(true);
    product2.setMasterDataProduct(new MasterDataProduct());
    List<Product> products = Arrays.asList(product1, product2);
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setMasterDataItem(new MasterDataItem());
    item1.setItemCode("ITEM_CODE_1");
    item1.setMasterDataItem(new MasterDataItem());
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item2.setItemCode("ITEM_CODE_2");
    item2.setMasterDataItem(new MasterDataItem());
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item3.setItemCode("ITEM_CODE_3");
    item3.setMasterDataItem(new MasterDataItem());
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item4.setItemCode("ITEM_CODE_4");
    item4.setMasterDataItem(new MasterDataItem());
    List<Item> items = Arrays.asList(item1, item2, item3, item4);
    ProductAndItemsVO response1 = new ProductAndItemsVO(product1, Arrays.asList(item1, item2));
    ProductAndItemsVO response2 = new ProductAndItemsVO(product2, Arrays.asList(item3, item4));

    List<ProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateMasterDataExists(products, items,
            new HashMap<String, MasterDataProduct>(), itemCodeMasterDataItemMap);
  }

  @Test
  public void convertToListOfProductAndItemsResponseVo_whenProductSyncTest() {
    ReflectionTestUtils.setField(objectConverterServiceImpl, "isProductVisibilityEnabled", true);
    Map<String, MasterDataProduct> pcMasterDataProductMap =
        new HashMap<String, MasterDataProduct>();
    Map<String, MasterDataItem> itemCodeMasterDataItemMap = new HashMap<String, MasterDataItem>();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    pcMasterDataProductMap
        .put(ObjectConverterServiceImplTest.PRODUCT_CODE, masterDataProduct);
    itemCodeMasterDataItemMap.put("ITEM_CODE_1", new MasterDataItem());
    itemCodeMasterDataItemMap.put("ITEM_CODE_4", new MasterDataItem());
    itemCodeMasterDataItemMap.put("ITEM_CODE_2", new MasterDataItem());
    itemCodeMasterDataItemMap.put("ITEM_CODE_3", new MasterDataItem());
    Product product1 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    product1.setSynchronized(true);
    product1.setMasterDataProduct(new MasterDataProduct());
    Product product2 = new Product(ObjectConverterServiceImplTest.PRODUCT_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_CODE);
    product2.setMasterDataProduct(new MasterDataProduct());
    List<Product> products = Arrays.asList(product1, product2);
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setMasterDataItem(new MasterDataItem());
    item1.setItemCode("ITEM_CODE_1");
    item1.setMasterDataItem(new MasterDataItem());
    item1.setPristineDataItem(new PristineDataItem());
    Item item2 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_2,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item2.setItemCode("ITEM_CODE_2");
    item2.setMasterDataItem(new MasterDataItem());
    item2.setPristineDataItem(new PristineDataItem());
    Item item3 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_3,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item3.setItemCode("ITEM_CODE_3");
    item3.setMasterDataItem(new MasterDataItem());
    item3.setPristineDataItem(new PristineDataItem());
    Item item4 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_4,
        ObjectConverterServiceImplTest.PRODUCT_SKU_2);
    item4.setItemCode("ITEM_CODE_4");
    item4.setMasterDataItem(new MasterDataItem());
    item4.setPristineDataItem(new PristineDataItem());
    List<Item> items = Arrays.asList(item1, item2, item3, item4);
    ProductAndItemsVO response1 = new ProductAndItemsVO(product1, Arrays.asList(item1, item2));
    ProductAndItemsVO response2 = new ProductAndItemsVO(product2, Arrays.asList(item3, item4));

    List<ProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateMasterDataExists(products, items, pcMasterDataProductMap,
            itemCodeMasterDataItemMap);
    assertTrue(responses.contains(response1));
    assertTrue(responses.contains(response2));
  }

  @Test
  public void convertToMapOfListOfItemCategoryListTestWithNullListOfCategoriesList() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.objectConverterServiceImpl.convertToListOfItemCatalog(null));
  }

  @Test
  public void convertToMasterDataAttributeTest() {
    attributeResponse.setId(ID);
    MasterDataAttribute result =
        this.objectConverterServiceImpl.convertToMasterDataAttribute(this.attributeResponse);

    assertNotNull(result);
    assertEquals(result.getAttributeCode(), this.attributeResponse.getAttributeCode());
    assertEquals(result.getAttributeName(), this.attributeResponse.getName());
    assertEquals(result.getAttributeType(),
        MasterDataAttributeType.valueOf(this.attributeResponse.getAttributeType()));
    assertEquals(result.getDescription(),
        new String(this.attributeResponse.getDescription()));
    assertEquals(result.isSearchable(), this.attributeResponse.isSearchAble());
    assertEquals(result.isSkuValue(), this.attributeResponse.isSkuValue());
    assertEquals(result.getStoreId(), this.attributeResponse.getStoreId());
    assertTrue(result.isVariantCreation());
    assertTrue(result.isBasicView());
    assertTrue(result.isMandatory());
    assertEquals(ID, result.getId());
  }

  @Test
  public void convertToPriceHistoryTesWithNullPricet() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.objectConverterServiceImpl
        .convertToPriceHistory(null, ObjectConverterServiceImplTest.ITEM_SKU));
  }

  @Test
  public void convertToPriceHistoryTest() {
    PriceHistory result = this.objectConverterServiceImpl
        .convertToPriceHistory(this.price, ObjectConverterServiceImplTest.ITEM_SKU);

    assertEquals(result.getChannel(), this.price.getChannel());
    assertEquals(result.getItemSku(), ObjectConverterServiceImplTest.ITEM_SKU);
    assertEquals(result.getOfferPrice(), this.price.getOfferPrice());
  }

  @Test
  public void convertToPriceHistoryTestWithBlankItemSku() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.objectConverterServiceImpl.convertToPriceHistory(this.price, null));
  }

  @Test
  public void convertToProductForTransactionTest() throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO productForTransactionVO =
        this.objectConverterServiceImpl.convertToProductForTransaction(this.product, this.item, this.itemCatalogs);
    Assertions.assertNotNull(productForTransactionVO);
  }

  @Test
  public void convertToProductForTransactionWithPreOrderTest() throws Exception {
    product.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(), this.product.getMasterDataProduct()))
        .thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransaction(this.product, this.item, this.itemCatalogs);
    assertEquals(PREORDER_VALUE, result.getItemDetail().getPreOrder().getPreOrderValue());
  }

  @Test
  public void convertToProductForTransactionTestWithNullItem() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product, null, this.itemCatalogs));
  }

  @Test
  public void convertToProductForTransactionTestWithNullItemCatalogs() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl.convertToProductForTransaction(this.product, this.item, null));
  }

  @Test
  public void convertToProductForTransactionTestWithNullProduct() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl
        .convertToProductForTransaction(null, this.item, this.itemCatalogs));
  }

  @Test
  public void
  convertToProductForTransactionTest_product_getProductSpecialAttributes_containsGuaranteeAND_notContainsLamaGaransi()
      throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product3.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product3, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_TYPE, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void
  convertToProductForTransactionTest_product_getProductSpecialAttributes_containsLamaGaransiAND_notContainsGuarantee()
      throws Exception {

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product4.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product4, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTest_product_getProductSpecialAttributes_containsNull()
      throws Exception {
    this.product1.getProductSpecialAttributes().clear();
    this.product1.getProductSpecialAttributes().add(null);

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void
  convertToProductForTransactionTest_product_getProductSpecialAttributes_getAttributeName_isNull()
      throws Exception {
    productSpecialAttribute1.setAttributeName(null);

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTest_product_getProductSpecialAttributes_isEmpty()
      throws Exception {
    this.product1.getProductSpecialAttributes().clear();
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTest_product_getProductSpecialAttributes_isNull()
      throws Exception {
    this.product1.setProductSpecialAttributes(null);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void
  convertToProductForTransactionTest_product_getProductSpecialAttributes_notContainsGaransiAndGuaranteeAndLamaGaransi()
      throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product2.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product2, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTest_product_getProductSpecialAttributes_notIsEmpty()
      throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTest_preorderTest()
      throws Exception {
    product1.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
    Assertions.assertTrue(result.getItemDetail().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductForTransactionTest_preorderFalseTest()
      throws Exception {
    preOrder.setIsPreOrder(false);
    product1.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
    Assertions.assertFalse(result.getItemDetail().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductForTransactionTest_preorderTrueExpiredDateTest()
      throws Exception {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(Constants.DATE);
    product1.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransaction(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
    Assertions.assertFalse(result.getItemDetail().getPreOrder().getIsPreOrder());
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);

    ItemCategoryVO itemCategoryVO = new ItemCategoryVO();
    itemCategoryVO.setCategory(ObjectConverterServiceImplTest.CATEGORY_NAME_MASTER_C1);
    itemCategoryVO.setCategoryId(ObjectConverterServiceImplTest.CATEGORY_CODE_MASTER_C1);
    itemCategoryVO.setLevel(ObjectConverterServiceImplTest.LEVEL);
    itemCategoryVO.setProductCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_MASTER_C1);

    this.itemCatalogs = Arrays.asList(new ItemCatalogVO(ObjectConverterServiceImplTest.CATALOG_ID,
        Arrays.asList(itemCategoryVO)));

    // master catalog
    this.masterCatalog = new CatalogResponse();
    this.masterCatalog.setCatalogCode(ObjectConverterServiceImplTest.CATALOG_CODE_MASTER);

    this.categoryMasterC1 = new CategoryResponse();
    this.categoryMasterC1.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_MASTER_C1);
    this.categoryMasterC1.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_MASTER_C1);

    this.categoryMasterC2 = new CategoryResponse();
    this.categoryMasterC2.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_MASTER_C2);
    this.categoryMasterC2.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_MASTER_C2);

    this.categoryMasterC3 = new CategoryResponse();
    this.categoryMasterC3.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_MASTER_C3);
    this.categoryMasterC3.setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_MASTER_C3);
    this.categoryMasterC3.setCatalog(this.masterCatalog);

    this.categoriesListMaster = new ArrayList<CategoryResponse>();
    this.categoriesListMaster.add(this.categoryMasterC3);
    this.categoriesListMaster.add(this.categoryMasterC2);
    this.categoriesListMaster.add(this.categoryMasterC1);

    // sales catalog A category 1
    this.salesCatalogA = new CatalogResponse();
    this.salesCatalogA.setCatalogCode(ObjectConverterServiceImplTest.CATALOG_CODE_SALES_A);

    this.categorySalesA1C1 = new CategoryResponse();
    this.categorySalesA1C1.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_A1_C1);
    this.categorySalesA1C1
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_A1_C1);

    this.categorySalesA1C2 = new CategoryResponse();
    this.categorySalesA1C2.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_A1_C2);
    this.categorySalesA1C2
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_A1_C2);
    this.categorySalesA1C2.setCatalog(this.salesCatalogA);

    this.categoriesListSalesA1 = new ArrayList<CategoryResponse>();
    this.categoriesListSalesA1.add(this.categorySalesA1C2);
    this.categoriesListSalesA1.add(this.categorySalesA1C1);

    // sales catalog A category 2
    this.categorySalesA2C1 = new CategoryResponse();
    this.categorySalesA2C1.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_A2_C1);
    this.categorySalesA2C1
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_A2_C1);

    this.categorySalesA2C2 = new CategoryResponse();
    this.categorySalesA2C2.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_A2_C2);
    this.categorySalesA2C2
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_A2_C2);
    this.categorySalesA2C2.setCatalog(this.salesCatalogA);

    this.categoriesListSalesA2 = new ArrayList<CategoryResponse>();
    this.categoriesListSalesA2.add(this.categorySalesA2C2);
    this.categoriesListSalesA2.add(this.categorySalesA2C1);

    // sales catalog B category 1
    this.salesCatalogB = new CatalogResponse();
    this.salesCatalogB.setCatalogCode(ObjectConverterServiceImplTest.CATALOG_CODE_SALES_B);

    this.categorySalesB1C1 = new CategoryResponse();
    this.categorySalesB1C1.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_B1_C1);
    this.categorySalesB1C1
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_B1_C1);

    this.categorySalesB1C2 = new CategoryResponse();
    this.categorySalesB1C2.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_B1_C2);
    this.categorySalesB1C2
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_B1_C2);
    this.categorySalesB1C2.setCatalog(this.salesCatalogB);

    this.categoriesListSalesB1 = new ArrayList<CategoryResponse>();
    this.categoriesListSalesB1.add(this.categorySalesB1C2);
    this.categoriesListSalesB1.add(this.categorySalesB1C1);

    // sales catalog B category 2
    this.categorySalesB2C1 = new CategoryResponse();
    this.categorySalesB2C1.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_B2_C1);
    this.categorySalesB2C1
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_B2_C1);

    this.categorySalesB2C2 = new CategoryResponse();
    this.categorySalesB2C2.setName(ObjectConverterServiceImplTest.CATEGORY_NAME_SALES_B2_C2);
    this.categorySalesB2C2
        .setCategoryCode(ObjectConverterServiceImplTest.CATEGORY_CODE_SALES_B2_C2);
    this.categorySalesB2C2.setCatalog(this.salesCatalogB);

    this.categoriesListSalesB2 = new ArrayList<CategoryResponse>();
    this.categoriesListSalesB2.add(this.categorySalesB2C2);
    this.categoriesListSalesB2.add(this.categorySalesB2C1);

    this.listOfCategoriesList = new ArrayList<List<CategoryResponse>>();
    this.listOfCategoriesList.add(this.categoriesListMaster);
    this.listOfCategoriesList.add(this.categoriesListSalesA1);
    this.listOfCategoriesList.add(this.categoriesListSalesA2);
    this.listOfCategoriesList.add(this.categoriesListSalesB1);
    this.listOfCategoriesList.add(this.categoriesListSalesB2);

    // end of categories

    this.productSpecialAttribute1 = new ProductSpecialAttribute();
    this.productSpecialAttribute1.setAttributeName(GARANSI);
    this.productSpecialAttribute1.setAttributeValue(GUARANTEE_TYPE);

    this.productSpecialAttribute2 = new ProductSpecialAttribute();
    this.productSpecialAttribute2.setAttributeName(LAMA_GARANSI);
    this.productSpecialAttribute2.setAttributeValue(GUARANTEE_DURATION);

    this.productSpecialAttribute3 = new ProductSpecialAttribute();
    this.productSpecialAttribute3.setAttributeName(COLOUR_TYPE);
    this.productSpecialAttribute3.setAttributeValue(COLOUR_VALUE);

    this.productSpecialAttribute4 = new ProductSpecialAttribute();
    this.productSpecialAttribute4.setAttributeName(GUARANTEE);
    this.productSpecialAttribute4.setAttributeValue(GUARANTEE_TYPE);

    this.productSpecialAttribute5 = new ProductSpecialAttribute();
    this.productSpecialAttribute5.setAttributeName(LAMA_GARANSI);
    this.productSpecialAttribute5.setAttributeValue(GUARANTEE_DURATION);

    List<ProductSpecialAttribute> productSpecialAttributes =
        new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes.add(productSpecialAttribute1);
    productSpecialAttributes.add(productSpecialAttribute2);

    List<ProductSpecialAttribute> productSpecialAttributes1 =
        new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes1.add(productSpecialAttribute3);

    List<ProductSpecialAttribute> productSpecialAttributes2 =
        new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes2.add(productSpecialAttribute4);

    List<ProductSpecialAttribute> productSpecialAttributes3 =
        new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes3.add(productSpecialAttribute5);

    this.masterDataProduct = new MasterDataProduct();
    this.masterDataProduct.setBrand(ObjectConverterServiceImplTest.BRAND);

    this.product = new Product();
    this.product.setSynchronized(true);
    this.product.setStoreId(STORE_ID);
    this.product.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_ID);
    this.product.setProductType(ObjectConverterServiceImplTest.PRODUCT_TYPE);
    this.product.setProductCatentryId(ObjectConverterServiceImplTest.PRODUCT_CATENTRY_ID);
    this.product.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU);
    this.product.setMasterDataProduct(new MasterDataProduct());
    this.product.setMasterDataProduct(masterDataProduct);
    this.product.setProductCode(ObjectConverterServiceImplTest.PRODUCT_CODE);
    this.product.setDistributionStatus(DistributionStatus.NON_DISTRIBUTION);

    this.product1 = new Product();
    this.product1.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_ID);
    this.product1.setProductType(ObjectConverterServiceImplTest.PRODUCT_TYPE);
    this.product1.setProductCatentryId(ObjectConverterServiceImplTest.PRODUCT_CATENTRY_ID);
    this.product1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU);
    this.product1.setMasterDataProduct(new MasterDataProduct());
    this.product1.getMasterDataProduct().setBrand(ObjectConverterServiceImplTest.BRAND);
    this.product1.setProductSpecialAttributes(productSpecialAttributes);

    this.product2 = new Product();
    this.product2.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_ID);
    this.product2.setProductType(ObjectConverterServiceImplTest.PRODUCT_TYPE);
    this.product2.setProductCatentryId(ObjectConverterServiceImplTest.PRODUCT_CATENTRY_ID);
    this.product2.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU);
    this.product2.setMasterDataProduct(new MasterDataProduct());
    this.product2.getMasterDataProduct().setBrand(ObjectConverterServiceImplTest.BRAND);
    this.product2.setProductSpecialAttributes(productSpecialAttributes1);

    this.product3 = new Product();
    this.product3.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_ID);
    this.product3.setProductType(ObjectConverterServiceImplTest.PRODUCT_TYPE);
    this.product3.setProductCatentryId(ObjectConverterServiceImplTest.PRODUCT_CATENTRY_ID);
    this.product3.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU);
    this.product3.setMasterDataProduct(new MasterDataProduct());
    this.product3.getMasterDataProduct().setBrand(ObjectConverterServiceImplTest.BRAND);
    this.product3.setProductSpecialAttributes(productSpecialAttributes2);

    this.product4 = new Product();
    this.product4.setMerchantCode(ObjectConverterServiceImplTest.MERCHANT_ID);
    this.product4.setProductType(ObjectConverterServiceImplTest.PRODUCT_TYPE);
    this.product4.setProductCatentryId(ObjectConverterServiceImplTest.PRODUCT_CATENTRY_ID);
    this.product4.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU);
    this.product4.setMasterDataProduct(new MasterDataProduct());
    this.product4.getMasterDataProduct().setBrand(ObjectConverterServiceImplTest.BRAND);
    this.product4.setProductSpecialAttributes(productSpecialAttributes3);

    this.discountPrice = new DiscountPrice();
    this.discountPrice.setStartDateTime(ObjectConverterServiceImplTest.START_DATE_TIME);
    this.discountPrice.setEndDateTime(ObjectConverterServiceImplTest.END_DATE_TIME);
    this.discountPrice.setDiscountPrice(ObjectConverterServiceImplTest.SALE_PRICE);

    this.discountPriceList = new ArrayList<DiscountPrice>();
    this.discountPriceList.add(this.discountPrice);

    this.price = new Price();
    this.price.setOfferPrice(ObjectConverterServiceImplTest.OFFER_PRICE);
    this.price.setChannel(ObjectConverterServiceImplTest.CHANNEL);
    this.price.setCurrency(ObjectConverterServiceImplTest.IDR);
    this.price.setListOfDiscountPrices(this.discountPriceList);

    this.masterDataItemImage = new MasterDataItemImage();
    this.masterDataItemImage.setMainImage(true);
    this.masterDataItemImage.setLocationPath(IMAGE_PATH);

    this.masterDataItemImages = new ArrayList<>();
    this.masterDataItemImages.add(this.masterDataItemImage);

    this.masterDataAttribute = new MasterDataAttribute();
    this.masterDataAttribute.setAttributeName("attribute-name");
    this.masterDataAttribute.setAttributeCode("attribute-code");
    this.masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);

    this.masterDataItemAttributeValue = new MasterDataItemAttributeValue();
    this.masterDataItemAttributeValue.setAttributeValue(ITEM_ATTRIBUTE_VALUE);
    this.masterDataItemAttributeValue.setMasterDataAttribute(masterDataAttribute);

    this.masterDataItemAttributeValues = new ArrayList<>();
    this.masterDataItemAttributeValues.add(masterDataItemAttributeValue);

    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setGeneratedItemName(ObjectConverterServiceImplTest.GENERATED_ITEM_NAME);
    this.masterDataItem.setItemLength(ObjectConverterServiceImplTest.ITEM_LENGTH);
    this.masterDataItem.setItemWidth(ObjectConverterServiceImplTest.ITEM_WIDTH);
    this.masterDataItem.setItemHeight(ObjectConverterServiceImplTest.ITEM_HEIGHT);
    this.masterDataItem.setItemWeight(ITEM_WEIGHT);
    this.masterDataItem.setItemDeliveryWeight(DELIVERY_WEIGHT);
    this.masterDataItem.setMasterDataItemImages(this.masterDataItemImages);
    this.masterDataItem.setMasterDataItemAttributeValues(masterDataItemAttributeValues);

    this.itemViewConfig = new ItemViewConfig();
    this.itemViewConfig.setChannel(Constants.DEFAULT);
    this.itemViewConfig.setBuyable(true);

    ItemViewConfig itemViewConfigB2B = new ItemViewConfig();
    itemViewConfigB2B.setChannel(Constants.B2B);
    itemViewConfigB2B.setBuyable(true);

    this.itemViewConfigs = new HashSet<>();
    this.itemViewConfigs.add(itemViewConfig);
    this.itemViewConfigs.add(itemViewConfigB2B);

    pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);

    this.item = new Item();
    this.item.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    this.item.setMerchantSku(ObjectConverterServiceImplTest.MERCHANT_SKU);
    this.item.setItemCatentryId(ObjectConverterServiceImplTest.CATENTRY_ID);
    this.item.getPrice().add(this.price);
    this.item.setMasterDataItem(this.masterDataItem);
    this.item.setItemViewConfigs(itemViewConfigs);
    this.item.setHeight(1);
    this.item.setWidth(2);
    this.item.setLength(3);
    this.item.setShippingWeight(4);
    this.item.setWeight(5);
    this.item.setDangerousLevel(0);

    this.itemPickupPoint = new ItemPickupPoint();
    this.itemPickupPoint1 = new ItemPickupPoint();
    this.itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    this.itemPickupPoint.setMerchantSku(ObjectConverterServiceImplTest.MERCHANT_SKU);
    this.itemPickupPoint.getPrice().add(this.price);
    this.itemPickupPoint.setItemViewConfig(itemViewConfigs);

    this.company = new CompanyDTO();
    this.company.setCityName(ObjectConverterServiceImplTest.CITY_NAME);
    this.company.setAddress1(ObjectConverterServiceImplTest.ADDRESS1);
    this.company.setProvinceName(ObjectConverterServiceImplTest.PROVINCE_NAME);
    this.company.setZipCode(ObjectConverterServiceImplTest.ZIP_CODE);
    this.company.setBusinessPartnerName(ObjectConverterServiceImplTest.BUSINESS_PARTNER_NAME);
    this.company.setPhone(ObjectConverterServiceImplTest.PHONE);
    this.company.setEmail(ObjectConverterServiceImplTest.EMAIL);
    this.company.setPurchaseTerm(ObjectConverterServiceImplTest.PURCHASE_TERM);

    this.responsiblePerson = new ResponsiblePersonDTO();
    this.responsiblePerson.setName(ObjectConverterServiceImplTest.RESPONSIBLE_PERSON_NAME);

    this.merchantProfile = new ProfileResponse();
    this.merchantProfile.setCompany(this.company);
    this.merchantProfile.setResponsiblePerson(this.responsiblePerson);

    this.mapOfListOfItemCategoryList = new HashMap<String, List<List<ItemCategoryVO>>>();
    this.mapOfListOfItemCategoryList.put(ObjectConverterServiceImplTest.CATALOG_CODE_MASTER,
        new ArrayList<List<ItemCategoryVO>>());

    // Removed ProductHelperService mock setup - now using static method

    this.attributeResponse = new AttributeResponse();
    this.attributeResponse.setAttributeCode(ObjectConverterServiceImplTest.ATTRIBUTE_CODE);
    this.attributeResponse.setName(ObjectConverterServiceImplTest.ATTRIBUTE_NAME);
    this.attributeResponse.setAttributeType(ObjectConverterServiceImplTest.DEFINING_ATTRIBUTE);
    this.attributeResponse.setDescription(ObjectConverterServiceImplTest.DESCRIPTION.getBytes());
    this.attributeResponse.setSearchAble(ObjectConverterServiceImplTest.SEARCHABLE);
    this.attributeResponse.setSkuValue(ObjectConverterServiceImplTest.IS_SKU_VALUE);
    this.attributeResponse.setStoreId(ObjectConverterServiceImplTest.STORE_ID);
    this.attributeResponse.setBasicView(ObjectConverterServiceImplTest.IS_BASIC_VIEW);
    this.attributeResponse.setMandatory(ObjectConverterServiceImplTest.MANDATORY);
    this.attributeResponse.setVariantCreation(true);

    this.comboRule = new ComboRuleVO();
    this.comboRule.setItemSku(ITEM_SKU);
    this.comboRule.setDiscountPercentage(10.0);
    this.comboRule.setMainSku(true);
    this.comboRule.setQuantity(10);

    this.comboRules = new ArrayList<>();
    this.comboRules.add(comboRule);

    this.promoBundlingDetailResponseVO = new PromoBundlingDetailResponseVO();
    this.promoBundlingDetailResponseVO.setItemSku(ITEM_SKU);
    this.promoBundlingDetailResponseVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.promoBundlingDetailResponseVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.promoBundlingDetailResponseVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.promoBundlingDetailResponseVO.setComboRules(comboRules);

    this.promoBundlingDetailResponses = new ArrayList<>();
    this.promoBundlingDetailResponses.add(promoBundlingDetailResponseVO);

    this.request = new SimpleSetStringRequest();
    this.itemCodes = new HashSet<>();
    this.itemCodes.add(ITEM_CODE);

    this.comboItemVO = new ComboItemVO();
    this.comboItemVO.setItemSku(ITEM_SKU);
    this.comboItemVO.setQuantity(10);
    this.comboItemVO.setDiscountPercentage(10.0);
    this.comboItemVO.setMainSku(true);

    this.comboItemVOList = new ArrayList<>();
    this.comboItemVOList.add(comboItemVO);

    this.comboVO = new ComboVO();
    this.comboVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.comboVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.comboVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);

    this.comboVOList = new ArrayList<>();
    this.comboVOList.add(comboVO);

    this.wholesaleRuleVO = new WholesaleRuleVO();
    this.wholesaleRuleVO.setMinQuantity(MIN_QUANTITY);
    this.wholesaleRuleVO.setMaxQuantity(MAX_QUANTITY);
    this.wholesaleRuleVO.setDiscountPercentage(DISCOUNT_PERCENTAGE);

    this.wholesaleRuleVOList = new ArrayList<>();
    this.wholesaleRuleVOList.add(wholesaleRuleVO);

    this.wholesaleVO = new WholesaleVO();
    this.wholesaleVO.setItemCode(ITEM_CODE_1);
    this.wholesaleVO.setItemSku(ITEM_SKU_1);
    this.wholesaleVO.setOfferPrice(10000.0);

    this.wholesaleRuleVO = new WholesaleRuleVO();
    this.wholesaleRuleVO.setMinQuantity(MIN_QUANTITY);
    this.wholesaleRuleVO.setMaxQuantity(MAX_QUANTITY);
    this.wholesaleRuleVO.setDiscountPercentage(DISCOUNT_PERCENTAGE);

    this.wholesaleRuleVOList = new ArrayList<>();
    this.wholesaleRuleVOList.add(wholesaleRuleVO);

    this.promoBundlingDetailResponseWholesaleVO = new PromoBundlingDetailResponseVO();
    this.promoBundlingDetailResponseWholesaleVO.setItemSku(ITEM_SKU_1);
    this.promoBundlingDetailResponseWholesaleVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.promoBundlingDetailResponseWholesaleVO.setPromoBundlingType(PROMO_BUNDLING_TYPE_WHOLESALE);
    this.promoBundlingDetailResponseWholesaleVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.promoBundlingDetailResponseWholesaleVO.setWholesaleRules(this.wholesaleRuleVOList);

    this.promoBundlingDetailResponsesWholesaleVO = new ArrayList<>();
    this.promoBundlingDetailResponsesWholesaleVO.add(promoBundlingDetailResponseWholesaleVO);

    offlineItemDetailVo = new OfflineItemDetailVo();
    offlineItemDetailVo.setItemSku(ITEM_SKU);
    offlineItemDetailVo.setPickupPointCode(PICKUP_POINT_CODE);
    offlineItemDetailVos = new ArrayList<>();
    offlineItemDetailVos.add(offlineItemDetailVo);

    businessPartnerChange = new BusinessPartnerChange();
    companyVO = new CompanyVO();
    businessPartnerChange.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerChange.setBusinessPartnerType(BUSINESS_PARTNER_TYPE);
    businessPartnerChange.setAllCategory(true);
    businessPartnerChange.setMerchantStatus(STATUS);
    businessPartnerChange.setStoreId(Constants.STORE_ID);
    List<String> salesChannel = new ArrayList<>();
    salesChannel.add(Constants.B2B_SELLER_CHANNEL);
    companyVO.setBusinessPartnerAlias(ALIAS_BUSINESS_PARTNER);
    companyVO.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    companyVO.setLinkedPartnerStore(LINKED_BUSINESS_PARTNER);
    companyVO.setInternationalFlag(true);
    companyVO.setUmkmFlag(true);
    companyVO.setOfflineToOnlineFlag(true);
    companyVO.setCncActivated(true);
    companyVO.setName(NAME);
    companyVO.setInventoryFulfillment(INVENTORY_TYPE);
    companyVO.setMerchantType(TYPE);
    companyVO.setSupplierFlag(true);
    companyVO.setCustomerFlag(true);
    companyVO.setMerchantFlag(true);
    companyVO.setMerchantDeliveryType(MERCHANT_DELIVERY_TYPE);
    companyVO.setSalesChannel(salesChannel);
    businessPartnerChange.setCompany(companyVO);

    productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setBrand(BRAND);
    productAndItemSolr.setItemName(GENERATED_ITEM_NAME);
    productAndItemSolr.setMasterCatalog(CATALOG_CODE + Constants.DELIMETER + CATEGORY_CODE);

    categoryNamesResponse = new CategoryNamesResponse();
    Map<String, String> categoryMap = new HashMap();
    categoryMap.put(CATEGORY_CODE, CATEGORY_NAME);
    categoryNamesResponse.setCategoryMap(categoryMap);

    preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(PREORDER_VALUE);
    itemActivationRequest =
        NeedCorrectionItemActivationRequest.builder().itemSku(ITEM_SKU).isBuyable(true).isDiscoverable(true)
                .listPrice(10.0).offerPrice(11.0).merchantSku(MERCHANT_SKU).pickupPointCode(PICKUP_POINT_CODE).wholesalePriceActivated(null).build();
    productItemResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    image.setMainImages(true);
    image.setLocationPath(IMAGE_PATH);
    productItemResponse.setImages(Arrays.asList(image));
    productItemAttributeValueResponse.setAttributeResponse(new AttributeResponse());
    productItemAttributeValueResponse.getAttributeResponse().setAttributeCode(ATTRIBUTE_CODE);
    productItemAttributeValueResponse.getAttributeResponse().setName(ATTRIBUTE_NAME);
    productItemAttributeValueResponse.setValue(ITEM_ATTRIBUTE_VALUE);
    productItemAttributeValueResponse.getAttributeResponse().setAttributeType(AttributeType.DEFINING_ATTRIBUTE.name());
    productItemResponse.setProductItemAttributeValueResponses(Arrays.asList(productItemAttributeValueResponse));
    productItemResponse.setItemHeight(ITEM_HEIGHT);
    productItemResponse.setItemWeight(ITEM_HEIGHT);
    productItemResponse.setItemWidth(ITEM_WIDTH);
    productItemResponse.setItemLength(ITEM_LENGTH);
    productItemResponse.setItemDeliveryWeight(ITEM_HEIGHT);
    productMasterDataResponse.setHeight(ITEM_HEIGHT);
    productMasterDataResponse.setWeight(ITEM_HEIGHT);
    productMasterDataResponse.setWidth(ITEM_WIDTH);
    productMasterDataResponse.setShippingWeight(ITEM_HEIGHT);
    productMasterDataResponse.setBrand(BRAND);
    productMasterDataResponse.setName(PRODUCT_NAME);
    productMasterDataResponse.setProductCode(PRODUCT_CODE);
    productMasterDataResponse.setProductItemResponses(Collections.singleton(productItemResponse));

    PriceModel price = new PriceModel();
    price.setOfferPrice(OFFER_PRICE);
    productEventModel =
        ProductEventModel.builder().productSku(PRODUCT_SKU).productCode(PRODUCT_CODE).productScore(new ProductScoreVo())
            .productType(ProductType.BOPIS).tradingProduct(true).isSuspended(true).merchantCode(MERCHANT_CODE)
            .productCatentryId(PRODUCT_CATENTRY_ID)
            .salesCatalogs(Arrays.asList(new SalesCatalogModel())).build();
    itemEventModel = ItemEventModel.builder().itemSku(ITEM_SKU).activePromoBundlings(new HashSet<>()).cncActivated(true)
        .generatedItemName(NAME).isArchived(true).itemCode(ITEM_CODE).isLateFulfillment(true).isSynchronized(true)
        .isBuyable(true).isDiscoverable(true).channel(CHANNEL).merchantSku(MERCHANT_SKU).merchantPromoDiscount(true)
        .off2OnChannelActive(true).pickupPointCode(PICKUP_POINT_CODE).price(new HashSet<>(Arrays.asList(price)))
        .pristineId(PRISTINE_ID).ticketTemplateCode(TYPE).build();

    productAndItemSolr1 = new ProductAndItemSolr();
    productAndItemSolr1.setItemCode(ITEM_CODE);
    productAndItemSolr1.setItemSku(ITEM_SKU);
    productAndItemSolr1.setMerchantSku(MERCHANT_SKU);
    productAndItemSolr1.setProductName(PRODUCT_NAME);
    productAndItemSolr1.setMerchantCode(MERCHANT_CODE);
    productAndItemSolr1.setProductCode(PRODUCT_CODE);
    productAndItemSolr1.setProductSku(PRODUCT_SKU);
    productAndItemSolr1.setSalesCatalog(Arrays.asList(SALES_CATEGORY));
    productAndItemSolr1.setMasterCatalog(MASTER_CATEGORY);
    productAndItemSolrs = new ArrayList<>();
    productAndItemSolrs.add(productAndItemSolr1);

    this.itemPickupPoint = new ItemPickupPoint();
    this.itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    this.itemPickupPoint.setMerchantSku(ObjectConverterServiceImplTest.MERCHANT_SKU);
    this.itemPickupPoint.getPrice().add(this.price);
    this.itemPickupPoint.setItemViewConfig(itemViewConfigs);
    masterDataItems  = new HashMap<>();
    masterDataItems.put(ITEM_CODE, masterDataItem);
    dbItemMap = new HashMap<>();
    dbItemMap.put(ITEM_SKU, item);
    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    Set<Price> prices = new HashSet<>(Arrays.asList(new Price("$", 10, 10, "", "", new Date())));
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.setPrice(prices);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);

    itemPickupPointMap =
        ImmutableMap.of(item.getItemSku(),
            ItemPickupPoint.builder().price(item.getPrice()).itemViewConfig(item.getItemViewConfigs()).build());

    pickupPointVO = new PickupPointVOEventModel();
    pickupPointVO.setCode(CODE);
    pickupPointVO.setExternalPickupPointCode(EXTERNAL_PICK_UP_POINT_CODE);
    pickupPointVO.setName(NAME);
    pickupPointVO.setAddress(ADDRESS);
    pickupPointVO.setCountryCode(COUNTRY_CODE);
    pickupPointVO.setProvinceCode(PROVINCE_CODE);
    pickupPointVO.setCityCode(CITY_CODE);
    pickupPointVO.setDistrictCode(DISTRICT_CODE);
    pickupPointVO.setSubDistrictCode(SUB_DISTRICT_CODE);
    pickupPointVO.setZipCode(ZIP_CODE);
    pickupPointVO.setProvinceName(PROVINCE_NAME);
    pickupPointVO.setCityName(CITY_NAME);
    pickupPointVO.setDistrictName(DISTRICT_NAME);
    pickupPointVO.setSubDistrictName(SUB_DISTRICT_NAME);
    pickupPointVO.setTelephone(TELEPHONE);
    pickupPointVO.setWarehouseId(WAREHOUSE_ID);
    pickupPointVO.setOriginId(ORIGIN_ID);
    contactPersonVO = new ContactPersonVOEventModel();
    contactPersonVO.setName(CONTACT_PERSON);
    contactPersonVO.setEmail(EMAIL);
    geolocationVO = new GeolocationVOEventModel();
    geolocationVO.setLatitude(100.0);
    geolocationVO.setLongitude(200.0);
    geolocationVO.setPlaceId(PLACE_ID);
    pickupPointVO.setContactPerson(contactPersonVO);
    pickupPointVO.setGeolocation(geolocationVO);
    pickupPointVO.setCoverageAreaSetting(COVERAGE_AREA_SETTINGS);
    pickupPointVO.setFax(FAX);
    pickupPointVO.setAdditionalInfo(ADDITIONAL_INFO);
    pickupPointVO.setLocationId(LOCATION_ID);
    pickupPointVO.setCncActivated(true);
    businessHourVO = new BusinessHourVOEventModel();
    businessHourVO.setDay(DayOfWeek.FRIDAY);
    businessHourVO.setClosingTimeInSeconds(10);
    pickupPointVO.setBusinessHours(Collections.singletonList(businessHourVO));
    pickupPointVO.setFlags(PickupPointFlagVOEventModel.builder().deliveryFlag(true).build());

    pickupPointChange = new PickupPointVOEventModel();
    pickupPointChange.setCode(CODE);
    pickupPointChange.setExternalPickupPointCode(EXTERNAL_PICK_UP_POINT_CODE);
    pickupPointChange.setName(NAME);
    pickupPointChange.setAddress(ADDRESS);
    pickupPointChange.setCountryCode(COUNTRY_CODE);
    pickupPointChange.setProvinceCode(PROVINCE_CODE);
    pickupPointChange.setCityCode(CITY_CODE);
    pickupPointChange.setDistrictCode(DISTRICT_CODE);
    pickupPointChange.setSubDistrictCode(SUB_DISTRICT_CODE);
    pickupPointChange.setZipCode(ZIP_CODE);
    pickupPointChange.setProvinceName(PROVINCE_NAME);
    pickupPointChange.setCityName(CITY_NAME);
    pickupPointChange.setDistrictName(DISTRICT_NAME);
    pickupPointChange.setSubDistrictName(SUB_DISTRICT_NAME);
    pickupPointChange.setTelephone(TELEPHONE);
    pickupPointChange.setWarehouseId(WAREHOUSE_ID);
    pickupPointChange.setOriginId(ORIGIN_ID);
    pickupPointChange.setContactPerson(contactPersonVO);
    pickupPointChange.setGeolocation(geolocationVO);
    pickupPointChange.setCoverageAreaSetting(COVERAGE_AREA_SETTINGS);
    pickupPointChange.setFax(FAX);
    pickupPointChange.setAdditionalInfo(ADDITIONAL_INFO);
    pickupPointChange.setLocationId(LOCATION_ID);
    pickupPointChange.setCncActivated(true);
    pickupPointChange.setBusinessHours(Collections.singletonList(businessHourVO));
    pickupPointChange.setFlags(PickupPointFlagVOEventModel.builder().deliveryFlag(true).build());

    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint1.setDelivery(false);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(Mockito.anyString(),
        Mockito.anyString(), eq(true), eq(false))).thenReturn(Arrays.asList(itemPickupPoint1));

    productAttributeDomainEventModel = new ProductAttributeDomainEventModel();
    AttributeDomainEventModel attribute = new AttributeDomainEventModel();
    attribute.setAttributeType(com.gdn.x.productcategorybase.AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    attribute.setVariantCreation(true);
    productAttributeDomainEventModel.setAttribute(attribute);
    ProductAttributeValueDomainEventModel productAttributeValueDomainEventModel = new ProductAttributeValueDomainEventModel();
    AllowedAttributeValueDomainEventModel allowedAttributeValueDomainEventModel = new AllowedAttributeValueDomainEventModel();
    allowedAttributeValueDomainEventModel.setValue(DEFINING_ATTRIBUTE);
    productAttributeValueDomainEventModel.setAllowedAttributeValue(allowedAttributeValueDomainEventModel);
    productAttributeValueDomainEventModel.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeDomainEventModel.setProductAttributeValues(Arrays.asList(productAttributeValueDomainEventModel));

    productMap = new HashMap<>();
    productMap.put(product.getProductSku(), product);
    productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeCode(ObjectConverterServiceImplTest.ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ObjectConverterServiceImplTest.ATTRIBUTE_NAME);
    productAttributeDetail.setAttributeValue(ObjectConverterServiceImplTest.ATTRIBUTE_VALUE);

    itemPickupPoints = new ArrayList<>();
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setCncActive(CNC_ACTIVE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoints.add(itemPickupPoint);

    imageResponse =
      ImageResponse.builder().locationPath(LOCATION_PATH1).sequence(1).active(Boolean.TRUE)
        .originalImage(Boolean.FALSE).mainImage(Boolean.TRUE).build();
    imageResponses.add(imageResponse);
    itemResponse = new ItemResponse();
    itemResponse.setItemCode(ITEM_CODE);
    itemResponse.setMerchantCode(MERCHANT_CODE);
    itemResponse.setProductSku(PRODUCT_SKU);
    itemResponse.setItemSku(ITEM_SKU);
    priceDTO = new PriceDTO();
    priceDTO.setListOfDiscountPrices(new ArrayList<>());
    priceDTO.setListPrice(SALE_PRICE);
    priceDTO.setOfferPrice(OFFER_PRICE);
    priceDTO.setMerchantPromoDiscountPrice(null);
    priceDTOS = new HashSet<>();
    priceDTOS.add(priceDTO);
    itemResponse.setPrice(priceDTOS);
    itemResponses = new ArrayList<>();
    itemResponses.add(itemResponse);
    this.masterDataProductImageDTO = new MasterDataProductImageDTO();
    this.masterDataProductImageDTO.setProductCode(PRODUCT_CODE);
    this.masterDataProductImageDTO.setLocationPath(LOCATION_PATH1);
    this.masterDataProductImageDTO.setMainImage(Boolean.TRUE);
    this.masterDataProductImageDTO.setCommonImage(Boolean.TRUE);
  }

  @Test
  public void convertToPristineItemTest(){
    PristineItemVO pristineItemVO = new PristineItemVO();
    when(gdnMapper.deepCopy(this.item, PristineItemVO.class))
        .thenReturn(pristineItemVO);

    PristineItemVO result = this.objectConverterServiceImpl.convertToPristineItem(this.item);
    Assertions.assertEquals(pristineItemVO, result);

    verify(gdnMapper).deepCopy(this.item, PristineItemVO.class);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.gdnMapper);
    verifyNoMoreInteractions(this.itemHelperService);
    verifyNoMoreInteractions(this.itemService);
  }

  @Test
  public void
  convertMasterDataProductAndItemsVoToOfferPageHeaderVoTest_NullMasterDataProductAndItemsVO
      () {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  objectConverterServiceImpl.convertMasterDataProductAndItemsVoToOfferPageHeaderVo(null,
        "itemCode", "itemSku"));
  }

  @Test
  public void convertMasterDataProductAndItemsVoToOfferPageHeaderVoTest_NullItemCode() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  objectConverterServiceImpl.convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
        new MasterDataProductAndItemsVO(), null, "itemSku"));
  }

  @Test
  public void convertMasterDataProductAndItemsVoToOfferPageHeaderVoTest_Success() {
    MasterDataProductImage masterDataProductImage = new MasterDataProductImage();
    masterDataProductImage.setLocationPath("locationPath");
    masterDataProductImage.setMainImage(true);

    List<MasterDataProductImage> masterDataProductImages = new ArrayList<>();
    masterDataProductImages.add(masterDataProductImage);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    masterDataProduct.setBrand("brand");
    masterDataProduct.setProductName("productName");

    //--- Master Data Item ---//

    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeName("attributeName");
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);

    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    masterDataAttribute2.setAttributeName("attributeName");
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);

    MasterDataItemAttributeValue masterDataItemAttributeValue = new MasterDataItemAttributeValue();
    masterDataItemAttributeValue.setAttributeValue("attributeValue");
    masterDataItemAttributeValue.setMasterDataAttribute(masterDataAttribute);

    MasterDataItemAttributeValue masterDataItemAttributeValue2 = new MasterDataItemAttributeValue();
    masterDataItemAttributeValue2.setAttributeValue("attributeValue");
    masterDataItemAttributeValue2.setMasterDataAttribute(masterDataAttribute2);

    List<MasterDataItemAttributeValue> masterDataItemAttributeValues = new ArrayList<>();
    masterDataItemAttributeValues.add(masterDataItemAttributeValue);
    masterDataItemAttributeValues.add(masterDataItemAttributeValue2);

    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setMainImage(true);
    masterDataItemImage.setLocationPath("locationPath");

    List<MasterDataItemImage> masterDataItemImages = new ArrayList<>();
    masterDataItemImages.add(masterDataItemImage);

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setSkuCode("itemCode");
    masterDataItem.setProductCode("productCode");
    masterDataItem.setGeneratedItemName("generatedItemName");
    masterDataItem.setMasterDataItemImages(masterDataItemImages);
    masterDataItem.setMasterDataItemAttributeValues(masterDataItemAttributeValues);

    Map<String, MasterDataItem> stringMasterDataItemMap = new HashMap<>();
    stringMasterDataItemMap.put("itemCode", masterDataItem);
    stringMasterDataItemMap.put("itemCode2", masterDataItem);

    MasterDataProductAndItemsVO masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);
    masterDataProductAndItemsVO.setMasterDataItems(stringMasterDataItemMap);
    ReflectionTestUtils.setField(objectConverterServiceImpl, "cncForWarehouseFeatureSwitch", true);

    objectConverterServiceImpl.convertMasterDataProductAndItemsVoToOfferPageHeaderVo(
        masterDataProductAndItemsVO, "itemCode", "itemSku");
  }

  @Test
  public void convertPristineItemDetailAndMappingVoToOfferPageHeaderVoTest(){
    PristineItemDetailAndMappingVo pristineItemDetailAndMappingVo =
        new PristineItemDetailAndMappingVo();
    List<PristineSimilarItemVo> pristineSimilarItemVos = new ArrayList<>();
    Map<String, String> attribute = new HashMap<>();
    attribute.put("1","attribute");
    pristineSimilarItemVos.add(new PristineSimilarItemVo("id", "name", attribute));
    pristineItemDetailAndMappingVo.setOtherPristineItems(pristineSimilarItemVos);
    pristineItemDetailAndMappingVo.setBrand("brand");

    OfferedSummaryVo offeredSummaryVo = new OfferedSummaryVo();
    offeredSummaryVo.setBrand("brand");

    when(gdnMapper.deepCopy(pristineItemDetailAndMappingVo, OfferedSummaryVo.class))
        .thenReturn(offeredSummaryVo);

    OfferedSummaryVo response = objectConverterServiceImpl
        .convertPristineItemDetailAndMappingVoToOfferedSummaryVo(pristineItemDetailAndMappingVo);
    assertEquals(response.getBrand(), pristineItemDetailAndMappingVo.getBrand());
    verify(gdnMapper).deepCopy(pristineItemDetailAndMappingVo, OfferedSummaryVo.class);
  }

  @Test
  public void convertProductToOfferPageHeaderVoTest() throws Exception{
    MandatoryRequestParam param = MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID,
        CHANNEL_ID, CLIENT_ID, REQUEST_ID);

    MasterDataProductImage masterDataProductImage = new MasterDataProductImage();
    masterDataProductImage.setLocationPath("locationPath");
    masterDataProductImage.setMainImage(true);

    List<MasterDataProductImage> masterDataProductImages = new ArrayList<>();
    masterDataProductImages.add(masterDataProductImage);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    masterDataProduct.setBrand("brand");
    masterDataProduct.setProductName("productName");

    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue("value");
    productAttributeDetail.setAttributeName("name");

    List<ProductAttributeDetail> productAttributeDetails = new ArrayList<>();
    productAttributeDetails.add(productAttributeDetail);

    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(item.getItemSku());
    productAttribute.setProductAttributeDetails(productAttributeDetails);

    ProductAttribute productAttribute2 = new ProductAttribute();
    productAttribute2.setItemSku("itemSku2");
    productAttribute2.setProductAttributeDetails(productAttributeDetails);

    List<ProductAttribute> productAttributes = new ArrayList<>();
    productAttributes.add(productAttribute);
    productAttributes.add(productAttribute2);

    MasterDataItemImage masterDataItemImage = new MasterDataItemImage();
    masterDataItemImage.setLocationPath("location-path");
    masterDataItemImage.setMainImage(true);

    List<MasterDataItemImage> masterDataItemImages = new ArrayList<>();
    masterDataItemImages.add(masterDataItemImage);

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName("item-name");
    masterDataItem.setMasterDataItemImages(masterDataItemImages);

    product.setMasterDataProduct(masterDataProduct);
    product.setDefiningAttributes(productAttributes);

    item.setMasterDataItem(masterDataItem);

    objectConverterServiceImpl.convertProductToOfferedSummaryVo(param, product, item);
  }

  @Test
  public void convertComboRulesResponseToPromoBundlingItemVO_success(){
    List<ComboItemVO> result =
        this.objectConverterServiceImpl.convertComboRulesResponseToComboItemVO(comboRules);
    assertNotNull(result);
    assertEquals(comboItemVOList, result);
  }

  @Test
  public void convertPromoBundlingDetailResponseToPromoBundlingDetailVo_success() {
    PromoBundlingDetailResponse promoBundlingDetailResponse = new PromoBundlingDetailResponse();
    promoBundlingDetailResponse.setItemSku(ITEM_SKU);
    promoBundlingDetailResponse.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponse.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingDetailResponse.setPromoBundlingName(PROMO_BUNDLING_NAME);
  }

  @Test
  public void convertItemToComboItemVO_successPristineTrue(){
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    item.setPristineDataItem(pristineDataItem);
    item.setOfflineItems(offlineItemDetailVos);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToComboItemVO(comboItemVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    assertEquals(comboItemVO.getProductName(), PRISTINE_PRODUCT_NAME);
  }

  @Test
  public void convertItemToComboItemVO_successPristineFalse(){
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setProductName(PRODUCT_NAME);
    product.setMasterDataProduct(masterDataProduct);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToComboItemVO(comboItemVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    assertEquals(comboItemVO.getProductName(), PRODUCT_NAME);
  }

  @Test
  public void convertItemToItemInfoVO(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(item.getMasterDataItem().getGeneratedItemName(), itemInfoVO.getProductName());
  }

  @Test
  public void convertItemToItemInfoVOVaraintCreationAttribute(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    item.getMasterDataItem().getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setVariantCreation(true);
    item.getMasterDataItem().getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(item.getMasterDataItem().getGeneratedItemName(), itemInfoVO.getProductName());
  }

  @Test
  public void convertItemToItemInfoVOVaraintCreationAttribute1(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    item.getMasterDataItem().setGeneratedItemName(PRISTINE_PRODUCT_NAME);
    item.getMasterDataItem().getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setVariantCreation(true);
    item.getMasterDataItem().getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(PRISTINE_PRODUCT_NAME, itemInfoVO.getProductName());
  }

  @Test
  public void convertItemToItemInfoVOVaraintCreationAttribute2(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    item.getMasterDataItem().getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setVariantCreation(false);
    item.getMasterDataItem().getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(item.getMasterDataItem().getGeneratedItemName(), itemInfoVO.getProductName());
  }

  @Test
  public void convertItemToItemInfoVO_whenMasterDataItemIsNull(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineBrand(BRAND);
    pristineDataItem.setPristineModel(MODEL);
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);
    this.item.setMasterDataItem(null);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(item.getPristineDataItem().getPristineProductName(), itemInfoVO.getProductName());
    assertNull(itemInfoVO.getImageUrl());
    assertNull(itemInfoVO.getMasterDataItemAttributes());
  }

  @Test
  public void convertItemToItemInfoVO_whenMasterDataItemImagesNull(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineBrand(BRAND);
    pristineDataItem.setPristineModel(MODEL);
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);
    this.item.getMasterDataItem().setMasterDataItemImages(null);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    verify(this.itemHelperService).getDiscountPrice(price);

    assertEquals(item.getPristineDataItem().getPristineProductName(), itemInfoVO.getProductName());
  }

  @Test
  public void convertPromoBundlingDetailResponseVoToComboVO_success(){
    ComboVO result = this.objectConverterServiceImpl
        .convertPromoBundlingDetailResponseVoToComboVO(promoBundlingDetailResponseVO);

    assertNotNull(result);
    assertEquals(comboVO, result);
  }

  @Test
  public void convertPromoBundlingDetailResponseVOToPromoBundlingVO_success() {
    Map<String, Price> priceByItemSku = new HashMap<>();
    priceByItemSku.put(ITEM_SKU, price);

    when(this.itemPriceService.getFinalPrice(price ,10.0)).thenReturn(10000.0);

    this.objectConverterServiceImpl
        .convertPromoBundlingDetailResponseVOToPromoBundlingVO(promoBundlingDetailResponseVO, priceByItemSku);

    verify(this.itemPriceService).getFinalPrice(price ,10.0);
  }

  @Disabled
  @Test
  public void convertProductToItemInfoVO_success(){
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setCatgroupId(CATEGORY_ID);

    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(CATALOG_CODE);
    masterCatalog.setCategory(category);

    product.getMasterDataProduct().setMasterCatalog(masterCatalog);

    ItemCategoryVO itemCategoryVO = new ItemCategoryVO();
    itemCategoryVO.setCategory(CATEGORY_CODE);
    itemCategoryVO.setCategoryId(CATEGORY_ID);
    itemCategoryVO.setLevel(LEVEL);
    itemCategoryVO.setProductCategoryCode(PRODUCT_CODE);

    List<ItemCategoryVO> itemCategoryVOList = new ArrayList<>();
    itemCategoryVOList.add(itemCategoryVO);

    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    itemCatalogVO.setCatalogId(CATALOG_ID);
    itemCatalogVO.setItemCategories(itemCategoryVOList);
    this.product.setItemCatalogs(itemCatalogs);
    this.objectConverterServiceImpl.convertProductToComboItemVO(comboItemVO, product);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsTestNullMasterCatalog() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    productAndItemSolr.setMasterCatalog(null);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.isNull())).thenReturn(productAndItemSolr);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
    Mockito.verify(productAndItemSolrRepository).findOne(Mockito.anyString(), Mockito.isNull());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsTestNullResponse() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.isNull())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(null);
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
    Mockito.verify(productCategoryBaseClient).getCategoryNames(Mockito.anyList());
    Mockito.verify(productAndItemSolrRepository).findOne(Mockito.anyString(), Mockito.isNull());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsActualPristineResponse() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses1(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    List<SimpleProductAndItemsVO> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExists(products, items, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
  }

  @Test
  public void convertItemToItemInfoVO_whenMasterDataItemIsNullDummyPristineIdTest(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);
    this.item.setMasterDataItem(null);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertNull(itemInfoVO.getProductName());
    assertNull(itemInfoVO.getImageUrl());
    assertNull(itemInfoVO.getMasterDataItemAttributes());
  }

  @Test
  public void convertItemToItemInfoVO_nullPristineItemTest(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    this.item.setPristineDataItem(null);
    this.item.setOfflineItems(offlineItemDetailVos);
    this.item.setMasterDataItem(null);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertNull(itemInfoVO.getProductName());
    assertNull(itemInfoVO.getImageUrl());
    assertNull(itemInfoVO.getMasterDataItemAttributes());
  }

  @Test
  public void convertItemToItemInfoVO_EmptyBrand(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineModel(MODEL);
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);
    this.item.setMasterDataItem(null);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(PRISTINE_PRODUCT_NAME, itemInfoVO.getProductName());
    assertNull(itemInfoVO.getImageUrl());
    assertNull(itemInfoVO.getMasterDataItemAttributes());
  }

  @Test
  public void convertItemToItemInfoVO_EmptyModel(){
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    comboItemVO.setItemSku(ITEM_SKU);
    comboItemVO.setQuantity(10);
    comboItemVO.setDiscountPercentage(10.0);
    comboItemVO.setMainSku(true);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    pristineDataItem.setId("ID");
    pristineDataItem.setPristineBrand(BRAND);
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setOfflineItems(offlineItemDetailVos);
    this.item.setMasterDataItem(null);

    when(this.itemHelperService.getDiscountPrice(price)).thenReturn(10000.0);

    this.objectConverterServiceImpl.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);

    verify(this.itemHelperService).getDiscountPrice(price);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(null, item.getItemSku(), Boolean.TRUE, Boolean.FALSE);
    assertEquals(PRISTINE_PRODUCT_NAME, itemInfoVO.getProductName());
    assertNull(itemInfoVO.getImageUrl());
    assertNull(itemInfoVO.getMasterDataItemAttributes());
  }

  @Test
  public void getItemForActivationTest() {
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setItemViewConfigs(new HashSet<>());
    objectConverterServiceImpl.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    assertNotNull(item);
  }

  @Test
  public void getItemForActivationWholepriceNotNull() {
    itemActivationRequest.setWholesalePriceActivated(false);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setItemViewConfigs(new HashSet<>());
    objectConverterServiceImpl.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    assertNotNull(item);
  }

  @Test
  public void getItemForActivationNoChangeFalseTest() {
    itemActivationRequest.setWholesalePriceActivated(false);
    item.setActivePromoBundlings(new HashSet<>());
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setItemViewConfigs(new HashSet<>());
    objectConverterServiceImpl.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    assertNotNull(item);
  }

  @Test
  public void getItemForActivationWholesalePriceTrueTest() {
    itemActivationRequest.setWholesalePriceActivated(true);
    itemActivationRequest.setPickupPointCode(null);
    item.setActivePromoBundlings(new HashSet<>());
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setItemViewConfigs(new HashSet<>());
    objectConverterServiceImpl.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    assertNotNull(item);
  }

  @Test
  public void getItemForActivationWholesalePriceTrueNoChangeTest() {
    itemActivationRequest.setWholesalePriceActivated(true);
    item.setPickupPointCode(PICKUP_POINT_CODE);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setItemViewConfigs(new HashSet<>());
    objectConverterServiceImpl.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    assertNotNull(item);
  }

  @Test
  public void getItemForActivationTest_noPrice() {
    objectConverterServiceImpl.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    assertNotNull(item);
  }

  @Test
  public void convertToProductForTransactionNewMasterDataTest() {
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_unsyncProductTest() {
    this.product.setSynchronized(false);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs,
        productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_unsyncProduct_nullMasterDataItemDimensionsTest() {
    this.masterDataItem.setItemWeight(null);
    this.masterDataItem.setItemWidth(null);
    this.masterDataItem.setItemLength(null);
    this.masterDataItem.setItemHeight(null);
    this.masterDataItem.setItemDeliveryWeight(null);
    this.product.setSynchronized(false);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs,
        productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nullMasterDataProductTest() {
    this.product.setMasterDataProduct(null);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertFalse(response.getItemDetail().isBuyable());
    Assertions.assertFalse(response.getItemDetail().isDiscoverable());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nullMasterDataItemTest() {
    this.item.setMasterDataItem(null);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertFalse(response.getItemDetail().isBuyable());
    Assertions.assertFalse(response.getItemDetail().isDiscoverable());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_withPristineDataTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_onlyBrandTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    item.getPristineDataItem().setPristineBrand(BRAND);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(PRODUCT_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_withPristineModelTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.getPristineDataItem().setPristineModel(BRAND);
    item.getPristineDataItem().setPristineBrand(BRAND);
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(PRODUCT_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nonNullProductMasterDataResponseTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nonNullProductMasterDataResponse_nullDimensionsTest() {
    productMasterDataResponse.getProductItemResponses().stream().forEach(productItemResponse1 -> {
      productItemResponse1.setItemHeight(null);
      productItemResponse1.setItemLength(null);
      productItemResponse1.setItemWeight(null);
      productItemResponse1.setItemWidth(null);
      productItemResponse1.setItemDeliveryWeight(null);
      productItemResponse1.setDangerousGoodsLevel(0);
      productItemResponse1.getProductItemAttributeValueResponses().get(0).getAttributeResponse()
        .setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    });
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(PRODUCT_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nonNullProductMasterDataResponse_withPristineTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    item.getPristineDataItem().setPristineBrand(BRAND);
    item.getPristineDataItem().setPristineModel(BRAND);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(PRODUCT_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nonNullProductMasterDataResponse_variantCreatingAttributeTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    productMasterDataResponse.getProductItemResponses().stream().findFirst().get().getProductItemAttributeValueResponses()
      .get(0).getAttributeResponse().setVariantCreation(true);
    productMasterDataResponse.getProductItemResponses().stream().findFirst().get().getProductItemAttributeValueResponses()
      .get(0).getAttributeResponse().setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nonNullProductMasterDataResponse_syncTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.setSynchronized(true);
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    productMasterDataResponse.getProductItemResponses().stream().findFirst().get().getProductItemAttributeValueResponses()
      .get(0).getAttributeResponse().setVariantCreation(true);
    productMasterDataResponse.getProductItemResponses().stream().findFirst().get().getProductItemAttributeValueResponses()
      .get(0).getAttributeResponse().setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_nonNullProductMasterDataResponse_unsyncTest() {
    item.setPristineDataItem(new PristineDataItem());
    item.setSynchronized(false);
    item.setMasterDataItem(null);
    item.getPristineDataItem().setPristineProductName(PRODUCT_NAME);
    productMasterDataResponse.getProductItemResponses().stream().findFirst().get().getProductItemAttributeValueResponses()
      .get(0).getAttributeResponse().setVariantCreation(true);
    productMasterDataResponse.getProductItemResponses().stream().findFirst().get().getProductItemAttributeValueResponses()
      .get(0).getAttributeResponse().setAttributeType(AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, productMasterDataResponse);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
    Assertions.assertEquals(PRODUCT_NAME, response.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_archivedTest() {
    this.item.setArchived(true);
    this.item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_archivedFalseTest() {
    this.item.setArchived(false);
    this.item.getItemViewConfigs().stream().findFirst().get().setDiscoverable(true);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
  }

  @Test
  public void convertToProductForTransactionNewMasterData_buyableFalseTest() {
    this.item.getItemViewConfigs().stream().findFirst().get().setBuyable(false);
    when(this.masterDataConstructorService
      .constructItemDimensionFields(this.item.getMasterDataItem(),
        this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO response = this.objectConverterServiceImpl
      .convertToProductForTransactionNewMasterData(this.product, this.item, this.itemCatalogs, null);
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU, response.getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
      response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.getItemDetail().getImageUrl());
  }

  @Test
  public void convertToProductTest() {
    productEventModel.getProductScore().setTotalScore(100);
    productEventModel.getSalesCatalogs().get(0).setCatalogCode(CATALOG_CODE);
    productEventModel.getSalesCatalogs().get(0).setCategoryCodes(Arrays.asList(CATEGORY_CODE));
    productEventModel.setProductName(PRODUCT_NAME);
    productEventModel.setCategoryCode(CATEGORY_CODE);
    productEventModel.setCatalogCode(CATALOG_CODE);
    productEventModel.setFbbActivated(true);
    Product product = objectConverterServiceImpl.convertToProduct(productEventModel);
    Assertions.assertEquals(PRODUCT_SKU, product.getProductSku());
    Assertions.assertEquals(PRODUCT_CODE, product.getProductCode());
    Assertions.assertEquals(ProductType.BOPIS, product.getProductType());
    Assertions.assertTrue(product.isTradingProduct());
    Assertions.assertTrue(product.isSuspended());
    Assertions.assertEquals(MERCHANT_CODE, product.getMerchantCode());
    Assertions.assertEquals(PRODUCT_CATENTRY_ID, product.getProductCatentryId());
    Assertions.assertEquals( product.getProductScore().getTotalScore(), 100,0);
    Assertions.assertEquals(CATALOG_CODE, product.getSalesCatalogs().get(0).getCatalogCode());
    Assertions.assertEquals(PRODUCT_NAME, product.getMasterDataProduct().getProductName());
    Assertions.assertEquals(CATEGORY_CODE,
      product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode());
    Assertions.assertTrue(product.isFbbActivated());
  }

  @Test
  public void convertToListItemTest() {
    List<Item> itemList = objectConverterServiceImpl.convertToListItem(Arrays.asList(itemEventModel), PRODUCT_SKU, false);
    Assertions.assertEquals(ITEM_SKU, itemList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemList.get(0).getItemCode());
    Assertions.assertEquals(NAME, itemList.get(0).getMasterDataItem().getGeneratedItemName());
    Assertions.assertTrue(itemList.get(0).isCncActivated());
    Assertions.assertTrue(itemList.get(0).isArchived());
    Assertions.assertTrue(itemList.get(0).isMerchantPromoDiscount());
    Assertions.assertEquals(MERCHANT_SKU, itemList.get(0).getMerchantSku());
    Assertions.assertTrue(itemList.get(0).isOff2OnChannelActive());
    Assertions.assertTrue(itemList.get(0).isSynchronized());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemList.get(0).getPickupPointCode());
    Assertions.assertEquals(PRISTINE_ID, itemList.get(0).getPristineDataItem().getPristineId());
    Assertions.assertTrue(itemList.get(0).getItemViewConfigs().stream().findFirst().get().isBuyable());
    Assertions.assertEquals(ObjectConverterServiceImplTest.OFFER_PRICE,
        itemList.get(0).getPrice().stream().findFirst().get().getOfferPrice(), 0);
  }

  @Test
  public void convertToListItemPricceTest() {
    itemEventModel.getPrice().stream().findFirst().get().setMerchantPromoDiscountPrice(new DiscountPriceModel());
    itemEventModel.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPriceModel()));
    List<Item> itemList = objectConverterServiceImpl.convertToListItem(Arrays.asList(itemEventModel), PRODUCT_SKU, false);
    Assertions.assertEquals(ITEM_SKU, itemList.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemList.get(0).getItemCode());
    Assertions.assertEquals(NAME, itemList.get(0).getMasterDataItem().getGeneratedItemName());
    Assertions.assertTrue(itemList.get(0).isCncActivated());
    Assertions.assertTrue(itemList.get(0).isArchived());
    Assertions.assertTrue(itemList.get(0).isMerchantPromoDiscount());
    Assertions.assertEquals(MERCHANT_SKU, itemList.get(0).getMerchantSku());
    Assertions.assertTrue(itemList.get(0).isOff2OnChannelActive());
    Assertions.assertTrue(itemList.get(0).isSynchronized());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemList.get(0).getPickupPointCode());
    Assertions.assertEquals(PRISTINE_ID, itemList.get(0).getPristineDataItem().getPristineId());
    Assertions.assertTrue(itemList.get(0).getItemViewConfigs().stream().findFirst().get().isBuyable());
    Assertions.assertEquals(ObjectConverterServiceImplTest.OFFER_PRICE,
        itemList.get(0).getPrice().stream().findFirst().get().getOfferPrice(), 0);
  }

  @Test
  public void convertToListItemneedToOverrideL4DetailsFromL5TrueTest() {
    itemEventModel.setStoreId(STORE_ID);
    objectConverterServiceImpl.convertToListItem(Arrays.asList(itemEventModel), PRODUCT_SKU, true);
    Mockito.verify(itemPickupPointService).findByStoreIdAndProductSkuAndDelivery(STORE_ID, PRODUCT_SKU, true);
  }

  @Test
  public void convertToProductEventModelTest() {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setCurationStatus(CurationStatus.NONE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE,
        Arrays.asList(new Category(CATEGORY_CODE, CATEGORY_ID), new Category(CATEGORY_CODE1, CATEGORY_ID1)))));
    masterDataProduct.setMasterCatalog(new MasterCatalog());
    masterDataProduct.getMasterCatalog().setCategory(new Category(CATEGORY_CODE, CATEGORY_ID));
    product.setMasterDataProduct(masterDataProduct);
    ProductEventModel productEventModel = objectConverterServiceImpl.convertToProductEventModel(product);
    Assertions.assertEquals(STORE_ID, productEventModel.getStoreId());
    Assertions.assertEquals(TOTAL_SCORE, productEventModel.getProductScore().getTotalScore(), 0);
    Assertions.assertEquals(CATEGORY_CODE, productEventModel.getCategoryCode());
    Assertions.assertEquals(CATALOG_CODE, productEventModel.getSalesCatalogs().get(0).getCatalogCode());
    Assertions.assertEquals(CATEGORY_CODE1, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(1));
    Assertions.assertEquals(CATEGORY_CODE, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(0));
  }

  @Test
  public void convertToProductEventModelMasterCatalogNullTest() {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setCurationStatus(CurationStatus.NONE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE,
        Arrays.asList(new Category(CATEGORY_CODE, CATEGORY_ID), new Category(CATEGORY_CODE1, CATEGORY_ID1)))));
    masterDataProduct.setMasterCatalog(null);
    product.setMasterDataProduct(masterDataProduct);
    ProductEventModel productEventModel = objectConverterServiceImpl.convertToProductEventModel(product);
    Assertions.assertEquals(STORE_ID, productEventModel.getStoreId());
    Assertions.assertEquals(TOTAL_SCORE, productEventModel.getProductScore().getTotalScore(), 0);
    Assertions.assertEquals(CATALOG_CODE, productEventModel.getSalesCatalogs().get(0).getCatalogCode());
    Assertions.assertEquals(CATEGORY_CODE1, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(1));
    Assertions.assertEquals(CATEGORY_CODE, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(0));
  }

  @Test
  public void convertToProductEventModelMasterDataNullTest() {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setCurationStatus(CurationStatus.NONE);
    product.setSalesCatalogs(new ArrayList<>());
    product.setMasterDataProduct(null);
    ProductEventModel productEventModel = objectConverterServiceImpl.convertToProductEventModel(product);
    Assertions.assertEquals(STORE_ID, productEventModel.getStoreId());
    Assertions.assertEquals(TOTAL_SCORE, productEventModel.getProductScore().getTotalScore(), 0);
  }

  @Test
  public void convertToItemEventModelTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemViewConfigs(itemViewConfigs);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRISTINE_ID, itemEventModel.getPristineId());
    Assertions.assertEquals(DISCOUNT_PRICE,
      itemEventModel.getPrice().iterator().next().getMerchantPromoDiscountPrice()
        .getDiscountPrice(), 0);
    Assertions.assertEquals(ADJUSTMENT_TYPE,
      itemEventModel.getPrice().iterator().next().getListOfDiscountPrices().get(0)
        .getAdjustmentType());
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToProductEventModelMasterCatalogIsBundleProductTrue() {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setCurationStatus(CurationStatus.NONE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE,
        Arrays.asList(new Category(CATEGORY_CODE, CATEGORY_ID), new Category(CATEGORY_CODE1, CATEGORY_ID1)))));
    product.setBundleProduct(true);
    masterDataProduct.setMasterCatalog(null);
    product.setMasterDataProduct(masterDataProduct);
    ProductEventModel productEventModel = objectConverterServiceImpl.convertToProductEventModel(product);
    Assertions.assertEquals(STORE_ID, productEventModel.getStoreId());
    Assertions.assertEquals(TOTAL_SCORE, productEventModel.getProductScore().getTotalScore(), 0);
    Assertions.assertEquals(CATALOG_CODE, productEventModel.getSalesCatalogs().get(0).getCatalogCode());
    Assertions.assertEquals(CATEGORY_CODE1, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(1));
    Assertions.assertEquals(CATEGORY_CODE, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(0));
    Assertions.assertTrue(productEventModel.isBundleProduct());
  }

  @Test
  public void convertToProductEventModelMasterCatalogIsBundleProductFalse() {
    product.setDistributionStatus(DistributionStatus.DISTRIBUTION);
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setCurationStatus(CurationStatus.NONE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE,
        Arrays.asList(new Category(CATEGORY_CODE, CATEGORY_ID), new Category(CATEGORY_CODE1, CATEGORY_ID1)))));
    product.setBundleProduct(false);
    masterDataProduct.setMasterCatalog(null);
    product.setMasterDataProduct(masterDataProduct);
    ProductEventModel productEventModel = objectConverterServiceImpl.convertToProductEventModel(product);
    Assertions.assertEquals(STORE_ID, productEventModel.getStoreId());
    Assertions.assertEquals(TOTAL_SCORE, productEventModel.getProductScore().getTotalScore(), 0);
    Assertions.assertEquals(CATALOG_CODE, productEventModel.getSalesCatalogs().get(0).getCatalogCode());
    Assertions.assertEquals(CATEGORY_CODE1, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(1));
    Assertions.assertEquals(CATEGORY_CODE, productEventModel.getSalesCatalogs().get(0).getCategoryCodes().get(0));
    Assertions.assertFalse(productEventModel.isBundleProduct());
    Assertions.assertEquals(2, productEventModel.getDistributionStatus());
  }

  @Test
  public void convertToItemEventModelIsSynchronizedTrueTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemViewConfigs(itemViewConfigs);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    item.setSynchronized(true);
    Mockito.when(
            itemHelperService.processDiscountPricesByPriority(discountPriceList))
        .thenReturn(discountPrice);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRISTINE_ID, itemEventModel.getPristineId());
    Assertions.assertEquals(DISCOUNT_PRICE,
        itemEventModel.getPrice().iterator().next().getMerchantPromoDiscountPrice()
            .getDiscountPrice(), 0);
    Assertions.assertEquals(ADJUSTMENT_TYPE,
        itemEventModel.getPrice().iterator().next().getListOfDiscountPrices().get(0)
            .getAdjustmentType());
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToItemEventModelIsSynchronizedTrueMasterDataItemNullTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemViewConfigs(itemViewConfigs);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    item.setSynchronized(true);
    item.setMasterDataItem(null);
    Mockito.when(
            itemHelperService.processDiscountPricesByPriority(discountPriceList))
        .thenReturn(discountPrice);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRISTINE_ID, itemEventModel.getPristineId());
    Assertions.assertEquals(DISCOUNT_PRICE,
        itemEventModel.getPrice().iterator().next().getMerchantPromoDiscountPrice()
            .getDiscountPrice(), 0);
    Assertions.assertEquals(ADJUSTMENT_TYPE,
        itemEventModel.getPrice().iterator().next().getListOfDiscountPrices().get(0)
            .getAdjustmentType());
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToItemEventModelIsSynchronizedFalseTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemViewConfigs(itemViewConfigs);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    item.setSynchronized(false);
    item.getMasterDataItem().setGeneratedItemName(GENERATED_ITEM_NAME);
    Mockito.when(
            itemHelperService.processDiscountPricesByPriority(discountPriceList))
        .thenReturn(discountPrice);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRISTINE_ID, itemEventModel.getPristineId());
    Assertions.assertEquals(DISCOUNT_PRICE,
        itemEventModel.getPrice().iterator().next().getMerchantPromoDiscountPrice()
            .getDiscountPrice(), 0);
    Assertions.assertEquals(ADJUSTMENT_TYPE,
        itemEventModel.getPrice().iterator().next().getListOfDiscountPrices().get(0)
            .getAdjustmentType());
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToItemEventModelIsSynchronizedFalseMasterDataItemNullTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemViewConfigs(itemViewConfigs);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    item.setSynchronized(false);
    item.setMasterDataItem(null);
    Mockito.when(
            itemHelperService.processDiscountPricesByPriority(discountPriceList))
        .thenReturn(discountPrice);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRISTINE_ID, itemEventModel.getPristineId());
    Assertions.assertEquals(DISCOUNT_PRICE,
        itemEventModel.getPrice().iterator().next().getMerchantPromoDiscountPrice()
            .getDiscountPrice(), 0);
    Assertions.assertEquals(ADJUSTMENT_TYPE,
        itemEventModel.getPrice().iterator().next().getListOfDiscountPrices().get(0)
            .getAdjustmentType());
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToItemEventModelPriceNullTest() {
    item.setItemViewConfigs(itemViewConfigs);
    item.setPristineDataItem(null);
    item.setMasterDataItem(null);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(null);
    item.getPrice().iterator().next().setListOfDiscountPrices(null);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToListItemEventModelTest() {
    item.setPristineDataItem(pristineDataItem);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    List<ItemEventModel> itemEventModels =
      objectConverterServiceImpl.convertToListItemEventModel(Arrays.asList(item));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(GENERATED_ITEM_NAME, itemEventModels.get(0).getGeneratedItemName());
    Assertions.assertEquals(PRISTINE_ID, itemEventModels.get(0).getPristineId());
  }

  @Test
  public void convertToItemEventModelNullDiscountTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemViewConfigs(itemViewConfigs);
    discountPrice.setDiscountPrice(0.0);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(null);
    ItemEventModel itemEventModel = objectConverterServiceImpl.convertToItemEventModel(item);
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRISTINE_ID, itemEventModel.getPristineId());
    Assertions.assertEquals(0,
      itemEventModel.getPrice().iterator().next().getMerchantPromoDiscountPrice()
        .getDiscountPrice(), 0);
    Assertions.assertEquals(true, itemEventModel.isBuyable());
  }

  @Test
  public void convertToProductAndItemEventModelTest() {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE,
      Arrays.asList(new Category(CATEGORY_CODE, CATEGORY_ID),
        new Category(CATEGORY_CODE1, CATEGORY_ID1)))));
    masterDataProduct.setMasterCatalog(new MasterCatalog());
    masterDataProduct.getMasterCatalog().setCategory(new Category(CATEGORY_CODE, CATEGORY_ID));
    product.setMasterDataProduct(masterDataProduct);
    product.setPreOrder(new PreOrder());
    product.getPreOrder().setIsPreOrder(true);
    product.setCurationStatus(CurationStatus.NONE);
    item.setPristineDataItem(pristineDataItem);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    ProductAndItemEventModel productAndItemEventModel =
      objectConverterServiceImpl.convertToProductAndItemEventModel(
        new ProductAndItemsVO(product, Arrays.asList(item)));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModel.getProductSku());
    Assertions.assertEquals(STORE_ID, productAndItemEventModel.getProduct().getStoreId());
    Assertions.assertEquals(PRISTINE_ID, productAndItemEventModel.getItems().get(0).getPristineId());
  }

  @Test
  public void convertToProductAndItemEventModelItemPickupPointTest() {
    product.setProductScore(new ProductScore());
    product.getProductScore().setTotalScore(TOTAL_SCORE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE,
      Arrays.asList(new Category(CATEGORY_CODE, CATEGORY_ID),
        new Category(CATEGORY_CODE1, CATEGORY_ID1)))));
    masterDataProduct.setMasterCatalog(new MasterCatalog());
    masterDataProduct.getMasterCatalog().setCategory(new Category(CATEGORY_CODE, CATEGORY_ID));
    product.setMasterDataProduct(masterDataProduct);
    product.setPreOrder(new PreOrder());
    product.getPreOrder().setIsPreOrder(true);
    product.setCurationStatus(CurationStatus.NONE);
    item.setPristineDataItem(pristineDataItem);
    discountPrice.setDiscountPrice(DISCOUNT_PRICE);
    discountPriceList.get(0).setAdjustmentType(ADJUSTMENT_TYPE);
    item.getPrice().iterator().next().setMerchantPromoDiscountPrice(discountPrice);
    item.getPrice().iterator().next().setListOfDiscountPrices(discountPriceList);
    ItemPickupPoint itemPickupPoint =
      ItemPickupPoint.builder().itemSku(item.getItemSku()).price(item.getPrice())
        .itemViewConfig(item.getItemViewConfigs()).build();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, Arrays.asList(item));
    ProductItemsVo productItemsVo = generateProductItemVo(product, item, itemPickupPoint);
    productItemsVo.getItemVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    ProductAndItemEventModel productAndItemEventModel =
      objectConverterServiceImpl.convertToProductAndItemEventModel(productAndItemsVO,
        productItemsVo.getItemVoList());
    verify(itemHelperService, times(2)).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(PICKUP_POINT_CODE, productAndItemEventModel.getItems().get(0).getPickupPointCode());
    Assertions.assertEquals(PRODUCT_SKU, productAndItemEventModel.getProductSku());
    Assertions.assertEquals(STORE_ID, productAndItemEventModel.getProduct().getStoreId());
    Assertions.assertEquals(PRISTINE_ID, productAndItemEventModel.getItems().get(0).getPristineId());
  }

  @Test
  public void constructItemSummaryListResponse() {
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(0)
      .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues()
      .add(new MasterDataItemAttributeValue());
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(1)
      .setMasterDataAttribute(new MasterDataAttribute());
    PriceDTO priceDTO = new PriceDTO();
    dbItemMap.get(ITEM_SKU).setPrice(Collections.singleton(price));
    dbItemMap.get(ITEM_SKU).setItemViewConfigs(null);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(Collections.singletonList(discountPrice)))
      .thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      objectConverterServiceImpl.constructItemSummaryListResponse(productAndItemSolrs,
        itemPickupPointMap, masterDataItems);
    verify(itemHelperService).processDiscountPricesByPriority(Collections.singletonList(discountPrice));
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(IMAGE_PATH, itemSummaryListResponses.get(0).getMainImageUrl());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
    Assertions.assertEquals(MERCHANT_CODE, itemSummaryListResponses.get(0).getMerchantCode());
    Assertions.assertEquals(Arrays.asList(CATALOG_CODE_SALES_A),
      itemSummaryListResponses.get(0).getSalesCategoryCode());
  }

  @Test
  public void constructItemSummaryListResponseUsingNewMasterData(){
    item.setProductSku(product.getProductSku());
    product.setProductName(PRODUCT_NAME);
    item.setItemCode(ITEM_CODE);
    item.setMainImageUrl(IMAGE_PATH);
    item.setCategoryCode(CATEGORY_CODE);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(Collections.singletonList(discountPrice)))
      .thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      objectConverterServiceImpl.constructItemSummaryListResponseUsingNewMasterData(
        Arrays.asList(item), productMap, itemPickupPointMap);
    verify(itemHelperService).processDiscountPricesByPriority(Collections.singletonList(discountPrice));
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(IMAGE_PATH, itemSummaryListResponses.get(0).getMainImageUrl());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
  }

  @Test
  public void constructItemSummaryListResponseWithEmptyDiscountPriceTest(){
    item.setProductSku(product.getProductSku());
    product.setProductName(PRODUCT_NAME);
    item.setItemCode(ITEM_CODE);
    item.setMainImageUrl(IMAGE_PATH);
    item.setCategoryCode(CATEGORY_CODE);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(Collections.singletonList(discountPrice)))
      .thenReturn(null);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      objectConverterServiceImpl.constructItemSummaryListResponseUsingNewMasterData(
        Arrays.asList(item), productMap, itemPickupPointMap);
    verify(itemHelperService).processDiscountPricesByPriority(Collections.singletonList(discountPrice));
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(IMAGE_PATH, itemSummaryListResponses.get(0).getMainImageUrl());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
  }

  @Test
  public void constructItemSummaryListResponseUsingNewMasterDataTest(){
    item.setProductSku(product.getProductSku());
    product.setProductName(PRODUCT_NAME);
    item.setItemCode(ITEM_CODE);
    item.setMainImageUrl(IMAGE_PATH);
    item.setCategoryCode(CATEGORY_CODE);
    product.setSalesCatalogs(Arrays.asList(new SalesCatalog(CATALOG_CODE, null)));
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(Collections.singletonList(discountPrice)))
      .thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      objectConverterServiceImpl.constructItemSummaryListResponseUsingNewMasterData(
        Arrays.asList(item), productMap, itemPickupPointMap);
    verify(itemHelperService).processDiscountPricesByPriority(Collections.singletonList(discountPrice));
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(IMAGE_PATH, itemSummaryListResponses.get(0).getMainImageUrl());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
    Assertions.assertEquals(Arrays.asList(), itemSummaryListResponses.get(0).getSalesCategoryCode());
  }

  @Test
  public void constructItemSummaryListResponseUsingNewMasterData1Test(){
    item.setProductSku(product.getProductSku());
    product.setProductName(PRODUCT_NAME);
    item.setItemCode(ITEM_CODE);
    item.setMainImageUrl(IMAGE_PATH);
    item.setCategoryCode(CATEGORY_CODE);
    product.setDefiningAttributes(Collections
        .singletonList(new ProductAttribute(item.getItemSku(), Collections.singletonList(productAttributeDetail))));
    productMap.put(product.getProductSku(), product);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    product.setSalesCatalogs(Arrays.asList(
      new SalesCatalog(CATALOG_CODE, Arrays.asList(new Category(SALES_CATEGORY_DB, CODE)))));
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(Collections.singletonList(discountPrice)))
      .thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      objectConverterServiceImpl.constructItemSummaryListResponseUsingNewMasterData(
        Arrays.asList(item), productMap, itemPickupPointMap);
    verify(itemHelperService).processDiscountPricesByPriority(Collections.singletonList(discountPrice));
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(IMAGE_PATH, itemSummaryListResponses.get(0).getMainImageUrl());
    Assertions.assertEquals(CATEGORY_CODE, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
    Assertions.assertEquals(Arrays.asList(SALES_CATEGORY_DB),
      itemSummaryListResponses.get(0).getSalesCategoryCode());
    Assertions.assertEquals(ATTRIBUTE_NAME,
      itemSummaryListResponses.get(0).getDefiningAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ATTRIBUTE_VALUE,
      itemSummaryListResponses.get(0).getDefiningAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE,
      itemSummaryListResponses.get(0).getDefiningAttributes().get(0).getAttributeCode());
  }

  @Test
  public void constructItemSummaryListResponseWithPriceAndItemViewConfig(){
    productAndItemSolrs.get(0).setMasterCatalog(null);
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    discountPriceList.add(new DiscountPrice());
    price.setListOfDiscountPrices(discountPriceList);
    price.setMerchantPromoDiscountPrice(discountPrice);
    price.getMerchantPromoDiscountPrice().setAdjustmentType(AdjustmentType.BLIBLI);
    itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    dbItemMap.get(ITEM_SKU).setPrice(new HashSet<>(Arrays.asList(price)));
    dbItemMap.get(ITEM_SKU).setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfig)));
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(0)
      .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues()
      .add(new MasterDataItemAttributeValue());
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(1)
      .setMasterDataAttribute(new MasterDataAttribute());
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
      objectConverterServiceImpl.constructItemSummaryListResponse(productAndItemSolrs,
        itemPickupPointMap, new HashMap<>());
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(null, itemSummaryListResponses.get(0).getMainImageUrl());
    assertNotNull(itemSummaryListResponses.get(0).getItemViewConfigs());
    assertNotNull(itemSummaryListResponses.get(0).getPrice());
    Assertions.assertEquals(OFFER_PRICE, itemSummaryListResponses.get(0).getOriginalSellingPrice(),
      0.0);
    Assertions.assertEquals(null, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
    Assertions.assertEquals(MERCHANT_CODE, itemSummaryListResponses.get(0).getMerchantCode());
    Assertions.assertEquals(Arrays.asList(CATALOG_CODE_SALES_A),
      itemSummaryListResponses.get(0).getSalesCategoryCode());
  }

  @Test
  public void constructItemSummaryListResponseWithPriceAndItemViewConfigProductTypeNotNullTest(){
    productAndItemSolrs.get(0).setMasterCatalog(null);
    productAndItemSolrs.forEach(productAndItemSolr2 -> productAndItemSolr2.setProductType("bopis"));
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    discountPriceList.add(new DiscountPrice());
    price.setListOfDiscountPrices(discountPriceList);
    price.setMerchantPromoDiscountPrice(discountPrice);
    price.getMerchantPromoDiscountPrice().setAdjustmentType(AdjustmentType.BLIBLI);
    itemViewConfig.setItemBuyableSchedules(new ItemBuyableSchedule());
    itemViewConfig.setItemDiscoverableSchedules(new ItemDiscoverableSchedule());
    dbItemMap.get(ITEM_SKU).setPrice(new HashSet<>(Arrays.asList(price)));
    dbItemMap.get(ITEM_SKU).setItemViewConfigs(new HashSet<>(Arrays.asList(itemViewConfig)));
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(0)
        .getMasterDataAttribute().setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues()
        .add(new MasterDataItemAttributeValue());
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(1)
        .setMasterDataAttribute(new MasterDataAttribute());
    Mockito.when(
            itemHelperService.processDiscountPricesByPriority(discountPriceList))
        .thenReturn(discountPrice);
    List<ItemSummaryListResponse> itemSummaryListResponses =
        objectConverterServiceImpl.constructItemSummaryListResponse(productAndItemSolrs,
            itemPickupPointMap, new HashMap<>());
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    assertNull(itemSummaryListResponses.get(0).getMainImageUrl());
    assertNotNull(itemSummaryListResponses.get(0).getItemViewConfigs());
    assertNotNull(itemSummaryListResponses.get(0).getPrice());
    Assertions.assertEquals(OFFER_PRICE, itemSummaryListResponses.get(0).getOriginalSellingPrice(),
        0.0);
    assertNull(itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
    Assertions.assertEquals(MERCHANT_CODE, itemSummaryListResponses.get(0).getMerchantCode());
    Assertions.assertEquals(List.of(CATALOG_CODE_SALES_A),
        itemSummaryListResponses.get(0).getSalesCategoryCode());
  }

  @Test
  public void constructItemSummaryListResponseWithPriceAndItemViewConfigWithNull(){
    productAndItemSolrs.get(0).setMasterCatalog("");
    Price price = new Price();
    price.setOfferPrice(OFFER_PRICE);
    discountPriceList.add(new DiscountPrice());
    itemPickupPointMap.get(item.getItemSku())
        .setPrice(new HashSet<>());
    itemPickupPointMap.get(item.getItemSku())
        .setItemViewConfig(new HashSet<>());
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(0).getMasterDataAttribute()
        .setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().add(new MasterDataItemAttributeValue());
    masterDataItems.get(ITEM_CODE).getMasterDataItemAttributeValues().get(1).setMasterDataAttribute(new MasterDataAttribute());
    List<ItemSummaryListResponse> itemSummaryListResponses =
        objectConverterServiceImpl.constructItemSummaryListResponse(productAndItemSolrs, itemPickupPointMap, masterDataItems);
    Assertions.assertEquals(ITEM_SKU, itemSummaryListResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_CODE, itemSummaryListResponses.get(0).getItemCode());
    Assertions.assertEquals(MERCHANT_SKU, itemSummaryListResponses.get(0).getMerchantSku());
    Assertions.assertEquals(PRODUCT_NAME, itemSummaryListResponses.get(0).getProductName());
    Assertions.assertEquals(IMAGE_PATH, itemSummaryListResponses.get(0).getMainImageUrl());
    assertNull(itemSummaryListResponses.get(0).getItemViewConfigs());
    assertNull(itemSummaryListResponses.get(0).getPrice());
    Assertions.assertEquals(null, itemSummaryListResponses.get(0).getMasterCategoryCode());
    Assertions.assertEquals(PRODUCT_SKU, itemSummaryListResponses.get(0).getProductSku());
    Assertions.assertEquals(MERCHANT_CODE, itemSummaryListResponses.get(0).getMerchantCode());
    Assertions.assertEquals(Arrays.asList(CATALOG_CODE_SALES_A), itemSummaryListResponses.get(0).getSalesCategoryCode());
  }

  private ProductItemsVo generateProductItemVo(Product product, Item item, ItemPickupPoint itemPickupPoint) {
    ProductItemsVo productItemsVo = new ProductItemsVo();
    productItemsVo.setProductVo(Objects.nonNull(product) ? mapper.deepCopy(product, ProductVo.class) : null);
    productItemsVo.setItemVoList(Objects.nonNull(item) ? Arrays.asList(mapper.deepCopy(item, ItemVo.class)) : null);
    if (CollectionUtils.isNotEmpty(productItemsVo.getItemVoList())) {
      productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(new ArrayList<>());
      productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(Objects.nonNull(itemPickupPoint) ?
          Arrays.asList(mapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class)) :
          null);
    }
    return productItemsVo;
  }

  @Test
  public void overrideL4DetailsFromL5Test() {
    objectConverterServiceImpl
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL4DetailsFromL5NotPresentTest() {
    itemPickupPoint.setItemSku(CATEGORY_CODE);
    objectConverterServiceImpl
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL4DetailsFromL4NullTest() {
    itemPickupPoint.setItemSku(CATEGORY_CODE);
    itemPickupPoint.setMerchantSku("newMerchantSku");
    item.setItemSku("category-code");
    objectConverterServiceImpl
        .overrideL4DetailsFromL5(Arrays.asList(item,null), Collections.singletonList(itemPickupPoint));
    Assertions.assertEquals("newMerchantSku",item.getMerchantSku());
  }

  @Test
  public void overrideL5DetailsFromL4Test() {
    item.getPrice().forEach(price1 -> price1.setChannel(DEFAULT_CHANNEL));
    itemPickupPoint.getPrice().forEach(priceL5 -> priceL5.setChannel(DEFAULT_CHANNEL));
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL5DetailsFromL4SameListPriceTest() {
    item.getPrice().forEach(price1 -> {price1.setChannel(DEFAULT_CHANNEL);
    price1.setListPrice(0.0);});
    itemPickupPoint.getPrice().forEach(priceL5 -> {priceL5.setChannel(DEFAULT_CHANNEL);
    priceL5.setListPrice(0.0);});
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL5DetailsFromL4PriceOtherChannelTest() {
    item.getPrice().forEach(price1 -> price1.setChannel(OTHER_CHANNEL));
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL5DetailsFromL5PriceOtherChannelTest() {
    item.getPrice().forEach(priceL4 -> priceL4.setChannel(DEFAULT_CHANNEL));
    itemPickupPoint.getPrice().forEach(priceL5 -> priceL5.setChannel(OTHER_CHANNEL));
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL5DetailsPriceOfferPriceSameTest() {
    item.getPrice().forEach(priceL4 -> {priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(1.0);
      priceL4.setOfferPrice(5.0);});
    itemPickupPoint.getPrice().forEach(priceL5 -> {priceL5.setChannel(DEFAULT_CHANNEL);
      priceL5.setListPrice(1.0);
      priceL5.setOfferPrice(6.0);});
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL5DetailsPriceSameTest() {
    item.getPrice().forEach(priceL4 -> {priceL4.setChannel(DEFAULT_CHANNEL);
      priceL4.setListPrice(5.0);
      priceL4.setOfferPrice(5.0);});
    itemPickupPoint.getPrice().forEach(priceL5 -> {priceL5.setChannel(DEFAULT_CHANNEL);
      priceL5.setListPrice(5.0);
      priceL5.setOfferPrice(5.0);});
    Mockito.when(channelService.getDefaultChannel()).thenReturn(DEFAULT_CHANNEL);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void overrideL5DetailsFromL4NotPresentTest() {
    itemPickupPoint.setItemSku(CATEGORY_CODE);
    objectConverterServiceImpl
        .overrideL5DetailsFromL4(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
  }

  @Test
  public void convertToBusinessPartnerPickupPointServiceTest() {
    BusinessPartnerPickupPoint response =
        objectConverterServiceImpl.convertToBusinessPartnerPickupPoint(pickupPointVO, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    Assertions.assertEquals(EMAIL, response.getContactEmail());
    Assertions.assertEquals(CONTACT_PERSON, response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertEquals(PLACE_ID, response.getGeolocation().getPlaceId());
    Assertions.assertEquals(response.getGeolocation().getLatitude(), 100,0);
    Assertions.assertEquals(response.getGeolocation().getLongitude(), 200, 0);
    Assertions.assertEquals(DayOfWeek.FRIDAY.getValue(), response.getBusinessHours().get(0).getDay().getValue());
    Assertions.assertEquals( response.getBusinessHours().get(0).getClosingTimeInSeconds(), 10,0);
    Assertions.assertEquals(response.isDelivery(), true);
  }

  @Test
  public void convertToBusinessPartnerPickupPointServiceTestNullFlag() {
    pickupPointVO.setFlags(null);
    BusinessPartnerPickupPoint response =
        objectConverterServiceImpl.convertToBusinessPartnerPickupPoint(pickupPointVO, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    Assertions.assertEquals(EMAIL, response.getContactEmail());
    Assertions.assertEquals(CONTACT_PERSON, response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertEquals(PLACE_ID, response.getGeolocation().getPlaceId());
    Assertions.assertEquals(response.getGeolocation().getLatitude(), 100,0);
    Assertions.assertEquals(response.getGeolocation().getLongitude(), 200, 0);
    Assertions.assertEquals(DayOfWeek.FRIDAY.getValue(), response.getBusinessHours().get(0).getDay().getValue());
    Assertions.assertEquals( response.getBusinessHours().get(0).getClosingTimeInSeconds(), 10,0);
    Assertions.assertEquals(response.isDelivery(), false);
  }

  @Test
  public void convertToBusinessPartnerPickupPointServiceTestNullDeliveryFlag() {
    pickupPointVO.setFlags(PickupPointFlagVOEventModel.builder().build());
    BusinessPartnerPickupPoint response =
        objectConverterServiceImpl.convertToBusinessPartnerPickupPoint(pickupPointVO, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    Assertions.assertEquals(EMAIL, response.getContactEmail());
    Assertions.assertEquals(CONTACT_PERSON, response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertEquals(PLACE_ID, response.getGeolocation().getPlaceId());
    Assertions.assertEquals(response.getGeolocation().getLatitude(), 100,0);
    Assertions.assertEquals(response.getGeolocation().getLongitude(), 200, 0);
    Assertions.assertEquals(DayOfWeek.FRIDAY.getValue(), response.getBusinessHours().get(0).getDay().getValue());
    Assertions.assertEquals( response.getBusinessHours().get(0).getClosingTimeInSeconds(), 10,0);
    Assertions.assertEquals(response.isDelivery(), false);
  }

  @Test
  public void convertToBusinessPartnerPickupPointServiceNullTest() {
    pickupPointVO.setBusinessHours(new ArrayList<>());
    pickupPointVO.setGeolocation(null);
    pickupPointVO.setContactPerson(null);
    BusinessPartnerPickupPoint response =
        objectConverterServiceImpl.convertToBusinessPartnerPickupPoint(pickupPointVO, BUSINESS_PARTNER_CODE);
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    assertNull(response.getContactEmail());
    assertNull(response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertTrue(Objects.isNull(response.getGeolocation()));
  }

  @Test
  public void convertToBusinessPartnerPickupPointFromPickupPointChangeTest() {
    BusinessPartnerPickupPoint response = objectConverterServiceImpl
        .convertToBusinessPartnerPickupPointFromPickupPointChange(pickupPointChange, BUSINESS_PARTNER_CODE,
            new BusinessPartnerPickupPoint());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    Assertions.assertEquals(EMAIL, response.getContactEmail());
    Assertions.assertEquals(CONTACT_PERSON, response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertEquals(PLACE_ID, response.getGeolocation().getPlaceId());
    Assertions.assertEquals( response.getGeolocation().getLatitude(),100, 0);
    Assertions.assertEquals( response.getGeolocation().getLongitude(),200, 0);
    Assertions.assertEquals(DayOfWeek.FRIDAY.getValue(), response.getBusinessHours().get(0).getDay().getValue());
    Assertions.assertEquals( response.getBusinessHours().get(0).getClosingTimeInSeconds(), 10,0);
    Assertions.assertEquals(response.isDelivery(), true);
  }

  @Test
  public void convertToBusinessPartnerPickupPointFromPickupPointChangeTestNullFlag() {
    pickupPointChange.setFlags(null);
    BusinessPartnerPickupPoint response = objectConverterServiceImpl
        .convertToBusinessPartnerPickupPointFromPickupPointChange(pickupPointChange, BUSINESS_PARTNER_CODE,
            new BusinessPartnerPickupPoint());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    Assertions.assertEquals(EMAIL, response.getContactEmail());
    Assertions.assertEquals(CONTACT_PERSON, response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertEquals(PLACE_ID, response.getGeolocation().getPlaceId());
    Assertions.assertEquals( response.getGeolocation().getLatitude(),100, 0);
    Assertions.assertEquals( response.getGeolocation().getLongitude(),200, 0);
    Assertions.assertEquals(DayOfWeek.FRIDAY.getValue(), response.getBusinessHours().get(0).getDay().getValue());
    Assertions.assertEquals( response.getBusinessHours().get(0).getClosingTimeInSeconds(), 10,0);
    Assertions.assertEquals(response.isDelivery(), false);
  }

  @Test
  public void convertToBusinessPartnerPickupPointFromPickupPointChangeTestNullDeliveryFlag() {
    pickupPointChange.setFlags(PickupPointFlagVOEventModel.builder().build());
    BusinessPartnerPickupPoint response = objectConverterServiceImpl
        .convertToBusinessPartnerPickupPointFromPickupPointChange(pickupPointChange, BUSINESS_PARTNER_CODE,
            new BusinessPartnerPickupPoint());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    Assertions.assertEquals(EMAIL, response.getContactEmail());
    Assertions.assertEquals(CONTACT_PERSON, response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertEquals(PLACE_ID, response.getGeolocation().getPlaceId());
    Assertions.assertEquals( response.getGeolocation().getLatitude(),100, 0);
    Assertions.assertEquals( response.getGeolocation().getLongitude(),200, 0);
    Assertions.assertEquals(DayOfWeek.FRIDAY.getValue(), response.getBusinessHours().get(0).getDay().getValue());
    Assertions.assertEquals( response.getBusinessHours().get(0).getClosingTimeInSeconds(), 10,0);
    Assertions.assertEquals(response.isDelivery(), false);
  }

  @Test
  public void convertToBusinessPartnerPickupPointFromPickupPointChangeForNullDayTest() {
    BusinessHourVOEventModel businessHourVO = new BusinessHourVOEventModel();
    businessHourVO.setDay(null);
    pickupPointChange.setBusinessHours(Collections.singletonList(businessHourVO));
    BusinessPartnerPickupPoint response = objectConverterServiceImpl
        .convertToBusinessPartnerPickupPointFromPickupPointChange(pickupPointChange, BUSINESS_PARTNER_CODE,
            new BusinessPartnerPickupPoint());
  }
  @Test
  public void convertToBusinessPartnerPickupPointFromPickupPointChangeNullTest() {
    pickupPointChange.setBusinessHours(new ArrayList<>());
    pickupPointChange.setGeolocation(null);
    pickupPointChange.setContactPerson(null);
    BusinessPartnerPickupPoint response = objectConverterServiceImpl
        .convertToBusinessPartnerPickupPointFromPickupPointChange(pickupPointChange, BUSINESS_PARTNER_CODE,
            new BusinessPartnerPickupPoint());
    Assertions.assertEquals(BUSINESS_PARTNER_CODE, response.getBusinessPartnerCode());
    Assertions.assertEquals(ADDRESS, response.getAddress());
    Assertions.assertEquals(ADDITIONAL_INFO, response.getAdditionalInfo());
    Assertions.assertEquals(CITY_CODE, response.getCityCode());
    Assertions.assertEquals(CITY_NAME, response.getCityName());
    Assertions.assertEquals(CODE, response.getCode());
    assertNull(response.getContactEmail());
    assertNull(response.getContactPersonName());
    Assertions.assertEquals(COUNTRY_CODE, response.getCountryCode());
    Assertions.assertEquals(COVERAGE_AREA_SETTINGS, response.getCoverageAreaSetting());
    Assertions.assertEquals(DISTRICT_CODE, response.getDistrictCode());
    Assertions.assertEquals(DISTRICT_NAME, response.getDistrictName());
    Assertions.assertEquals(EXTERNAL_PICK_UP_POINT_CODE, response.getExternalPickupPointCode());
    Assertions.assertEquals(FAX, response.getFax());
    Assertions.assertEquals(NAME, response.getName());
    Assertions.assertEquals(ORIGIN_ID, response.getOriginId());
    Assertions.assertEquals(PROVINCE_CODE, response.getProvinceCode());
    Assertions.assertEquals(PROVINCE_NAME, response.getProvinceName());
    Assertions.assertEquals(SUB_DISTRICT_CODE, response.getSubDistrictCode());
    Assertions.assertEquals(SUB_DISTRICT_NAME, response.getSubDistrictName());
    Assertions.assertEquals(TELEPHONE, response.getTelephone());
    Assertions.assertEquals(LOCATION_ID, response.getLocationId());
    Assertions.assertEquals(WAREHOUSE_ID, response.getWarehouseId());
    Assertions.assertEquals(ZIP_CODE, response.getZipCode());
    Assertions.assertTrue(response.isCncActivated());
    Assertions.assertTrue(Objects.isNull(response.getGeolocation()));
  }

  @Test
  public void toProductAttributeDetail_DESCRIPTIVE_ATTRIBUTE_Test() {
    ProductAttributeDetail productAttributeDetail =
        objectConverterServiceImpl.toProductAttributeDetail(productAttributeDomainEventModel);
    Assertions.assertEquals(DESCRIPTIVE_ATTRIBUTE_VALUE, productAttributeDetail.getAttributeValue());
  }

  @Test
  public void toProductAttributeDetail_DEFINING_ATTRIBUTE_Test() {
    productAttributeDomainEventModel.getAttribute().setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE.name());
    ProductAttributeDetail productAttributeDetail =
        objectConverterServiceImpl.toProductAttributeDetail(productAttributeDomainEventModel);
    Assertions.assertEquals(DEFINING_ATTRIBUTE, productAttributeDetail.getAttributeValue());
  }

  @Test
  public void toProductAttributeDetail_DESCRIPTIVE_ATTRIBUTE_Test1() {
    ProductAttributeDetail productAttributeDetail =
        objectConverterServiceImpl.toProductAttributeDetail(productItemAttributeValueResponse);
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE, productAttributeDetail.getAttributeValue());
  }

  @Test
  public void convertItemToLevel4SummaryResponseTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
      ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(NAME);
    masterDataItem.setUpcCode(CODE);
    item1.setMasterDataItem(masterDataItem);
    item1.setSynchronized(Boolean.FALSE);
    List<Item> items = new ArrayList<>();
    items.add(item1);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    List<ItemLevel4ListingResponse> level4SummaryResponses = new CopyOnWriteArrayList<>();
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse
      .class))
      .thenReturn(itemLevel4ListingResponse1);
    List<ItemLevel4ListingResponse> level4ListingResponseList =
      objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>() );
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    verify(gdnMapper).deepCopy(item1,ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(level4ListingResponseList.get(0));
    Assertions.assertEquals(NAME,level4ListingResponseList.get(0).getGeneratedItemName());
  }

  @Test
  public void convertItemToLevel4SummaryResponseItemSkuAndPPMapTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1, ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(NAME);
    masterDataItem.setUpcCode(CODE);
    item1.setMasterDataItem(masterDataItem);
    item1.setSynchronized(Boolean.FALSE);
    item1.setItemSku(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    items.add(item1);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    itemPickupPoints.get(0).setItemSku(ITEM_SKU_1);
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse.class)).thenReturn(itemLevel4ListingResponse1);
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    List<ItemLevel4ListingResponse> level4ListingResponseList =
        objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, itemPickupPoints, new HashMap<>());
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    verify(gdnMapper).deepCopy(item1, ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(level4ListingResponseList.get(0));
    Assertions.assertEquals(NAME, level4ListingResponseList.get(0).getGeneratedItemName());
    Assertions.assertEquals(PICKUP_POINT_CODE, level4ListingResponseList.get(0).getPickupPointCodes().get(0));
  }

  @Test
  public void convertItemToItemLevel5ResponseTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    item.setBundleRecipe(ImmutableSet.of(new BundleRecipe()));
    items.add(item);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    product.setProductName(PRODUCT_NAME);
    product.setProductCode(PRODUCT_CODE);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
        objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items,
            Arrays.asList(product), true, new HashMap<>(), true, FETCH_VIEW_CONFIGS_BY_CHANNEL);
    assertNotNull(item);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level5ResponseList));
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    assertNotNull(items.get(0));
    assertNotNull(level5ResponseList.get(0));
    Assertions.assertFalse(level5ResponseList.get(0).getDelivery());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getPrices().get(0).getPrice(),0);
    Assertions.assertEquals(PRODUCT_NAME, level5ResponseList.get(0).getProductName());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getOriginalSellingPrice(), 0);
    Assertions.assertEquals(PRODUCT_CODE, level5ResponseList.get(0).getProductCode());
  }

  @Test
  public void convertItemToItemLevel5ResponseTestSynchornisedTrue() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    item.setBundleRecipe(ImmutableSet.of(new BundleRecipe()));
    item.setSynchronized(true);
    items.add(item);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    product.setProductName(PRODUCT_NAME);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
        objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items,
            Arrays.asList(product), true, new HashMap<>(), true, FETCH_VIEW_CONFIGS_BY_CHANNEL);
    assertNotNull(item);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level5ResponseList));
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    assertNotNull(items.get(0));
    assertNotNull(level5ResponseList.get(0));
    Assertions.assertFalse(level5ResponseList.get(0).getDelivery());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getPrices().get(0).getPrice(),0);
    Assertions.assertEquals(PRODUCT_NAME, level5ResponseList.get(0).getProductName());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getOriginalSellingPrice(), 0);
  }


  @Test
  public void convertItemToItemLevel5ResponseSynchornisedTrueMasterDataItemNullTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    item.setBundleRecipe(ImmutableSet.of(new BundleRecipe()));
    item.setSynchronized(false);
    item.setMasterDataItem(null);
    items.add(item);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    product.setProductName(PRODUCT_NAME);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
        objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items,
            Arrays.asList(product), true, new HashMap<>(), true, FETCH_VIEW_CONFIGS_BY_CHANNEL);
    assertNotNull(item);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level5ResponseList));
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    assertNotNull(items.get(0));
    assertNotNull(level5ResponseList.get(0));
    Assertions.assertFalse(level5ResponseList.get(0).getDelivery());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getPrices().get(0).getPrice(),0);
    Assertions.assertEquals(PRODUCT_NAME, level5ResponseList.get(0).getProductName());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getOriginalSellingPrice(), 0);
  }


  @Test
  public void convertItemToItemLevel5WithProductDataTest() throws Exception {
    List<Item> items = new ArrayList<>();
    Product product5 = new Product();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    item.setBundleRecipe(ImmutableSet.of(new BundleRecipe()));
    items.add(item);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
      objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items,
        Arrays.asList(product5), true, new HashMap<>(), true, FETCH_VIEW_CONFIGS_BY_CHANNEL);
    assertNotNull(item);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level5ResponseList));
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    assertNotNull(items.get(0));
    assertNotNull(level5ResponseList.get(0));
    Assertions.assertFalse(level5ResponseList.get(0).getDelivery());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getPrices().get(0).getPrice(),0);
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getOriginalSellingPrice(), 0);
  }

  @Test
  public void convertItemToItemLevel5ResponseB2bResponseTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    items.add(item);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setB2bFields(new B2bFields());
    product.setProductName(PRODUCT_NAME);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(Boolean.TRUE);
    preOrder.setPreOrderDate(new Date());
    product.setPreOrder(preOrder);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
        objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items,
            Arrays.asList(product), true, new HashMap<>(), true, FETCH_VIEW_CONFIGS_BY_CHANNEL);
    assertNotNull(item);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level5ResponseList));
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    assertNotNull(items.get(0));
    assertNotNull(level5ResponseList.get(0));
    Assertions.assertFalse(level5ResponseList.get(0).getDelivery());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getPrices().get(0).getPrice(),0);
    Assertions.assertEquals(PRODUCT_NAME, level5ResponseList.get(0).getProductName());
    assertNotNull(level5ResponseList.get(0).getPreOrder());
  }

  @Test
  public void convertItemToItemLevel5ResponseB2bResponseProductNullTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    items.add(item);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setB2bFields(new B2bFields());
    product.setProductName(PRODUCT_NAME);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
        objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items,
            Arrays.asList(null, null), true, new HashMap<>(), true, FETCH_VIEW_CONFIGS_BY_CHANNEL);
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    Assertions.assertEquals(null, itemLevel5Response.getProductName());
  }

  @Test
  public void convertItemToItemLevel5Response_EmptyProductListTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    items.add(item);
    Map<String, Product> productMap = new HashMap<>();
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    product.setProductName(PRODUCT_NAME);
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemPickupPoint.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU);
    when(gdnMapper.deepCopy(item, ItemLevel5Response.class)).thenReturn(itemLevel5Response);
    List<ItemLevel5Response> level5ResponseList =
        objectConverterServiceImpl.convertItemToItemLevel5Response(STORE_ID, itemPickupPointList, items, Arrays.asList(),
            false, new HashMap<>(), true, "ALL");
    assertNotNull(item);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level5ResponseList));
    verify(gdnMapper).deepCopy(item, ItemLevel5Response.class);
    assertNotNull(items.get(0));
    assertNotNull(level5ResponseList.get(0));
    Assertions.assertFalse(level5ResponseList.get(0).getDelivery());
    Assertions.assertEquals(PRICE_1, level5ResponseList.get(0).getPrices().get(0).getPrice(),0);
  }

  @Test
  public void convertItemToLevel4SummaryResponse_NullItemTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
        ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setSynchronized(Boolean.FALSE);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 5)));
    item1.setStoreId("10001");
    List<Item> items = new ArrayList<>();
    items.add(item1);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    List<ItemLevel4ListingResponse> level4SummaryResponses = new CopyOnWriteArrayList<>();
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse
        .class))
        .thenReturn(itemLevel4ListingResponse1);
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    when(itemService.getItemsByStoreIdAndItemSkus(anyString(), anySet())).thenReturn(Arrays.asList(item));
    List<ItemLevel4ListingResponse> level4ListingResponseList =
        objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>());
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    verify(gdnMapper).deepCopy(item1,ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(level4ListingResponseList.get(0));
    Assertions.assertEquals(null,level4ListingResponseList.get(0).getGeneratedItemName());
    verify(itemService).getItemsByStoreIdAndItemSkus(anyString(),anySet());
  }

  @Test
  public void convertItemToLevel4SummaryResponse_NullMasterDataTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
      ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setSynchronized(Boolean.FALSE);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 5)));
    List<Item> items = new ArrayList<>();
    items.add(item1);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    List<ItemLevel4ListingResponse> level4SummaryResponses = new CopyOnWriteArrayList<>();
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse
      .class))
      .thenReturn(itemLevel4ListingResponse1);
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    when(itemService.getItemsByStoreIdAndItemSkus(anyString(), anySet())).thenReturn(Arrays.asList(item));
    List<ItemLevel4ListingResponse> level4ListingResponseList =
      objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>());
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    verify(gdnMapper).deepCopy(item1,ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(level4ListingResponseList.get(0));
    Assertions.assertEquals(null,level4ListingResponseList.get(0).getGeneratedItemName());
    verify(itemService).getItemsByStoreIdAndItemSkus(isNull(), anySet());
  }

  @Test
  public void convertItemToLevel4SummaryResponse_SynchronizedTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
      ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setSynchronized(Boolean.TRUE);
    item1.setItemCode(ITEM_CODE);
    List<Item> items = new ArrayList<>();
    items.add(item1);
    GdnRestListResponse<ItemImageResponse> response = new GdnRestListResponse();
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_CODE));
    skuCodesRequest.setFetchImageResponse(false);
    ItemImageResponse itemImageResponse =
      new ItemImageResponse(ITEM_CODE, UPC_CODE, ITEM_CODE, null);
    itemImageResponse.setItemName(GENERATED_ITEM_NAME);
    response.setContent(List.of(itemImageResponse));
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setItemCode(ITEM_CODE);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ObjectConverterServiceImplTest.ITEM_CODE);
    List<ProductItemDetailResponse> responses =
      Collections.singletonList(productItemDetailResponse);
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    when(productCategoryBaseClient.getProductItemImagesByItemCode(skuCodesRequest)).thenReturn(response);
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse.class)).thenReturn(
      itemLevel4ListingResponse1);
    List<ItemLevel4ListingResponse> level4ListingResponseList =
      objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>());
    verify(productCategoryBaseClient).getProductItemImagesByItemCode(skuCodesRequest);
    verify(gdnMapper).deepCopy(item1, ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    assertNotNull(level4ListingResponseList.get(0));
    Assertions.assertEquals(GENERATED_ITEM_NAME,
      level4ListingResponseList.get(0).getGeneratedItemName());
  }


  @Test
  public void convertItemToLevel4SummaryResponse_SynchronizedWithNullResponseTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1,
      ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    item1.setSynchronized(Boolean.TRUE);
    item1.setItemCode(ITEM_CODE);
    List<Item> items = new ArrayList<>();
    items.add(item1);
    GdnRestListResponse<ItemImageResponse> response = new GdnRestListResponse();
    SkuCodesRequest skuCodesRequest = new SkuCodesRequest();
    skuCodesRequest.setSkuCodes(Collections.singletonList(ITEM_CODE));
    skuCodesRequest.setFetchImageResponse(false);
    ItemImageResponse itemImageResponse =
      new ItemImageResponse(ITEM_CODE, UPC_CODE, ITEM_CODE, null);
    itemImageResponse.setItemName(GENERATED_ITEM_NAME);
    response.setContent(null);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setItemCode(ITEM_CODE);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ObjectConverterServiceImplTest.ITEM_CODE);
    List<ProductItemDetailResponse> responses =
      Collections.singletonList(productItemDetailResponse);
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    when(productCategoryBaseClient.getProductItemImagesByItemCode(skuCodesRequest)).thenReturn(response);
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse.class)).thenReturn(
      itemLevel4ListingResponse1);
    List<ItemLevel4ListingResponse> level4ListingResponseList =
      objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>());
    verify(productCategoryBaseClient).getProductItemImagesByItemCode(skuCodesRequest);
    verify(gdnMapper).deepCopy(item1, ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    assertNotNull(level4ListingResponseList.get(0));
  }


  @Test
  public void convertItemToLevel4SummaryResponse_NullTest() throws Exception {
    Item item1 = null;
    List<Item> items = new ArrayList<>();
    ItemLevel4ListingResponse itemLevel4ListingResponse = new ItemLevel4ListingResponse();
    List<ItemLevel4ListingResponse> itemLevel4ListingResponses = new ArrayList<>();
    itemLevel4ListingResponses.add(itemLevel4ListingResponse);
    try {
      this.objectConverterServiceImpl.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>());
    }
    finally {
      Assertions.assertEquals(null, itemLevel4ListingResponse.getItemSku());
    }
  }

  @Test
  public void convertToProductForTransactionV2Test() throws Exception {
    item.setSynchronized(true);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, this.itemCatalogs);
    Assertions.assertEquals(1.0, result.getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, result.getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, result.getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, result.getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0 , result.getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0 , result.getItemDetail().getDangerousLevel());
    Assertions.assertEquals(ATTRIBUTE_VALUE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_NAME , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
  }


  @Test
  public void convertToProductForTransactionV2WithPristneDataTest() throws Exception {
    item.setSynchronized(true);
    item.setGeneratedItemName(NAME);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    item.setPristineDataItem(pristineDataItem);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, this.itemCatalogs);
    Assertions.assertEquals(1.0, result.getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, result.getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, result.getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, result.getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0 , result.getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0 , result.getItemDetail().getDangerousLevel());
    Assertions.assertEquals(ATTRIBUTE_VALUE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_NAME , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(NAME , result.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionV2WithPristneDataNotNullBrandTest() throws Exception {
    item.setSynchronized(true);
    item.setGeneratedItemName(NAME);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineBrand("ABCD");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    item.setPristineDataItem(pristineDataItem);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, this.itemCatalogs);
    Assertions.assertEquals(1.0, result.getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, result.getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, result.getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, result.getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0 , result.getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0 , result.getItemDetail().getDangerousLevel());
    Assertions.assertEquals(ATTRIBUTE_VALUE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_NAME , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(PRISTINE_PRODUCT_NAME , result.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionV2WithPristneDataNotNullModelTest() throws Exception {
    item.setSynchronized(true);
    item.setGeneratedItemName(NAME);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineModel("ABCD");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    item.setPristineDataItem(pristineDataItem);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, this.itemCatalogs);
    Assertions.assertEquals(1.0, result.getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, result.getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, result.getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, result.getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0 , result.getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0 , result.getItemDetail().getDangerousLevel());
    Assertions.assertEquals(ATTRIBUTE_VALUE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_NAME , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(PRISTINE_PRODUCT_NAME , result.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionV2WithPristneDataNotNullModelNotNullBrandTest() throws Exception {
    item.setSynchronized(true);
    item.setGeneratedItemName(NAME);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineModel("ABCD");
    pristineDataItem.setPristineBrand("ABCD");
    pristineDataItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    item.setPristineDataItem(pristineDataItem);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, this.itemCatalogs);
    Assertions.assertEquals(1.0, result.getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, result.getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, result.getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, result.getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0 , result.getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0 , result.getItemDetail().getDangerousLevel());
    Assertions.assertEquals(ATTRIBUTE_VALUE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_NAME , result.getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(PRISTINE_PRODUCT_NAME , result.getItemDetail().getProductName());
  }

  @Test
  public void convertToProductForTransactionV2WithPreOrderTest() throws Exception {
    product.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(), this.product.getMasterDataProduct()))
        .thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result =
        this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, this.itemCatalogs);
    assertEquals(PREORDER_VALUE, result.getItemDetail().getPreOrder().getPreOrderValue());
  }

  @Test
  public void convertToProductForTransactionV2WithNullItem() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product, null, this.itemCatalogs));
  }

  @Test
  public void convertToProductForTransactionV2WithNullItemCatalogs() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl.convertToProductForTransactionV2(this.product, this.item, null));
  }

  @Test
  public void convertToProductForTransactionV2WithNullProduct() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.objectConverterServiceImpl
        .convertToProductForTransactionV2(null, this.item, this.itemCatalogs));
  }

  @Test
  public void
  convertToProductForTransactionV2_product_getProductSpecialAttributes_containsGuaranteeAND_notContainsLamaGaransi()
      throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product3.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product3, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_TYPE, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void
  convertToProductForTransactionTestV2_product_getProductSpecialAttributes_containsLamaGaransiAND_notContainsGuarantee()
      throws Exception {

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product4.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product4, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTestV2_product_getProductSpecialAttributes_containsNull()
      throws Exception {
    this.product1.getProductSpecialAttributes().clear();
    this.product1.getProductSpecialAttributes().add(null);

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTestV2_product_getProductSpecialAttributes_getAttributeName_isNull()
      throws Exception {
    productSpecialAttribute1.setAttributeName(null);

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTestV2_product_getProductSpecialAttributes_isEmpty()
      throws Exception {
    this.product1.getProductSpecialAttributes().clear();
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTestV2_product_getProductSpecialAttributes_isNull()
      throws Exception {
    this.product1.setProductSpecialAttributes(null);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());

    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void
  convertToProductForTransactionTestV2_product_getProductSpecialAttributes_notContainsGaransiAndGuaranteeAndLamaGaransi()
      throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product2.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product2, this.item, this.itemCatalogs);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTestV2_product_getProductSpecialAttributes_notIsEmpty()
      throws Exception {
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
  }

  @Test
  public void convertToProductForTransactionTestV2_preorderTest()
      throws Exception {
    product1.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
    Assertions.assertTrue(result.getItemDetail().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductForTransactionTestV2_preorderFalseTest()
      throws Exception {
    preOrder.setIsPreOrder(false);
    product1.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
    Assertions.assertFalse(result.getItemDetail().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductForTransactionTestV2_preorderTrueExpiredDateTest()
      throws Exception {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(Constants.DATE);
    product1.setPreOrder(preOrder);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product1.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    ProductForTransactionVO result = this.objectConverterServiceImpl
        .convertToProductForTransactionV2(this.product1, this.item, this.itemCatalogs);
    assertEquals(GUARANTEE_INFO, result.getItemDetail().getWarrantyInfo());
    Assertions.assertFalse(result.getItemDetail().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToItemPickupPointChangeEventModelTest() {
    itemPickupPoint.setItemPickupPointDataChangeType(
        Collections.singletonList(ItemPickupPointChangeEventType.VARIANT_CHANGE.getName()));
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterServiceImpl.convertToItemPickupPointChangeEventModel(itemPickupPoint, null);
    Assertions.assertFalse(itemPickupPointDataChangeEventModel.isPureCNCStatusChange());
    Assertions.assertEquals(ItemPickupPointChangeEventType.VARIANT_CHANGE,
        itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes().get(0));
    verify(gdnMapper, times(this.itemViewConfigs.size())).deepCopy(any(), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
  }

  @Test
  public void convertToItemPickupPointChangeEventModelTrueTest() {
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(PRICE_1);
    itemPickupPoint.setB2bFields(b2bFields);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterServiceImpl.convertToItemPickupPointChangeEventModel(itemPickupPoint, true);
    Assertions.assertTrue(itemPickupPointDataChangeEventModel.isPureCNCStatusChange());
    verify(gdnMapper, times(this.itemViewConfigs.size())).deepCopy(any(), eq(com.gdn.x.product.domain.event.model.ItemViewConfig.class));
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2Test() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    B2bFields b2bFields = new B2bFields();
    b2bFields.setBasePrice(PRICE_1);
    b2bFields.setManaged(true);
    itemPickupPoint.setB2bFields(b2bFields);
    itemPickupPoint.setProductSku(PRODUCT_SKU_1);
    itemPickupPoints.add(itemPickupPoint);
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Set<String> productChannel = new HashSet<>();
    productChannel.add(Constants.B2B);
    productChannel.add(Constants.RETAIL);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertTrue(responses.get(1).getSimpleProduct().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(Constants.DAYS, responses.get(1).getSimpleProduct().getPreOrder().getPreOrderType());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
    Assertions.assertFalse(responses.get(0).getSimpleProduct().isHalalProduct());
    Assertions.assertEquals(responses.get(0).getSimpleItems().get(0).getPreferredSubscriptionType().size(), 2);
    Assertions.assertTrue(responses.get(0).getSimpleItems().get(0).getPreferredSubscriptionType().contains(MARKETPLACE));
    Assertions.assertEquals(responses.get(0).getSimpleProduct().getProductChannel().size(), 2);
    Assertions.assertEquals(responses.get(0).getSimpleProduct().getProductChannel(), productChannel);
    Assertions.assertEquals(ITEM_WEIGHT, responses.get(1).getSimpleItems().get(0).getShippingWeight(), 0.0);
    Assertions.assertEquals(ITEM_HEIGHT, responses.get(1).getSimpleItems().get(0).getHeight(), 0.0);
    Assertions.assertEquals(ITEM_WEIGHT, responses.get(1).getSimpleItems().get(0).getWeight(), 0.0);
    Assertions.assertEquals(ITEM_WIDTH, responses.get(1).getSimpleItems().get(0).getWidth(), 0.0);
    Assertions.assertEquals(ITEM_LENGTH, responses.get(1).getSimpleItems().get(0).getLength(), 0.0);
    Assertions.assertEquals(responses.get(1).getItemPickupPoints().get(0).getB2bFields(), b2bFields);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2ItemNullTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, null, itemPickupPoints, new HashMap<>(), masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2ItemPickupPointNullTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, null, new HashMap<>(), masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsIsSynchroniseV2Test() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(1).setSynchronized(true);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Product product1 = new Product();
    product1.setProductCode(TELEPHONE);
    Product product2 = new Product();
    product2.setProductCode(TELEPHONE);
    product2.setSynchronized(true);
    products.add(product1);
    products.add(product2);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts = new HashMap<>();
    mapOfMasterDataProducts.put(TELEPHONE, new SimpleMasterDataProductVO());
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, mapOfMasterDataProducts, masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsIsSynchroniseAllSalesV2Test() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(1).setSynchronized(true);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Product product1 = new Product();
    product1.setProductCode(TELEPHONE);
    SalesCatalog salesCatalog = new SalesCatalog();
    salesCatalog.setCatalogCode(Constants.SALES_CATEGORY_CATALOG_CODE);
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    salesCatalog.setListOfCategories(Collections.singletonList(category));
    product1.setSalesCatalogs(Collections.singletonList(salesCatalog));
    Product product2 = new Product();
    product2.setProductCode(TELEPHONE);
    product2.setSynchronized(true);
    products.add(product1);
    products.add(product2);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts = new HashMap<>();
    mapOfMasterDataProducts.put(TELEPHONE, new SimpleMasterDataProductVO());
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, mapOfMasterDataProducts, masterDataItemMap);
    Assertions.assertEquals(1, mapOfMasterDataProducts.get(TELEPHONE).getSalesCatalogs().size(), 0);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsIsSynchroniseB2bSalesV2Test() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(1).setSynchronized(true);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Product product1 = new Product();
    product1.setProductCode(TELEPHONE);
    SalesCatalog salesCatalog = new SalesCatalog();
    salesCatalog.setCatalogCode(B2B_SALES);
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    salesCatalog.setListOfCategories(Collections.singletonList(category));
    product1.setSalesCatalogs(Collections.singletonList(salesCatalog));
    Product product2 = new Product();
    product2.setProductCode(TELEPHONE);
    product2.setSynchronized(true);
    products.add(product1);
    products.add(product2);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts = new HashMap<>();
    mapOfMasterDataProducts.put(TELEPHONE, new SimpleMasterDataProductVO());
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    items.get(0).setMasterSku(MASTER_SKU);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, mapOfMasterDataProducts, masterDataItemMap);
    Assertions.assertEquals(1, mapOfMasterDataProducts.get(TELEPHONE).getSalesCatalogs().size(), 0);
    Assertions.assertEquals(MASTER_SKU, responses.get(0).getSimpleItems().get(0).getMasterSku());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsIsSynchroniseV2NewTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(1).setSynchronized(true);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Product product1 = new Product();
    product1.setProductCode(TELEPHONE);
    product1.setProductSku(PRODUCT_SKU_1);
    Product product2 = new Product();
    product2.setProductCode(TELEPHONE);
    product2.setSynchronized(true);
    products.add(product1);
    products.add(product2);
    Map<String, String> categoryCodeAndCategoryNameMap = new HashMap<>();
    categoryCodeAndCategoryNameMap.put(CATEGORY_CODE, CATEGORY_NAME);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts = new HashMap<>();
    mapOfMasterDataProducts.put(TELEPHONE, new SimpleMasterDataProductVO());
    mapOfMasterDataProducts.get(TELEPHONE).setSalesCatalogs(Arrays.asList(new SalesCatalog()));
    mapOfMasterDataProducts.get(TELEPHONE).setCategoryCodeAndCategoryNameMap(categoryCodeAndCategoryNameMap);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, mapOfMasterDataProducts, masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsIsSynchroniseV2CategoryCodeInItemNullNewTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    items.get(0).setCategoryCode(null);
    products.get(1).setSynchronized(true);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Product product1 = new Product();
    product1.setProductCode(TELEPHONE);
    product1.setProductSku(PRODUCT_SKU_1);
    Product product2 = new Product();
    product2.setProductCode(TELEPHONE);
    product2.setSynchronized(true);
    products.add(product1);
    products.add(product2);
    Map<String, String> categoryCodeAndCategoryNameMap = new HashMap<>();
    categoryCodeAndCategoryNameMap.put(CATEGORY_CODE, CATEGORY_NAME);
    Map<String, SimpleMasterDataProductVO> mapOfMasterDataProducts = new HashMap<>();
    mapOfMasterDataProducts.put(TELEPHONE, new SimpleMasterDataProductVO());
    mapOfMasterDataProducts.get(TELEPHONE).setSalesCatalogs(Arrays.asList(new SalesCatalog()));
    mapOfMasterDataProducts.get(TELEPHONE).setCategoryCodeAndCategoryNameMap(categoryCodeAndCategoryNameMap);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, mapOfMasterDataProducts, masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2ItemUpdateTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    items.get(2).setSynchronized(true);
    items.get(3).setSynchronized(true);
    items.get(3).setPristineDataItem(items.get(1).getPristineDataItem());
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2ItemPickupPointTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    itemPickupPoint.setProductSku(PRODUCT_SKU_2);
    itemPickupPoints.add(itemPickupPoint);
    itemPickupPoints.add(new ItemPickupPoint());
    itemPickupPoints.get(1).setProductSku(PRODUCT_SKU_1);
    itemPickupPoints.add(new ItemPickupPoint());
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    items = Arrays.asList(items.get(0));
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2ItemPickupPointFbbTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    itemPickupPoint.setProductSku(PRODUCT_SKU_2);
    itemPickupPoints.add(itemPickupPoint);
    itemPickupPoints.add(new ItemPickupPoint());
    itemPickupPoints.get(1).setProductSku(PRODUCT_SKU_1);
    itemPickupPoints.get(1).setFbbActivated(true);
    itemPickupPoints.add(new ItemPickupPoint());
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    items = Arrays.asList(items.get(0));
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(),
            masterDataItemMap);
    Assertions.assertTrue(this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(),
            masterDataItemMap).get(1).getItemPickupPoints().get(0).isFbbActivated());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2HalalFlagTrueTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    itemPickupPoint.setProductSku(PRODUCT_SKU_2);
    itemPickupPoints.add(itemPickupPoint);
    itemPickupPoints.add(new ItemPickupPoint());
    itemPickupPoints.get(1).setProductSku(PRODUCT_SKU_1);
    itemPickupPoints.get(1).setFbbActivated(true);
    itemPickupPoints.add(new ItemPickupPoint());
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    items = Arrays.asList(items.get(0));
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    products.get(0).setPreOrder(preOrder);
    products.get(0).setCurationStatus(CurationStatus.APPROVED);
    products.get(1).setCurationStatus(CurationStatus.APPROVED);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> result = this.objectConverterServiceImpl
      .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(),
        masterDataItemMap);
    assertTrue(result.get(0).getSimpleProduct().isHalalProduct());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2WithPreOrderDateGreaterThanCurrentDateTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(Constants.DATE);
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertTrue(responses.get(1).getSimpleProduct().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(Constants.DATE, responses.get(1).getSimpleProduct().getPreOrder().getPreOrderType());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2ProductScoreTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    products.get(0).setProductScore(new ProductScore());
    products.get(0).getProductScore().setTotalScore(90);
    preOrder.setPreOrderType(Constants.WEEK);
    products.get(0).setPreOrder(preOrder);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(categoryNamesResponse);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    Assertions.assertEquals(90,
        responses.get(1).getSimpleProduct().getProductScoreTotal(), 0.0);
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2NonPristineUnSyncTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap =
        createMasterDataItemAndResponses(products, items);
    items.get(1).setPristineDataItem(null);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints,
            new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNotNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2NonPristineImageFalseUnSyncTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap =
        createMasterDataItemAndResponses(products, items);
    items.get(1).setPristineDataItem(null);
    masterDataItemMap.get(ITEM_CODE_1).setMasterDataItemImages(null);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints,
            new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNotNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2_WhenMasterDataProductNullTest() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap =
        createMasterDataItemAndResponses(products, items);
    products.get(0).setSynchronized(true);
    products.get(1).setSynchronized(true);
    Map<String, SimpleMasterDataProductVO> masterDatProduct = new HashMap<String, SimpleMasterDataProductVO>();
    masterDatProduct.put(PRODUCT_CODE, null);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints,
            new HashMap<String, SimpleMasterDataProductVO>(), masterDataItemMap);
    Assertions.assertEquals(0, responses.size());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2TestNullMasterCatalog() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    productAndItemSolr.setMasterCatalog(null);
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
  }

  @Test
  public void convertAndValidateSimpleMasterDataExistsV2TestNullResponse() {
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    Map<String, SimpleMasterDataItemVO> masterDataItemMap = createMasterDataItemAndResponses(products, items);
    products.get(0).setProductSpecialAttributes(Arrays.asList(productSpecialAttribute1));
    Mockito.when(productAndItemSolrRepository.findOne(Mockito.anyString(), Mockito.anyString())).thenReturn(productAndItemSolr);
    Mockito.when(productCategoryBaseClient.getCategoryNames(Mockito.anyList())).thenReturn(null);
    List<SimpleProductAndItemsAndItemPickupPointV0> responses = this.objectConverterServiceImpl
        .convertAndValidateSimpleMasterDataExistsV2(products, items, itemPickupPoints, new HashMap<>(), masterDataItemMap);
    Assertions.assertEquals(PRODUCT_CODE, responses.get(0).getSimpleProduct().getProductCode());
    Assertions.assertEquals(PRODUCT_CODE, responses.get(1).getSimpleProduct().getProductCode());
    Assertions.assertEquals(2, responses.get(0).getSimpleItems().size());
    Assertions.assertEquals(2, responses.get(1).getSimpleItems().size());
    assertNull(responses.get(1).getSimpleItems().get(1).getSimpleAsyncMasterDataItem());
    Assertions.assertEquals(productSpecialAttribute1,
        responses.get(1).getSimpleProduct().getProductSpecialAttributes().get(0));
    assertFalse(responses.get(1).getSimpleProduct().isSynchronized());
    Assertions.assertEquals(2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(LOCATION_PATH1,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH2,
        responses.get(1).getSimpleProduct().getSimpleAsyncMasterDataProduct().getMasterDataProductImages().get(1)
            .getLocationPath());
    Assertions.assertEquals(2,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().size());
    Assertions.assertEquals(LOCATION_PATH3,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(0)
            .getLocationPath());
    Assertions.assertEquals(LOCATION_PATH4,
        responses.get(0).getSimpleItems().get(0).getSimpleAsyncMasterDataItem().getMasterDataItemImages().get(1)
            .getLocationPath());
  }

  @Test
  public void generateProductForTransactionVOWithoutMasterDataTest() {
    product.setItemCatalogs(itemCatalogs);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.item.getMasterDataItem(),
            this.product.getMasterDataProduct())).thenReturn(this.item.getMasterDataItem());
    List<ProductForTransactionVO> response =
        this.objectConverterServiceImpl.generateProductForTransactionVO(ImmutableSet.of(product.getProductSku()),
            Arrays.asList(new ProductAndItemsVO(product, Arrays.asList(item))), new HashMap<>());
    Assertions.assertEquals(ITEM_SKU, response.get(0).getItemSku());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.get(0).getItemDetail().getItemName());
    Assertions.assertEquals(MERCHANT_ID, response.get(0).getItemDetail().getMerchantCode());
    Assertions.assertEquals(MERCHANT_SKU,response.get(0).getItemDetail().getMerchantSku());
    Assertions.assertEquals(BRAND, response.get(0).getItemDetail().getBrandName());
    Assertions.assertEquals(PRODUCT_CODE, response.get(0).getItemDetail().getProductCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.get(0).getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
    Assertions.assertEquals(ITEM_ATTRIBUTE_VALUE,
        response.get(0).getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.get(0).getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(IMAGE_PATH, response.get(0).getItemDetail().getImageUrl());
  }

  @Test
  public void generateProductForTransactionVOWithMasterDataTest() {
    item.setSynchronized(true);
    product.setItemCatalogs(itemCatalogs);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(Arrays.asList(productAttributeDetail));
    List<ProductForTransactionVO> response =
        this.objectConverterServiceImpl.generateProductForTransactionVO(new HashSet<>(),
            Arrays.asList(new ProductAndItemsVO(product, Arrays.asList(item))), new HashMap<>());
    Assertions.assertEquals(1.0, response.get(0).getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, response.get(0).getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, response.get(0).getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, response.get(0).getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0, response.get(0).getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0, response.get(0).getItemDetail().getDangerousLevel());
    Assertions.assertEquals(ATTRIBUTE_VALUE, response.get(0).getItemDetail().getMasterDataItemAttributes().get(0).getAttributeValue());
    Assertions.assertEquals(ATTRIBUTE_CODE, response.get(0).getItemDetail().getMasterDataItemAttributes().get(0).getAttributeCode());
    Assertions.assertEquals(ATTRIBUTE_NAME, response.get(0).getItemDetail().getMasterDataItemAttributes().get(0).getAttributeName());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationNullValuesTest() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setManaged(true);
    b2bFieldsVo.setBasePrice(5.0);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    itemViewConfigSet.add(new ItemViewConfig());
    b2bFieldsVo.setB2bItemViewConfigs(itemViewConfigSet);
    itemActivationRequest.setB2bFields(b2bFieldsVo);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, new ItemPickupPoint(),
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationNullValuesB2bFieldsNullTest() {
    itemActivationRequest.setB2bFields(null);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, new ItemPickupPoint(),
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationTest() {
    ReflectionTestUtils.setField(objectConverterServiceImpl, "cncForWarehouseFeatureSwitch", true);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, itemPickupPoint,
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivation_withPickupPointDetailsTest() {
    ReflectionTestUtils.setField(objectConverterServiceImpl, "cncForWarehouseFeatureSwitch", true);
    BusinessPartnerPickupPoint businessPartnerPickupPoint =
        BusinessPartnerPickupPoint.builder().cncActivated(true).delivery(true).build();
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, itemPickupPoint,
        businessPartnerPickupPoint);
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationWholesalePriceActivatedTrueTest() {
    itemActivationRequest.setWholesalePriceActivated(true);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, itemPickupPoint,
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationWholesalePriceActivatedAndActivePromoBundlingsTest() {
    Set<String> promoBundlings = new HashSet<>();
    promoBundlings.add(PROMO_BUNDLING_TYPE_WHOLESALE);
    itemPickupPoint.setActivePromoBundlings(promoBundlings);
    itemActivationRequest.setWholesalePriceActivated(true);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, itemPickupPoint,
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationWholesalePriceActivatedFalseActivePromoBundlingsTest() {
    Set<String> promoBundlings = new HashSet<>();
    promoBundlings.add(PROMO_BUNDLING_TYPE_WHOLESALE);
    itemPickupPoint.setActivePromoBundlings(promoBundlings);
    itemActivationRequest.setWholesalePriceActivated(false);
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setManaged(true);
    b2bFieldsVo.setBasePrice(5.0);
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    itemViewConfigSet.add(new ItemViewConfig());
    b2bFieldsVo.setB2bItemViewConfigs(itemViewConfigSet);
    itemActivationRequest.setB2bFields(b2bFieldsVo);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, itemPickupPoint,
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void updateItemPickupPointOnNeedCorrectionActivationItemViewConfigB2bRequestEmptyTest() {
    B2bFieldsVo b2bFieldsVo = new B2bFieldsVo();
    b2bFieldsVo.setManaged(true);
    b2bFieldsVo.setBasePrice(5.0);
    itemActivationRequest.setB2bFields(b2bFieldsVo);
    objectConverterServiceImpl.updateItemPickupPointOnNeedCorrectionActivation(itemActivationRequest, itemPickupPoint,
        new BusinessPartnerPickupPoint());
  }

  @Test
  public void overrideDefiningAttributeDetailsFromL3ToL4NullProductAttributeTest() {
    product.setDefiningAttributes(null);
    objectConverterServiceImpl.overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), Arrays.asList(item));
    Assertions.assertEquals(item.getDefiningAttributes(), new ArrayList<>());
  }

  @Test
  public void overrideDefiningAttributeDetailsFromL3ToL4NullTest() {
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU);
    productAttributes.add(productAttribute);
    product.setDefiningAttributes(productAttributes);
    item.setItemSku(ITEM_SKU);
    objectConverterServiceImpl.overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), Arrays.asList(item));
    Assertions.assertEquals(item.getDefiningAttributes(), new ArrayList<>());
  }

  @Test
  public void overrideDefiningAttributeDetailsFromL3ToL4Test() {
    List<ProductAttribute> productAttributes = new ArrayList<>();
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeName(NAME);
    ProductAttribute productAttribute = new ProductAttribute();
    productAttribute.setItemSku(ITEM_SKU);
    productAttribute.setProductAttributeDetails(Arrays.asList(productAttributeDetail));
    productAttributes.add(productAttribute);
    product.setDefiningAttributes(productAttributes);
    item.setItemSku(ITEM_SKU);
    objectConverterServiceImpl.overrideDefiningAttributeDetailsFromL3ToL4(Arrays.asList(product), Arrays.asList(item));
    Assertions.assertEquals(item.getDefiningAttributes().get(0).getAttributeName(), productAttributeDetail.getAttributeName());
    Assertions.assertEquals(item.getDefiningAttributes(), Arrays.asList(productAttributeDetail));
  }

  @Test
  public void generateProductForTransactionVOWithEmptyMasterDataTest() {
    item.setSynchronized(true);
    product.setItemCatalogs(itemCatalogs);
    ProductAttributeDetail productAttributeDetail = new ProductAttributeDetail();
    productAttributeDetail.setAttributeValue(ATTRIBUTE_VALUE);
    productAttributeDetail.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetail.setAttributeName(ATTRIBUTE_NAME);
    item.setDefiningAttributes(new ArrayList<>());
    List<ProductForTransactionVO> response =
      this.objectConverterServiceImpl.generateProductForTransactionVO(new HashSet<>(),
        Arrays.asList(new ProductAndItemsVO(product, Arrays.asList(item))), new HashMap<>());
    Assertions.assertEquals(1.0, response.get(0).getItemDetail().getItemHeight().doubleValue(), 0);
    Assertions.assertEquals(2.0, response.get(0).getItemDetail().getItemWidth().doubleValue(), 0);
    Assertions.assertEquals(3.0, response.get(0).getItemDetail().getItemLength().doubleValue(), 0);
    Assertions.assertEquals(4.0, response.get(0).getItemDetail().getShippingWeight().doubleValue(), 0);
    Assertions.assertEquals(5.0, response.get(0).getItemDetail().getItemWeight().doubleValue(), 0);
    Assertions.assertEquals(0, response.get(0).getItemDetail().getDangerousLevel());
  }

  @Test
  public void convertItemPickupPointsAndProductToItemBasicDetailResponseTest() {
    itemPickupPoints = new ArrayList<>();
    Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
    ItemViewConfig itemViewConfig1 = new ItemViewConfig();
    itemViewConfig1.setChannel(Constants.DEFAULT);
    itemViewConfig1.setBuyable(BUYABLE);
    itemViewConfig1.setDiscoverable(DISCOVERABLE);
    itemViewConfigSet.add(itemViewConfig1);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setItemViewConfig(itemViewConfigSet);
    itemPickupPoint.setCncActive(CNC_ACTIVE);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoints.add(itemPickupPoint);
    Page<ItemPickupPoint> itemPickupPoints1 =
        new PageImpl<>(itemPickupPoints, PageRequest.of(0, 1), itemPickupPoints.size());
    Product product = new Product();
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setMerchantCode(MERCHANT_CODE);
    ItemBasicDetailResponse response =
        objectConverterServiceImpl.convertItemPickupPointsAndProductToItemBasicDetailResponse(itemPickupPoints1,
            product, ITEM_SKU);
    Assertions.assertEquals(MERCHANT_CODE, response.getMerchantCode());
    Assertions.assertEquals(ITEM_SKU, response.getItemSku());
    Assertions.assertEquals(2, response.getProductTypeCode());
    Assertions.assertEquals(BIG_PRODUCT, response.getProductTypeName());
    Assertions.assertEquals(ONLINE, response.isOnline());
    Assertions.assertFalse(response.getItemPickupPoints().getContent().get(0).isBuyable());
    Assertions.assertFalse(response.getItemPickupPoints().getContent().get(0).isDiscoverable());
  }

  @Test
  public void convertActivateNeedRevisionResponseListTest() {
    ActivateNeedRevisionResponse response =
        objectConverterServiceImpl.convertActivateNeedRevisionResponseList(true, null, null);
    Assertions.assertTrue(response.isCreateNew());
    assertNull(response.getNewlyAddedL5Responses());
  }

  @Test
  public void convertActivateNeedRevisionResponseListCreateNewFalseTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    item.setItemSku(ITEM_SKU);
    item.setItemCode(ITEM_CODE);
    item.setGeneratedItemName(GENERATED_ITEM_NAME);
    ActivateNeedRevisionResponse response =
        objectConverterServiceImpl.convertActivateNeedRevisionResponseList(false, Arrays.asList(item), Arrays.asList(itemPickupPoint));
    Assertions.assertFalse(response.isCreateNew());
    assertNotNull(response.getNewlyAddedL5Responses());
    Assertions.assertEquals(ITEM_SKU, response.getNewlyAddedL5Responses().get(0).getItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, response.getNewlyAddedL5Responses().get(0).getPickupPointCode());
    Assertions.assertEquals(ITEM_CODE, response.getNewlyAddedL5Responses().get(0).getItemCode());
    Assertions.assertEquals(GENERATED_ITEM_NAME, response.getNewlyAddedL5Responses().get(0).getItemName());
  }

  @Test
  public void convertToAdjustmentProductChangeTest() {
    AdjustmentProductChangeResponseVO changeResponseVO =
        AdjustmentProductChangeResponseVO.builder().priority(0).activated(Boolean.TRUE).productSku(ITEM_SKU)
            .pickupPointCode(PICKUP_POINT_CODE).endDate(END_DATE_TIME).startDate(START_DATE_TIME)
            .adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE).exclusiveProduct(true).build();
    AdjustmentProductChange productChange =
        AdjustmentProductChange.builder().adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE)
            .productSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).startDate(START_DATE_TIME).exclusiveProduct(true)
            .endDate(END_DATE_TIME).priority(0).build();
    when(gdnMapper.deepCopy(changeResponseVO, AdjustmentProductChange.class)).thenReturn(productChange);
    List<AdjustmentProductChange> adjustmentProductChanges =
        this.objectConverterServiceImpl.convertToAdjustmentProductChangeList(
            Collections.singletonList(changeResponseVO), Collections.emptyList());
    verify(gdnMapper).deepCopy(changeResponseVO, AdjustmentProductChange.class);
    Assertions.assertEquals(ITEM_SKU, adjustmentProductChanges.get(0).getProductSku());
    Assertions.assertEquals(CAMPAIGN_CODE, adjustmentProductChanges.get(0).getCampaignCode());
    Assertions.assertEquals(ADJUSTMENT_NAME, adjustmentProductChanges.get(0).getAdjustmentName());
    Assertions.assertEquals(PICKUP_POINT_CODE, adjustmentProductChanges.get(0).getPickupPointCode());
    Assertions.assertTrue(adjustmentProductChanges.get(0).isExclusiveProduct());
  }

  @Test
  public void convertToAdjustmentProductChangeV1Test(){
    AdjustmentProductResponse adjustmentProductResponse =
      AdjustmentProductResponse.builder().adjustmentName(ADJUSTMENT_NAME).startDate(START_DATE_TIME)
        .endDate(END_DATE_TIME).campaignCode(CAMPAIGN_CODE).campaignCode(CAMPAIGN_CODE).priority(0)
        .build();
    AdjustmentProductChange productChange =
      AdjustmentProductChange.builder().adjustmentName(ADJUSTMENT_NAME).campaignCode(CAMPAIGN_CODE)
        .productSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).startDate(START_DATE_TIME)
        .endDate(END_DATE_TIME).priority(0).build();
    when(gdnMapper.deepCopy(adjustmentProductResponse, AdjustmentProductChange.class))
      .thenReturn(productChange);
    List<AdjustmentProductChange> adjustmentProductChanges =
      this.objectConverterServiceImpl.convertToAdjustmentProductChangeList(
        Collections.emptyList(), Collections.singletonList(adjustmentProductResponse));
    verify(gdnMapper).deepCopy(adjustmentProductResponse,AdjustmentProductChange.class);
    Assertions.assertEquals(ITEM_SKU,adjustmentProductChanges.get(0).getProductSku());
    Assertions.assertEquals(CAMPAIGN_CODE,adjustmentProductChanges.get(0).getCampaignCode());
    Assertions.assertEquals(ADJUSTMENT_NAME,adjustmentProductChanges.get(0).getAdjustmentName());
    Assertions.assertEquals(PICKUP_POINT_CODE,adjustmentProductChanges.get(0).getPickupPointCode());
  }

  @Test
  public void convertProductToDuplicateProductDetailsResponseSuspendedTest(){
    product.setSuspended(true);
    DuplicateProductDetailsResponse duplicateProductDetailsResponse =
        objectConverterServiceImpl.convertProductToDuplicateProductDetailsResponse(product);
    Assertions.assertEquals(duplicateProductDetailsResponse.getStatus(), Constants.SUSPENDED);
  }

  @Test
  public void convertProductToDuplicateProductDetailsResponseArchivedTest(){
    product.setArchived(true);
    DuplicateProductDetailsResponse duplicateProductDetailsResponse =
        objectConverterServiceImpl.convertProductToDuplicateProductDetailsResponse(product);
    Assertions.assertEquals(duplicateProductDetailsResponse.getStatus(), Constants.ARCHIVED);
  }

  @Test
  public void convertProductToDuplicateProductDetailsResponseActiveTest(){
    DuplicateProductDetailsResponse duplicateProductDetailsResponse =
        objectConverterServiceImpl.convertProductToDuplicateProductDetailsResponse(product);
    Assertions.assertEquals(duplicateProductDetailsResponse.getStatus(), Constants.ACTIVE);
  }

  public void constructProductAndItemsForView_ForSynchronizedFalseTest() throws Exception {
    this.item.setSynchronized(Boolean.FALSE);
    dbItemMap.put(ITEM_SKU, item);
    product.setMasterCatalog(new MasterCatalog(CATALOG_CODE,new Category()));
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(
      imageResponses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class))
      .thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    when(gdnMapper.deepCopy(new MasterCatalog(CATALOG_CODE,new Category()),
        MasterCatalogDTO.class))
      .thenReturn(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO()));
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
      this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap,
        itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse,MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item,ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(gdnMapper).deepCopy(new MasterCatalog(CATALOG_CODE,new Category()),
      MasterCatalogDTO.class);
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueTest() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    item.setMerchantCode(MERCHANT_CODE);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
      ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
        .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses =
      Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(
      imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE),
      false, false)).thenReturn(
      responses);
    businessPartnerPromo.setBusinessPartnerCode(MERCHANT_CODE);
    when(businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(STORE_ID,
        Collections.singletonList(MERCHANT_CODE))).thenReturn(Collections.singletonList(businessPartnerPromo));
    when(gdnMapper.deepCopy(this.item, ItemResponse.class))
      .thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
      this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap,
        itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse,MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item,ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient).findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE),false,false);
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueTest() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    this.item.setPreferredSubscriptionType(Collections.emptySet());
    item.setMerchantCode(MERCHANT_CODE);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertTrue(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueNotEmptyTest() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    item.setMerchantCode(MERCHANT_CODE);
    Set<String> set = new HashSet<>(Arrays.asList(WAREHOUSE, MARKETPLACE));
    this.item.setPreferredSubscriptionType(set);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertTrue(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueSize1Test() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    item.setMerchantCode(MERCHANT_CODE);
    Set<String> set = new HashSet<>(Arrays.asList(ITEM_CODE));
    this.item.setPreferredSubscriptionType(set);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertTrue(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueSize1WHTest() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    item.setMerchantCode(MERCHANT_CODE);
    Set<String> set = new HashSet<>(Arrays.asList(WAREHOUSE));
    this.item.setPreferredSubscriptionType(set);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertFalse(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueNotEmpty_Test() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    item.setMerchantCode(MERCHANT_CODE);
    Set<String> set = new HashSet<>(Arrays.asList(WAREHOUSE, ITEM_CODE));
    this.item.setPreferredSubscriptionType(set);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertTrue(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueSize1MKTTest() throws Exception {
    itemPickupPointMap.get(ITEM_SKU).setFbbActivated(false);
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    item.setMerchantCode(MERCHANT_CODE);
    Set<String> set = new HashSet<>(Arrays.asList(MARKETPLACE));
    this.item.setPreferredSubscriptionType(set);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertTrue(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueSubscribableTrueSize1MKTFbbTrueTest()
      throws Exception {
    itemPickupPointMap.get(ITEM_SKU).setFbbActivated(true);
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    this.item.setSubscribable(true);
    item.setMerchantCode(MERCHANT_CODE);
    Set<String> set = new HashSet<>(Arrays.asList(MARKETPLACE));
    this.item.setPreferredSubscriptionType(set);
    dbItemMap.put(ITEM_SKU, item);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
        ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
            .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses = Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false))
        .thenReturn(responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class)).thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(itemHelperService.processDiscountPricesByPriority(discountPriceList)).thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
        this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap, itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse, MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item, ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient)
        .findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE), false, false);
    Assertions.assertFalse(productAndItemsSummaryResponseV2.getItemDetailResponsesV2().get(0).isSubscribable());
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedFalseWithNullViewConfigsTest() throws Exception {
    this.item.setSynchronized(Boolean.FALSE);
    item.setMasterDataItem(null);
    this.item.setPristineDataItem(pristineDataItem);
    item.setMerchantCode(MERCHANT_CODE);
    dbItemMap.put(ITEM_SKU, item);
    this.itemPickupPoint.setItemViewConfig(Collections.emptySet());
    product.setMasterCatalog(new MasterCatalog(CATALOG_CODE,new Category()));
    this.product.setSalesCatalogs(null);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(
      imageResponses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class))
      .thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    when(gdnMapper.deepCopy(new MasterCatalog(CATALOG_CODE,new Category()),
      MasterCatalogDTO.class))
      .thenReturn(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO()));
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
      this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap,
        itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse,MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item,ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(gdnMapper).deepCopy(new MasterCatalog(CATALOG_CODE,new Category()),
      MasterCatalogDTO.class);
  }

  @Test
  public void constructProductAndItemsForView_ForSynchronizedTrueWithUpcNullTest() throws Exception {
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.getMasterDataItem().setMasterDataItemImages(null);
    item.setMerchantCode(MERCHANT_CODE);
    this.image.setSequence(1);
    dbItemMap.put(ITEM_SKU, item);
    SalesCatalog salesCatalog = new SalesCatalog();
    salesCatalog.setCatalogCode(CATALOG_CODE);
    this.product.setSalesCatalogs(Collections.singletonList(salesCatalog));
    this.itemPickupPoint.setItemViewConfig(Collections.emptySet());
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(null);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    productItemDetailResponse.setDangerousGoodsLevel(1);
    productItemDetailResponse.setImages(Collections.singletonList(image));
    item.setDefiningAttributes(Collections.singletonList(
      ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
        .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses =
      Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(
      imageResponses);
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE),
      false, false)).thenReturn(
      responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class))
      .thenReturn(itemResponse);
    when(gdnMapper.deepCopy(salesCatalog,SalesCatalogDTO.class)).thenReturn(new SalesCatalogDTO());
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
      this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap,
        itemPickupPointMap);
    verify(gdnMapper).deepCopy(imageResponse,MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item,ItemResponse.class);
    verify(gdnMapper).deepCopy(salesCatalog,SalesCatalogDTO.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(itemHelperService).processDiscountPricesByPriority(discountPriceList);
    verify(productCategoryBaseClient).findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE),false,false);
  }

  @Test
  public void constructProductAndItemsForView_withEmptyImageResponse() throws Exception {
    this.masterDataItemImage = null;
    this.item.setSynchronized(Boolean.TRUE);
    this.item.setItemCode(ITEM_CODE);
    this.item.setMasterDataItem(null);
    item.setMerchantCode(MERCHANT_CODE);
    dbItemMap.put(ITEM_SKU, item);
    this.itemPickupPointMap.values()
      .forEach(itemPickupPoint1 -> itemPickupPoint1.setItemViewConfig(new HashSet<>()));
    this.itemPickupPointMap.values()
      .forEach(itemPickupPoint1 -> itemPickupPoint1.setPrice(new HashSet<>()));
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    productItemDetailResponse.setGeneratedItemName(GENERATED_ITEM_NAME);
    productItemDetailResponse.setUpcCode(CODE);
    productItemDetailResponse.setSkuCode(ITEM_CODE);
    item.setDefiningAttributes(Collections.singletonList(
      ProductAttributeDetail.builder().attributeCode(ATTRIBUTE_CODE).attributeName(ATTRIBUTE_NAME)
        .attributeValue(ATTRIBUTE_VALUE).build()));
    List<ProductItemDetailResponse> responses =
      Collections.singletonList(productItemDetailResponse);
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(
      Collections.emptyList());
    when(productCategoryBaseClient.findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE),
      false, false)).thenReturn(
      responses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class))
      .thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
      this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap,
        itemPickupPointMap);
    verify(gdnMapper).deepCopy(item,ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(productCategoryBaseClient).findProductItemDetailsBySkuCodes(Collections.singletonList(ITEM_CODE),false,false);
  }

  @Test
  public void constructProductAndItemsForView_withWrongItemPickupPoint() throws Exception {
    this.item.setSynchronized(Boolean.FALSE);
    item.setMasterDataItem(null);
    this.item.setPristineDataItem(pristineDataItem);
    item.setMerchantCode(MERCHANT_CODE);
    dbItemMap.put(ITEM_SKU, item);
    this.itemPickupPoint.setItemViewConfig(Collections.emptySet());
    this.itemPickupPoint.setItemSku(ITEM_SKU_1);
    product.setMasterCatalog(new MasterCatalog(CATALOG_CODE,new Category()));
    when(this.productCategoryBaseClient.getProductImagesByProductCode(PRODUCT_CODE)).thenReturn(
      imageResponses);
    when(gdnMapper.deepCopy(this.item, ItemResponse.class))
      .thenReturn(itemResponse);
    when(gdnMapper.deepCopy(this.imageResponse, MasterDataProductImageDTO.class)).thenReturn(masterDataProductImageDTO);
    Mockito.when(
        itemHelperService.processDiscountPricesByPriority(discountPriceList))
      .thenReturn(discountPrice);
    when(gdnMapper.deepCopy(new MasterCatalog(CATALOG_CODE,new Category()),
      MasterCatalogDTO.class))
      .thenReturn(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO()));
    ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2 =
      this.objectConverterServiceImpl.constructProductAndItemsForView(product, dbItemMap,
        new HashMap<>());
    verify(gdnMapper).deepCopy(imageResponse,MasterDataProductImageDTO.class);
    verify(gdnMapper).deepCopy(item,ItemResponse.class);
    verify(productCategoryBaseClient).getProductImagesByProductCode(PRODUCT_CODE);
    verify(businessPartnerPromoService)
        .findByStoreIdAndBusinessPartnerList(STORE_ID, Collections.singletonList(MERCHANT_CODE));
    verify(gdnMapper).deepCopy(new MasterCatalog(CATALOG_CODE,new Category()),
      MasterCatalogDTO.class);
  }

  @Test
  public void convertItemToLevel4SummaryResponseItemSkuAndFbbActivatedTest() throws Exception {
    Item item1 = new Item(ObjectConverterServiceImplTest.ITEM_SKU_1, ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(NAME);
    masterDataItem.setUpcCode(CODE);
    item1.setMasterDataItem(masterDataItem);
    item1.setSynchronized(Boolean.FALSE);
    item1.setItemSku(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    items.add(item1);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(ObjectConverterServiceImplTest.ITEM_SKU_1);
    itemLevel4ListingResponse1.setProductSku(ObjectConverterServiceImplTest.PRODUCT_SKU_1);
    itemPickupPoints.get(0).setItemSku(ITEM_SKU_1);
    itemPickupPoint1.setFbbActivated(true);
    itemPickupPoint1.setItemSku(ITEM_SKU_1);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPoints.add(itemPickupPoint1);
    when(itemSummaryUtil.getProductCodeFromItemCode(Mockito.anyString())).thenReturn(PRODUCT_CODE);
    when(gdnMapper.deepCopy(item1, ItemLevel4ListingResponse.class)).thenReturn(itemLevel4ListingResponse1);
    List<ItemLevel4ListingResponse> level4ListingResponseList = objectConverterServiceImpl
      .convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, itemPickupPoints, new HashMap<>() );
    assertNotNull(item1);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(level4ListingResponseList));
    verify(gdnMapper).deepCopy(item1, ItemLevel4ListingResponse.class);
    assertNotNull(items.get(0));
    assertNotNull(level4ListingResponseList.get(0));
    Assertions.assertEquals(NAME, level4ListingResponseList.get(0).getGeneratedItemName());
    Assertions.assertEquals(PICKUP_POINT_CODE, level4ListingResponseList.get(0).getPickupPointCodes().get(0));
    Assertions.assertEquals(PICKUP_POINT_CODE_2,
      level4ListingResponseList.get(0).getFbbPickupPointCodes().get(0));
  }

  @Test
  public void toProductScoreVoFromProductScoreTest() throws Exception {
    ProductScore productScore = new ProductScore();
    when(gdnMapper.deepCopy(productScore, ProductScoreVo.class)).thenReturn(new ProductScoreVo());
    ProductScoreVo productScoreVo = objectConverterServiceImpl.toProductScoreVoFromProductScore(productScore);
    verify(gdnMapper).deepCopy(productScore, ProductScoreVo.class);
    assertNotNull(productScoreVo);
  }

  @Test
  public void toCombinedEditItemResponseTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    when(gdnMapper.deepCopy(editItemResponse, CombinedEditItemResponse.class)).thenReturn(
        new CombinedEditItemResponse());
    CombinedEditItemResponse combinedEditItemResponse =
        objectConverterServiceImpl.toCombinedEditItemResponse(editItemResponse);
    verify(gdnMapper).deepCopy(editItemResponse, CombinedEditItemResponse.class);
    assertNotNull(combinedEditItemResponse);
  }

  @Test
  public void toCombinedEditItemResponseNotEmptyTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setUpdatedItems(Arrays.asList(item));
    editItemResponse.setUpdatedItemPickupPoints(Arrays.asList(itemPickupPoint));
    editItemResponse.setAllUpdatedItemPickupPoints(Arrays.asList(itemPickupPoint));
    when(gdnMapper.deepCopy(editItemResponse, CombinedEditItemResponse.class)).thenReturn(
        new CombinedEditItemResponse());
    when(gdnMapper.deepCopy(item, ItemVo.class)).thenReturn(
        new ItemVo());
    when(gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class)).thenReturn(
        new ItemPickupPointVo());
    CombinedEditItemResponse combinedEditItemResponse =
        objectConverterServiceImpl.toCombinedEditItemResponse(editItemResponse);
    verify(gdnMapper).deepCopy(item, ItemV2.class);
    verify(gdnMapper).deepCopy(editItemResponse, CombinedEditItemResponse.class);
    verify(gdnMapper, times(2)).deepCopy(itemPickupPoint, ItemPickupPointVo.class);
    assertNotNull(combinedEditItemResponse);
  }

  @Test
  public void toSalesCategoryHierarchy() {
    Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache = new HashMap<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    parentCategoriesFromDbAndCache.put(CATEGORY_CODE, Collections.singletonList(categoryResponse));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setSalesCategoryCodes(Collections.singletonList(CATEGORY_CODE));
    List<ProductBasicResponse> productBasicResponseList = new ArrayList<>();
    productBasicResponseList.add(productBasicResponse);
    objectConverterServiceImpl.toSalesCategoryHierarchy(productBasicResponseList, parentCategoriesFromDbAndCache);
    Assertions.assertEquals(CATEGORY_CODE, productBasicResponseList.get(0).getSalesCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_CODE,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getCategoryCode());
  }

  @Test
  public void toSalesCategoryHierarchyMultipleLevels() {
    Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache = new HashMap<>();
    CategoryResponse categoryResponseCn = new CategoryResponse();
    categoryResponseCn.setCategoryCode(CATEGORY_CODE_CN);
    CategoryResponse categoryResponseC1 = new CategoryResponse();
    categoryResponseC1.setCategoryCode(CATEGORY_CODE_C1);
    parentCategoriesFromDbAndCache.put(CATEGORY_CODE_CN, Arrays.asList(categoryResponseCn, categoryResponseC1));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setSalesCategoryCodes(Collections.singletonList(CATEGORY_CODE_CN));
    List<ProductBasicResponse> productBasicResponseList = new ArrayList<>();
    productBasicResponseList.add(productBasicResponse);
    objectConverterServiceImpl.toSalesCategoryHierarchy(productBasicResponseList, parentCategoriesFromDbAndCache);
    Assertions.assertEquals(CATEGORY_CODE_CN, productBasicResponseList.get(0).getSalesCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_CODE_CN,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_CN,
        productBasicResponseList.get(0).getSalesCategoryHierarchy().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_CN,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(0)
            .getCategoryCode());
    Assertions.assertEquals(2,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(0).getLevel());
    Assertions.assertEquals(CATEGORY_CODE_C1,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(1)
            .getCategoryCode());
    Assertions.assertEquals(1,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(1).getLevel());
  }

  @Test
  public void toSalesCategoryHierarchyMultipleLevelsMultipleCategory() {
    Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache = new HashMap<>();
    CategoryResponse categoryResponseCn = new CategoryResponse();
    categoryResponseCn.setCategoryCode(CATEGORY_CODE_CN);
    CategoryResponse categoryResponseC1 = new CategoryResponse();
    categoryResponseC1.setCategoryCode(CATEGORY_CODE_C1);
    parentCategoriesFromDbAndCache.put(CATEGORY_CODE_CN, Arrays.asList(categoryResponseCn, categoryResponseC1));
    CategoryResponse categoryResponseCn2 = new CategoryResponse();
    categoryResponseCn2.setCategoryCode(CATEGORY_CODE_CN_2);
    parentCategoriesFromDbAndCache.put(CATEGORY_CODE_CN_2, Collections.singletonList(categoryResponseCn2));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    productBasicResponse.setSalesCategoryCodes(Arrays.asList(CATEGORY_CODE_CN, CATEGORY_CODE_CN_2));
    List<ProductBasicResponse> productBasicResponseList = new ArrayList<>();
    productBasicResponseList.add(productBasicResponse);
    objectConverterServiceImpl.toSalesCategoryHierarchy(productBasicResponseList, parentCategoriesFromDbAndCache);
    Assertions.assertEquals(CATEGORY_CODE_CN, productBasicResponseList.get(0).getSalesCategoryCodes().get(0));
    Assertions.assertEquals(CATEGORY_CODE_CN,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_CN,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(0)
            .getCategoryCode());
    Assertions.assertEquals(2,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(0).getLevel());
    Assertions.assertEquals(CATEGORY_CODE_C1,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(1)
            .getCategoryCode());
    Assertions.assertEquals(1,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(0).getItemCategories().get(1).getLevel());
    Assertions.assertEquals(1,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(1).getItemCategories().get(0).getLevel());
    Assertions.assertEquals(CATEGORY_CODE_CN_2,
        productBasicResponseList.get(0).getSalesCategoryHierarchyV2().get(1).getItemCategories().get(0)
            .getCategoryCode());
  }

  @Test
  public void toSalesCategoryHierarchyBaseResponseEmpty() {
    Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache = new HashMap<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(CATEGORY_CODE);
    parentCategoriesFromDbAndCache.put(CATEGORY_CODE, Collections.singletonList(categoryResponse));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    List<ProductBasicResponse> productBasicResponseList = new ArrayList<>();
    productBasicResponseList.add(productBasicResponse);
    objectConverterServiceImpl.toSalesCategoryHierarchy(productBasicResponseList, parentCategoriesFromDbAndCache);
    Assertions.assertTrue(productBasicResponseList.get(0).getSalesCategoryCodes().isEmpty());
  }

  @Test
  public void convertToOdooCreationEventModelTest(){
      Product product = new Product();
      product.setMerchantCode("123");
      product.setProductCode("P123");
      product.setProductSku("SKU123");
      product.setMarkForDelete(false);
      product.setBrand("BrandA");
      product.setCategoryCode("Category1");
      product.setPreOrder(new PreOrder());
      ItemVo itemVo = new ItemVo();
      itemVo.setItemSku("ITEM123");
      itemVo.setGeneratedItemName("Item Name");
      itemVo.setMerchantSku("MSKU123");
      itemVo.setUpcCode("UPC123");
      ItemPickupPointVo pickupPointVo = new ItemPickupPointVo();
      pickupPointVo.setPickupPointCode("PP123");
      Price price = new Price();
      price.setListPrice(100.0);
      pickupPointVo.setPrice(new HashSet<>(Arrays.asList(price)));
      itemVo.setItemPickupPointVoList(Collections.singletonList(pickupPointVo));
      List<ItemVo> items = Collections.singletonList(itemVo);
      OdooCreationEventModel odoo = objectConverterServiceImpl.convertToOdooCreationEventModel(product, items);
    Assertions.assertNotNull(odoo);
  }

  @Test
  public void convertToOdooCreationEventModelPriceEmptyTest(){
    Product product = new Product();
    product.setMerchantCode("123");
    product.setProductCode("P123");
    product.setProductSku("SKU123");
    product.setMarkForDelete(false);
    product.setBrand("BrandA");
    product.setCategoryCode("Category1");
    product.setPreOrder(new PreOrder());
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku("ITEM123");
    itemVo.setGeneratedItemName("Item Name");
    itemVo.setMerchantSku("MSKU123");
    itemVo.setUpcCode("UPC123");
    ItemPickupPointVo pickupPointVo = new ItemPickupPointVo();
    pickupPointVo.setPickupPointCode("PP123");
    Price price = new Price();
    price.setListPrice(100.0);
    pickupPointVo.setPrice(Collections.emptySet());
    itemVo.setItemPickupPointVoList(Collections.singletonList(pickupPointVo));
    List<ItemVo> items = Collections.singletonList(itemVo);
    OdooCreationEventModel odoo = objectConverterServiceImpl.convertToOdooCreationEventModel(product, items);
    Assertions.assertNotNull(odoo);
  }

  @Test
  public void convertToOdooCreationEventModelItemsNullTest(){
    OdooCreationEventModel odoo = objectConverterServiceImpl.convertToOdooCreationEventModel(product, null);
    Assertions.assertNotNull(odoo);
  }

  @Test
  public void convertToOdooCreationEventModelPickupPointsNullTest() {
    Product product = new Product();
    product.setMerchantCode("123");
    product.setProductCode("P123");
    product.setProductSku("SKU123");
    product.setMarkForDelete(false);
    product.setBrand("BrandA");
    product.setCategoryCode("Category1");
    product.setPreOrder(new PreOrder());
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku("ITEM123");
    itemVo.setGeneratedItemName("Item Name");
    itemVo.setMerchantSku("MSKU123");
    itemVo.setUpcCode("UPC123");
    itemVo.setItemPickupPointVoList(null);
    List<ItemVo> items = Collections.singletonList(itemVo);
    OdooCreationEventModel odoo = objectConverterServiceImpl.convertToOdooCreationEventModel(product, items);
    Assertions.assertNotNull(odoo);
  }

  @Test
  public void convertToOdooCreationWithImeiEventModelPickupPointsNullTest() {
    ReflectionTestUtils.setField(objectConverterServiceImpl,"imeiAttributeCode",IMEI_ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(objectConverterServiceImpl,"imeiAllowedValues", IMEI_ATTRIBUTE_VALUES);
    Product product = new Product();
    product.setMerchantCode(MERCHANT_CODE);
    product.setProductCode(PRODUCT_CODE);
    product.setProductSku(PRODUCT_SKU);
    product.setMarkForDelete(false);
    product.setBrand(BRAND);
    product.setCategoryCode(CATEGORY_CODE);
    product.setPreOrder(new PreOrder());
    ProductSpecialAttribute specialAttribute = new ProductSpecialAttribute();
    specialAttribute.setAttributeCode(IMEI_ATTRIBUTE_CODE);
    specialAttribute.setAttributeValue(IMEI_ATTRIBUTE_VALUE);
    ProductSpecialAttribute specialAttribute1 = new ProductSpecialAttribute();
    specialAttribute1.setAttributeCode(IMEI_ATTRIBUTE_CODE);
    specialAttribute1.setAttributeValue(IMEI_ATTRIBUTE_VALUE);
    product.setProductSpecialAttributes(List.of(specialAttribute, specialAttribute1));
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku(ITEM_SKU);
    itemVo.setGeneratedItemName(GENERATED_ITEM_NAME);
    itemVo.setMerchantSku(MERCHANT_SKU);
    itemVo.setUpcCode(UPC_CODE);
    itemVo.setItemPickupPointVoList(null);
    List<ItemVo> items = Collections.singletonList(itemVo);
    OdooCreationEventModel odoo = objectConverterServiceImpl.convertToOdooCreationEventModel(product, items);
    Assertions.assertNotNull(odoo);
    Assertions.assertTrue(odoo.isImeiRequired());
  }
}
