package com.gdn.x.product.rest.web.helper.impl.test;

import static java.util.stream.Collectors.toSet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.io.File;
import java.math.BigDecimal;
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
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.model.ItemAndPickupPointBasicDetailVo;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.model.vo.ItemPickupPointUpdateRequestVo;
import com.gdn.x.product.rest.web.model.dto.DistributionInfoDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRequest;
import com.gdn.x.product.rest.web.model.request.AddVariantRequest;
import com.gdn.x.product.rest.web.model.request.B2bFields;
import com.gdn.x.product.rest.web.model.request.BuyableScheduleRequest;
import com.gdn.x.product.rest.web.model.request.DiscoverableScheduleRequest;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointActivationRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointDeleteRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.VideoAddEditRequest;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.PickupPointDetailResponse;
import org.apache.commons.beanutils.BeanUtils;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.Page;
import org.springframework.test.util.ReflectionTestUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.InventoryType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataItemImage;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.MasterDataProductAttribute;
import com.gdn.x.product.model.entity.MasterDataProductAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProductImage;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PreOrder;
import com.gdn.x.product.model.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ActiveProductDetailVo;
import com.gdn.x.product.model.vo.ActiveProductsRequestVO;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ComboDetailVo;
import com.gdn.x.product.model.vo.ComboResponseVO;
import com.gdn.x.product.model.vo.HandlingFeeRequest;
import com.gdn.x.product.model.vo.HandlingFeeResponse;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemNameSkuVO;
import com.gdn.x.product.model.vo.ItemPickupPointListingUpdateRequestVo;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemSummaryPageResponseVo;
import com.gdn.x.product.model.vo.ItemSummaryResponseVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemResponseVo;
import com.gdn.x.product.model.vo.MasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.MasterDataWithProductItemsVo;
import com.gdn.x.product.model.vo.OfficialStoreRequestVO;
import com.gdn.x.product.model.vo.OfflineItemPriceVO;
import com.gdn.x.product.model.vo.PreOrderVO;
import com.gdn.x.product.model.vo.PristineMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductDetailVo;
import com.gdn.x.product.model.vo.ProductForTransactionVO;
import com.gdn.x.product.model.vo.ProductItemDetailVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.model.vo.SimplePristineProductRequestVo;
import com.gdn.x.product.model.vo.SimpleProductRequestVo;
import com.gdn.x.product.model.vo.UpdateItemSummaryRequestVo;
import com.gdn.x.product.model.vo.WholesaleVO;
import com.gdn.x.product.rest.web.model.ItemDetailRestWeb;
import com.gdn.x.product.rest.web.model.SystemParameterRequest;
import com.gdn.x.product.rest.web.model.SystemParameterResponse;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.ItemCategoryDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemAttributeValueDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.PristineDataItemDto;
import com.gdn.x.product.rest.web.model.dto.PristineProductAndItemsDTO;
import com.gdn.x.product.rest.web.model.dto.PristineProductResponse;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemsDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductDTO;
import com.gdn.x.product.rest.web.model.dto.ProductItemDetailDTO;
import com.gdn.x.product.rest.web.model.dto.SalesCatalogDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.dto.SimpleProductsAndItemsResponse;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ActiveComboRequest;
import com.gdn.x.product.rest.web.model.request.ActiveProductRequest;
import com.gdn.x.product.rest.web.model.request.AllowedAttributeValueRequest;
import com.gdn.x.product.rest.web.model.request.AttributeRequest;
import com.gdn.x.product.rest.web.model.request.HandlingFeeRequestRestWeb;
import com.gdn.x.product.rest.web.model.request.ItemListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigAndItemSkuRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.MasterCatalogRequest;
import com.gdn.x.product.rest.web.model.request.OfflineItemRequest;
import com.gdn.x.product.rest.web.model.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemsRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeRequest;
import com.gdn.x.product.rest.web.model.request.ProductAttributeValueRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.QuickEditUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCatalogRequest;
import com.gdn.x.product.rest.web.model.request.SimpleProductListRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.UpsertOfflineItemRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ComboDetailResponse;
import com.gdn.x.product.rest.web.model.response.ComboResponse;
import com.gdn.x.product.rest.web.model.response.HandlingFeeResponseRestWeb;
import com.gdn.x.product.rest.web.model.response.ItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailPageResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemSummaryPageResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemResponse;
import com.gdn.x.product.rest.web.model.response.MasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;
import com.gdn.x.product.rest.web.model.response.PristineItemResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailResponse;
import com.gdn.x.product.rest.web.model.response.PristineMasterDataDetailWithProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductForTransactionResponse;
import com.gdn.x.product.rest.web.model.response.ProductInfoResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import com.gdn.x.product.rest.web.model.response.SimpleItemResponse;
import com.gdn.x.product.rest.web.model.response.SimplePristineProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductListResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductResponse;
import com.gdn.x.product.rest.web.model.response.SizeChartDataColumn;
import com.gdn.x.product.rest.web.model.response.SizeChartDataRow;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.rest.web.model.response.WholesaleResponse;
import com.gdn.x.product.service.api.AllowedAttributeValuesService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ProductAndItemSolrConstructorService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.api.util.ListUtil;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.GdnMapperHelper;
import com.gdn.x.product.service.util.ModelConverterImpl;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.google.common.collect.ImmutableSet;

public class ModelConverterImplTest {

  private static final String DEFAULT_CHANNEL = "DEFAULT";
  private static final String CHANNEL = "CHANNEL";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String ITEM_CODE = "itemCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final boolean IS_SYNCHRONIZED = false;
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PICKUP_POINT_CODE_1 = "pickupPointCode1";
  private static final String PICKUP_POINT_NAME = "pickupPointName";
  private static final String PICKUP_POINT_NAME_1 = "pickupPointName1";
  private static final String BRAND = "brand";
  private static final String PRODUCT_CATENTRY_ID = "productCatentryId";
  private static final String TICKET_TEMPLATE_CODE = "ticketTemplateCode";
  private static final Double LENGTH = 0.0;
  private static final Double WIDTH = 0.0;
  private static final Double WEIGHT = 0.0;
  private static final Double HEIGHT = 0.0;
  private static final Double SHIPPING_WEIGHT = 0.0;
  private static final String MERCHANT_SKU_NOT_FOUND = "merchant-sku-not-found";
  private static final String MERCHANT_SKU_DUPLICATE = "merchant-sku-duplicate";
  private static final String SPECIFICATION_DETAIL = "specificationDetail";
  private static final String PRODUCT_STORY = "productStory";
  private static final String UNIQUE_SELLING_POINT = "uniqueSellingPoint";
  private static final String UOM = "uom";
  private static final String URL = "url";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String CATALOG_CODE = "catalogCode";
  private static final String ITEM_CATENTRY_ID = "itemCatentryId";
  private static final Boolean IS_LATE_FULFILLMENT = false;
  private static final String ETD_NOTE = "etdNote";
  private static final String SKU_CODE = "skuCode";
  private static final String GENERATED_ITEM_NAME = "generatedItemName";
  private static final String UPC_CODE = "upcCode";
  private static final boolean IS_ACTIVATED = false;
  private static final boolean IS_VIEWABLE = false;
  private static final String HASH = "hash";
  private static final String IMAGE_URL = "imageUrl";
  private static final int DANGEROUS_LEVEL = 0;
  private static final Double ITEM_DELIVERY_WEIGHT = 0.0;
  private static final Double ITEM_HEIGHT = 0.0;
  private static final Double ITEM_LENGTH = 0.0;
  private static final Double ITEM_WEIGHT = 0.0;
  private static final Double ITEM_WIDTH = 0.0;
  private static final InventoryType INVENTORY_TYPE = InventoryType.WITH_INVENTORY;
  private static final String LONG_DESCRIPTION = "longDescription";
  private static final String SETTLEMENT_TYPE = "settlementType";
  private static final boolean INSTALLATION_REQUIRED = false;
  private static final String CATGROUP_ID = "catGroupId";
  private static final boolean DISCOVERABLE = true;
  private static final int TOTAL_OTHER_COMBO = 5;
  private static final String HASH_VALUE = "0�_��Tr�\u0019�őA%\u001AK�2�*����;�\u001B�J�5�";

  private static final boolean BOOLEAN_FALSE = false;
  private static final String ATTRIBUTE_CODE = "attribute-code";
  private static final String ATTRIBUTE_CODE_2 = "attribute-code-2";
  private static final String ATTRIBUTE_CODE_3 = "attribute-code-3";
  private static final Integer SEQUENCE = 1;
  private static final String ALLOWED_ATTRIBUTE_CODE = "allowed-attribute-code";
  private static final String PREDEFINED_ALLOWED_ATTRIBUTE_CODE = "predefined-allowed-attribute-code";
  private static final String DESCRIPTIVE_ATTRIBUTE_VALUE = "descriptive-attribute-value";
  private static final DescriptiveAttributeValueType DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE =
      DescriptiveAttributeValueType.SINGLE;
  private static final String PRISTINE_PRODUCT_NAME = "pristine-product-name";
  private static final String STATUS = "ACTIVE";
  private static final String SEARCH_KEY = "product";

  private static final int SIZE = 10;

  private static final String MASTER_CATALOG_STRING = "master-catalog-string";

  private static final List<String> SALES_CATALOG_LIST = Arrays.asList("sales-catalog-string");

  private static final int TOTAL_SIZE = 1;

  private static final int PAGE = 0;
  private static final String REQUEST_ID = "requestid";

  private static final String ITEM_NAME = "itemname";

  private static final String ITEM_SKU = "itemsku";

  private static final String MERCHANTID = "merchantid";

  private static final String ITEM_PRICE = "item price";

  private static final double OFFER_PRICE = 5.0;

  private static final boolean BUYABLE = true;

  private static final String STORE_ID = "store id";

  private static final String DESCRIPTION = "description";

  private static final String VALUE_FORMAT1 = "1:15000:12345,123;0:10000:1234,12347;0:12500: ";

  private static final String VARIABLE = "variable";

  private static final Integer TOTAL = 10;

  private static final String PATH_FILE_INSERT_INVENTORY_LEVEL2_REQUEST_JSON =
      "src/test/resources/insertInventoryLevel2Request.json";

  private static final String LEVEL1_ID = "level1-id";

  private static final String LEVEL2_ID = "level2-id";

  private static final int STOCK = 1;

  private static final int ORIGINAL_STOCK = 1;

  private static final String MERCHANT_CODE = "merchant-level1-code";

  private static final String LEVEL2_MERCHANT_CODE = "merchant-level2-code";

  private static final BigDecimal TOTAL_HANDLING_FEE = new BigDecimal(50000);

  private static final String CATENTRY_ID = "catentry-id";

  private static final int QUANTITY = 2;

  private static final String MERCHANT_LEVEL1_CODE = "merchant-level1-code";

  private static final String MERCHANT_LEVEL2_CODE = "merchant-level2-code";
  private static final Date CREATED_DATE = new Date();
  private static final Date UPDATED_DATE = new Date();

  private static final String CHANNEL_ALL = ChannelName.DEFAULT.toString();

  private static final String CURRENCY = "IDR";

  private static final Date START_DATE_TIME = new Date();

  private static final Date END_DATE_TIME = new Date();

  private static final double SALE_PRICE = 0;

  private static final String PRODUCT_SKU = "product-sku";
  private static final String SIZE_CHART_CODE = "sizeChartCode";

  private static final String ITEM_SKU_B = "item-sku-B";

  private static final String PRISTINE_ID = "pristineId";
  private static final String GARANSI = "garansi";

  private static final String LAMA_GARANSI = "lama garansi";

  private static final String GUARANTEE_TYPE = "distributor guarantee";

  private static final String GUARANTEE_DURATION = "1 Tahun";
  private static final String MERCHANT_ID = "merchantId";
  private static final ProductType PRODUCT_TYPE = ProductType.REGULAR;
  private static final double MIN_PRICE = 200;
  private static final String ATTRIBUTE_NAME = "attributeName";
  private static final String VALUE = "value";
  private static final String OFFLINE_ITEM_ID = "offline-item-id";
  private static final String CATEGORY_CODE_VARIABLE = "categoryCodeForDisableUnSync";
  private static final String SYSTEM_PARAMETER_VALUE = "VALUE";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String SOURCE_ITEM_CODE = "sourceItemCode";
  private static final String DOCUMENT_TYPE = "DT1, DT2";
  private static final String DOCUMENT_TYPE_NEW = "Driving";
  private static final String DOCUMENT_TYPE_1 = "DT1";
  private static final String DOCUMENT_TYPE_2 = "DT2";
  private static final String PREORDER_TYPE = "DAYS";
  private static final String PREORDER_WEEK_TYPE = "WEEK";
  private static final Integer PREORDER_VALUE = 10;
  private static final Integer TOTAL_DAYS = 70;
  private static final String WAREHOUSE = "WH";
  private static final String MARKETPLACE = "MKT";
  private static final String CNC = "CNC";
  private static final String DEFAULT = "DEFAULT";
  private static final List<String> IMEI_ATTRIBUTE_VALUES = Arrays.asList("YES","Yes","yes");
  private static final List<String> IMEI_ATTRIBUTE_VALUES_1 = Arrays.asList("No","no");
  public static final String IMEI_ATTRIBUTE_CODE = "IM-2100000003";
  public static final String IMEI_ATTRIBUTE_CODE_1 = "NO-2100000003";
  private static final Set<ItemViewConfigDTO> ALL_VIEW_CONFIGS = getAllViewConfigs();
  public static final String ATTRIBUTE_CODE1 = "IMEI123";
  public static final String ATTRIBUTE_VALUE = "YES";
  public static final String VIDEO_ID = "VideoId";
  public static final String VIDEO_URL = "video-url";
  private static final String VIDEO_NAME = "video-name";
  public static final String COVER_IMAGE_PATH = "/coverImagePath";
  public static final String FINAL_IMAGE_URL = "/finalImageUrl";


  private SystemParameter systemParameter;

  private ProductItemDetailVO itemDetail_1;

  private ItemDetailRestWeb itemDetailRestWeb;

  private List<ProductItemDetailVO> itemDetails;
  private ItemPriceVO itemPrice;
  private List<ItemPriceVO> itemPriceList;
  private SimpleItemResponse itemPriceRestWeb;
  private SystemParameterRequest systemParameterRequest;
  private ProductForTransactionVO transactionVO;
  private ProductForTransactionResponse productResponseRestWeb;
  private ObjectMapper mapper;
  private String insertInventoryLevel2Request;
  private SystemParameter systemParameterForSystemResponse;
  private HandlingFeeResponse handlingFeeResponse;
  private HandlingFeeRequestRestWeb handlingFeeRequestRestWeb;
  private List<HandlingFeeRequestRestWeb> handlingFeeRequestRestWebList;
  private ProductRequest productRequest;
  private Product productObject;
  private List<OfflineItemRequest> offlineItemRequestList;
  private Map<String, ProductType> productTypeByProductSkuMap;
  private UpsertOfflineItemRequest upsertOfflineItemRequest;
  private List<UpsertOfflineItemRequest> upsertOfflineItemRequests;
  private CategoryDTO categoryDTO = new CategoryDTO();
  private CategoryResponse categoryResponse;
  private ItemViewConfigAndItemSkuRequest itemViewConfigAndItemSkuRequest;
  private MasterDataDetailWithProductAndItemsResponse masterDataDetailWithProductAndItemsResponse;
  private QuickEditUpdateRequest quickEditUpdateRequest;
  private ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest1;
  private BusinessPartnerPickupPoint businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
  private BusinessPartnerPickupPoint businessPartnerPickupPoint1 = new BusinessPartnerPickupPoint();
  private ItemPickupPointActivationRequest itemPickupPointActivationRequest = new ItemPickupPointActivationRequest();
  private ItemActivationRequest itemActivationRequest = new ItemActivationRequest();
  private ItemPickupPointUpdateRequest itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
  private BasicProductAndItemDTO basicProductAndItemDTO = new BasicProductAndItemDTO();

  @InjectMocks
  private ModelConverterImpl modelConverterImpl;
  @Mock
  private GdnMapperHelper gdnMapperHelper;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private ChannelService channelService;
  @Mock
  private ProductAndItemSolrConstructorService solrConstructorService;
  @Mock
  private Page<ProductAndItemSolr> solrPage;
  @Mock
  private MasterDataService masterDataService;
  @Mock
  private ProductHelperService productHelperService;
  @Mock
  private MasterDataConstructorService masterDataConstructorService;
  @Mock
  private ListUtil listUtil;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemService itemService;

  @Mock
  private AllowedAttributeValuesService allowedAttributeValuesService;

  private ProductResponse productResponse;

  private DiscountPriceDTO salePriceRequest;

  private PriceRequest priceRequest;

  private com.gdn.x.product.rest.web.model.request.ItemRequest itemRequest1;

  private MasterDataItemAttributeValueDTO masterItemAttributeValueRequest;

  private ArrayList<MasterDataItemAttributeValueDTO> masterItemAttributeValuesRequest;

  private MasterDataItemImageDTO masterItemImageRequest;

  private ArrayList<MasterDataItemImageDTO> masterItemImagesRequest;

  private MasterDataItemDTO masterDataItemRequest;

  private MasterDataItemImage masterItemImage;

  private Item itemDetail;

  private DiscountPrice salePrice;

  private Price price1;

  private MasterDataItemAttributeValue masterItemAttributeValue;

  private ArrayList<MasterDataItemAttributeValue> masterItemAttributeValues;

  private MasterDataItem masterDataItem;

  private ArrayList<MasterDataItemImage> masterItemImages;

  private DiscountPriceDTO salePriceResponse;

  private PriceDTO priceResponse;

  private MasterDataItemAttributeValueDTO masterItemAttributeValueResponse;

  private ArrayList<MasterDataItemAttributeValueDTO> masterItemAttributeValuesResponse;

  private MasterDataItemImageDTO masterItemImageResponse;

  private ArrayList<MasterDataItemImageDTO> masterItemImagesResponse;

  private MasterDataItemDTO masterDataItemResponse;

  private ItemResponse itemResponse1;

  private Set<PriceRequest> hashSetPriceRequest;

  private Set<Price> hashSetPrice;

  private Set<PriceDTO> hashSetPriceResponse;

  private ProductAndItemsVO productanditemsvo;

  private ProductItemsVo productItemsVo;

  private Product productForGetProductAndItems;

  private Item itemAForGetProductAndItems;

  private Item itemBForGetProductAndItems;

  private Item itemCForGetProductAndItems;

  private List<Item> itemList;

  private ProductAndItemsResponse productAndItemsResponse1;

  private ProductResponse productResponseForGet;

  private ItemResponse itemAResponseForGet;

  private ItemResponse itemBResponseForGet;

  private MasterCatalogRequest masterCatalogRequest;

  private MasterCatalog masterCatalog1;

  private MasterCatalogDTO masterCatalogResponse;

  private SalesCatalogRequest salesCatalogRequest;

  private SalesCatalog salesCatalog;

  private SalesCatalogDTO salesCatalogResponse;

  private ItemViewConfigRequest productViewConfigRequest;

  private ItemViewConfig viewConfig;

  private ItemViewConfigDTO productViewConfigResponse;

  private ArrayList<DiscountPriceDTO> listOfSalePriceResponse;

  private ProductAndItemsRequest productAndItemsRequestDTO;

  private ArrayList<ItemResponse> listOfItemAndStockDTO;

  private ItemResponse itemAndStockADTOForGet;

  private ItemResponse itemAndStockBDTOForGet;

  private ProductDTO productDTO;

  private ItemDTO itemDTO;

  private Item itemRequestVO;

  private BasicItemDTO basicItemDTO;

  private Product productSimple;
  private ProductResponse productResponseForGetProductAndItems;
  private ProductL3Response productL3Response;

  private Item itemSimple;
  private ProductAndItemInfoResponse productAndItemInfoResponse;
  private ItemInfoResponse itemInfoResponse;
  private ItemInfoResponseV2 itemInfoResponseV2;
  private ProductAndItemInfoResponseV2 productAndItemInfoResponseV2;
  private ProductInfoResponse productInfoResponse;
  private ItemViewConfigDTO itemViewConfigDTO;
  private PriceDTO priceDTO;
  private ComboDetailVo comboDetailVo;
  private ComboDetailResponse comboDetailResponse;
  private PreOrder preOrder;
  private PreOrderDTO preOrderDTO;
  private PreOrderVO preOrderVO;
  private ProductVo productVo = new ProductVo();
  private ItemVo itemVo = new ItemVo();

  @Test
  public void convertHandlingFeeRequestListTest() throws ReflectiveOperationException {
    List<HandlingFeeRequest> result =
        this.modelConverterImpl.convertToHandlingFeeRequestList(this.handlingFeeRequestRestWebList);
    assertNotNull(result);
    assertEquals(result.size(), (1));
    HandlingFeeRequest handlingFeeRequest = result.get(0);
    assertEquals(handlingFeeRequest.getItemSku(), (this.handlingFeeRequestRestWeb.getItemSku()));
    assertEquals(handlingFeeRequest.getQuantity(), (this.handlingFeeRequestRestWeb.getQuantity()));
  }

  @Test
  public void convertHandlingFeeRequestListTestWithNullParameter() throws ReflectiveOperationException {
    Assertions.assertThrows(Exception.class, () -> this.modelConverterImpl.convertToHandlingFeeRequestList(null));
  }

  @Test
  public void convertHandlingFeeResponseRestWebTest() throws ReflectiveOperationException {
    HandlingFeeResponseRestWeb result =
        this.modelConverterImpl.convertToHandlingFeeResponseRestWeb(this.handlingFeeResponse);
    assertNotNull(result);
    assertEquals(result.getTotalHandlingFee(), (ModelConverterImplTest.TOTAL_HANDLING_FEE));
  }

  @Test
  public void convertHandlingFeeResponseRestWebTestWithNullParameter() throws ReflectiveOperationException {
    Assertions.assertThrows(IllegalArgumentException.class, () -> this.modelConverterImpl.convertToHandlingFeeResponseRestWeb(null));
  }

  @Test
  public void convertItemDTOToItem() {
    Item result = this.modelConverterImpl.convertItemDTOToItem(this.itemDTO);
    verify(this.gdnMapperHelper).mapBean(this.itemDTO, Item.class);
    assertEquals(result, (this.itemRequestVO));
  }

  @Test
  public void convertItemDTOToItemWithNullItemDTO() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertItemDTOToItem(null));
  }

  @Test
  public void convertListToResponseTest() {
    List<Item> items = new ArrayList<>();
    Item item = new Item(ModelConverterImplTest.ITEM_SKU, ModelConverterImplTest.PRODUCT_SKU);
    items.add(item);

    List<ItemResponse> expectedResult = new ArrayList<>();
    ItemResponse itemResponse = new ItemResponse();
    itemResponse.setItemSku(ModelConverterImplTest.ITEM_SKU);
    itemResponse.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    expectedResult.add(itemResponse);

    when(this.gdnMapperHelper.mapBean(item, ItemResponse.class)).thenReturn(itemResponse);

    List<ItemResponse> result = this.modelConverterImpl.convertListToResponse(items, ItemResponse.class);
    Assertions.assertEquals(expectedResult, (result));

    verify(this.gdnMapperHelper).mapBean(item, ItemResponse.class);
  }

  @Test
  public void convertProductDTOToProduct() {
    Product result = this.modelConverterImpl.convertProductDTOToProduct(this.productDTO);
    verify(this.gdnMapperHelper).mapBean(this.productDTO, Product.class);
    assertEquals(result, (this.productObject));
  }

  @Test
  public void convertProductDTOToProductWithNullProductDTO() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertProductDTOToProduct(null));
  }

  @Test
  public void convertProductResponseTest() {
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
  }

  @Test
  public void convertProductResponseWithPreOrderTest() {
    itemDetail_1.setPreOrder(preOrderVO);
    transactionVO.setItemDetail(itemDetail_1);
    ProductItemDetailDTO itemDetailDTO = new ProductItemDetailDTO();
    ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO();
    itemCatalogDTO.setCatalogId(SYSTEM_PARAMETER_VALUE);
    itemDetailDTO.setItemCatalogs(Collections.singletonList(itemCatalogDTO));
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    productResponseRestWeb.getItemDetail().setPreOrder(preOrderDTO);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            StringUtils.EMPTY);
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(result.get(0).getItemDetail().getDocumentType()));
    assertEquals(PREORDER_VALUE, result.get(0).getItemDetail().getPreOrder().getPreOrderValue());
  }

  @Test
  public void convertProductResponseDocumentNoMasterCategoryTest() {
    ProductItemDetailDTO itemDetailDTO = new ProductItemDetailDTO();
    ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO();
    itemCatalogDTO.setCatalogId(SYSTEM_PARAMETER_VALUE);
    itemDetailDTO.setItemCatalogs(Collections.singletonList(itemCatalogDTO));
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            StringUtils.EMPTY);
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(result.get(0).getItemDetail().getDocumentType()));
  }

  @Test
  public void convertProductResponseDocumentNullCategoryResponseTest() {
    ProductItemDetailDTO itemDetailDTO = getProductItemDetailDTO();
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(result.get(0).getItemDetail().getDocumentType()));
  }

  @Test
  public void convertProductResponseDocumentTest() {
    ProductItemDetailDTO itemDetailDTO = getProductItemDetailDTO();
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
    assertEquals(DOCUMENT_TYPE_1, result.get(0).getItemDetail().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.get(0).getItemDetail().getDocumentType().get(1));
  }

  @Test
  public void convertProductResponseDocumentTwoItemsTest() {
    ProductItemDetailDTO itemDetailDTO = getProductItemDetailDTO();
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    ProductForTransactionVO productForTransactionVO = new ProductForTransactionVO();
    productForTransactionVO.setItemSku(ITEM_SKU_B);
    List<ProductForTransactionVO> productForTransactionVOS = new ArrayList<>();
    productForTransactionVOS.add(productForTransactionVO);
    productForTransactionVOS.add(transactionVO);
    ProductForTransactionResponse productResponseRestWeb1 = new ProductForTransactionResponse();
    productResponseRestWeb1.setItemSku(ITEM_SKU_B);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    when(gdnMapperHelper.mapBean(productForTransactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb1);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(productForTransactionVOS);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertNotNull(result);
    assertEquals(ITEM_SKU, result.get(1).getItemSku());
    assertEquals(ITEM_SKU_B, result.get(0).getItemSku());
    assertEquals(DOCUMENT_TYPE_1, result.get(1).getItemDetail().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.get(1).getItemDetail().getDocumentType().get(1));
    assertNull(result.get(0).getItemDetail());
  }

  @Test
  public void convertProductResponseDocumentTwoItemsDocumentTest() {
    ProductItemDetailDTO itemDetailDTO = getProductItemDetailDTO();
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    ProductForTransactionVO productForTransactionVO = new ProductForTransactionVO();
    productForTransactionVO.setItemSku(ITEM_SKU_B);
    List<ProductForTransactionVO> productForTransactionVOS = new ArrayList<>();
    productForTransactionVOS.add(productForTransactionVO);
    productForTransactionVOS.add(transactionVO);
    ProductForTransactionResponse productResponseRestWeb1 = new ProductForTransactionResponse();
    ProductItemDetailDTO productItemDetailDTO = getProductItemDetailDTO();
    productItemDetailDTO.getItemCatalogs().get(0).getItemCategories().get(0).setCategoryId(CATEGORY_CODE);
    productResponseRestWeb1.setItemDetail(productItemDetailDTO);
    productResponseRestWeb1.setItemSku(ITEM_SKU_B);
    CategoryResponse categoryResponse1 = new CategoryResponse();
    categoryResponse1.setDocumentType(DOCUMENT_TYPE_NEW);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    when(gdnMapperHelper.mapBean(productForTransactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb1);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE)).thenReturn(categoryResponse1);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(productForTransactionVOS);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            CATEGORY_CODE);
    assertNotNull(result);
    assertEquals(ITEM_SKU, result.get(1).getItemSku());
    assertEquals(ITEM_SKU_B, result.get(0).getItemSku());
    assertEquals(DOCUMENT_TYPE_1, result.get(1).getItemDetail().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.get(1).getItemDetail().getDocumentType().get(1));
    assertEquals(DOCUMENT_TYPE_NEW, result.get(0).getItemDetail().getDocumentType().get(0));
  }

  private ProductItemDetailDTO getProductItemDetailDTO() {
    ProductItemDetailDTO itemDetailDTO = new ProductItemDetailDTO();
    ItemCatalogDTO itemCatalogDTO = new ItemCatalogDTO();
    itemCatalogDTO.setCatalogId(Constants.MASTER_CATALOG);
    ItemCategoryDTO itemCategoryDTO = new ItemCategoryDTO();
    itemCategoryDTO.setCategoryId(SYSTEM_PARAMETER_VALUE);
    itemCatalogDTO.setItemCategories(Collections.singletonList(itemCategoryDTO));
    itemDetailDTO.setItemCatalogs(Collections.singletonList(itemCatalogDTO));
    return itemDetailDTO;
  }

  @Test
  public void convertProductResponseDocumentNullTest() {
    ProductItemDetailDTO itemDetailDTO = getProductItemDetailDTO();
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    categoryResponse.setDocumentType(null);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(result.get(0).getItemDetail().getDocumentType()));
  }

  @Test
  public void convertProductResponseDocumentExceptionTest() {
    ProductItemDetailDTO itemDetailDTO = getProductItemDetailDTO();
    productResponseRestWeb.setItemDetail(itemDetailDTO);
    when(gdnMapperHelper.mapBean(transactionVO,
        ProductForTransactionResponse.class)).thenReturn(productResponseRestWeb);
    Mockito.doThrow(RuntimeException.class).when(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    List<ProductForTransactionResponse> result =
        this.modelConverterImpl.convertToProductForTransactionResponse(Arrays.asList(this.transactionVO));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertNotNull(result);
    assertEquals(result.get(0).getItemSku(), (ModelConverterImplTest.ITEM_SKU));
    assertTrue(CollectionUtils.isEmpty(result.get(0).getItemDetail().getDocumentType()));
  }

  @Test
  public void convertProductResponseTestWithEmptyList() {
    Assertions.assertTrue(CollectionUtils.isEmpty(
      this.modelConverterImpl.convertToProductForTransactionResponse(Collections.emptyList())));
  }

  @Test
  public void convertProductResponseTestWithNullParameter() throws ReflectiveOperationException {
    Assertions.assertTrue(CollectionUtils.isEmpty(
      this.modelConverterImpl.convertToProductForTransactionResponse(null)));
  }

  @Test
  public void convertRequestListToModelTest() {
    List<ItemRequest> itemRequests = new ArrayList<>();
    ItemRequest itemRequest = new ItemRequest();
    itemRequest.setItemSku(ModelConverterImplTest.ITEM_SKU);
    itemRequest.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    itemRequests.add(itemRequest);

    List<Item> items = new ArrayList<>();
    Item item = new Item(ModelConverterImplTest.ITEM_SKU, ModelConverterImplTest.PRODUCT_SKU);
    items.add(item);

    when(this.gdnMapperHelper.mapBean(itemRequest, Item.class)).thenReturn(item);

    List<Item> result = this.modelConverterImpl.convertRequestListToModel(itemRequests, Item.class);
    Assertions.assertEquals(items.get(0).getItemSku(), (result.get(0).getItemSku()));
    Assertions.assertEquals(items.get(0).getProductSku(), (result.get(0).getProductSku()));

    verify(this.gdnMapperHelper).mapBean(itemRequest, Item.class);
  }

  @Test
  public void convertSystemParameterResponseTest() throws ReflectiveOperationException {
    SystemParameterResponse result =
        this.modelConverterImpl.convertToSystemParameterResponse(this.systemParameterForSystemResponse);
    assertNotNull(result);
    assertEquals(result.getDescription(), (ModelConverterImplTest.DESCRIPTION));
    assertEquals(result.getValue(), (ModelConverterImplTest.VALUE_FORMAT1));
  }

  @Test
  public void convertSystemParameterResponseTestWithNullParameter() throws ReflectiveOperationException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToSystemParameterResponse(null));
  }

  @Test
  public void convertSystemParameterTest() throws ReflectiveOperationException {
    SystemParameter result =
        this.modelConverterImpl.convertToSystemParameter(this.systemParameterRequest, this.systemParameter);
    assertNotNull(result);
    assertEquals(result.getDescription(), (ModelConverterImplTest.DESCRIPTION));
    assertEquals(result.getVariable(), (ModelConverterImplTest.VARIABLE));
    assertEquals(result.getValue(), (ModelConverterImplTest.VALUE_FORMAT1));
  }

  @Test
  public void convertSystemParameterTestWithNullParameter() throws ReflectiveOperationException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToSystemParameter(null, this.systemParameter));
  }

  @Test
  public void convertToAddProductAndItemsResponse() {
    AddProductAndItemsResponseVo addProductAndItemsResponseVo = new AddProductAndItemsResponseVo();
    AddProductAndItemsResponse response = new AddProductAndItemsResponse();
    when(this.gdnMapperHelper.mapBean(addProductAndItemsResponseVo, AddProductAndItemsResponse.class))
        .thenReturn(response);
    this.modelConverterImpl
        .convertToAddProductAndItemsResponse(ModelConverterImplTest.REQUEST_ID, addProductAndItemsResponseVo);
    verify(this.gdnMapperHelper).mapBean(addProductAndItemsResponseVo, AddProductAndItemsResponse.class);
  }

  @Test
  public void convertToItem() {
    Item result = this.modelConverterImpl.convertToItem(this.itemRequest1);

    verify(this.gdnMapperHelper).mapBean(this.itemRequest1, Item.class);
    assertNotNull(result);
    assertEquals(result, (this.itemDetail));
  }

  @Test
  public void convertToItemDetailRestWebGdnRestListResponseCorrectParameters() {
    GdnRestListResponse<ItemDetailRestWeb> result = this.modelConverterImpl
        .convertToItemDetailRestWebGdnRestListResponse(this.itemDetails, ModelConverterImplTest.PAGE,
            ModelConverterImplTest.TOTAL_SIZE, ModelConverterImplTest.REQUEST_ID);
    assertEquals(result.getContent().get(0), (this.itemDetailRestWeb));
  }

  @Test
  public void convertToItemDetailRestWebGdnRestListResponseTestWithNullParameter() {
    this.modelConverterImpl.convertToItemDetailRestWebGdnRestListResponse(null, 0, 0, null);
  }

  @Test
  public void convertToItemDetailRestWebGdnRestListResponseWithEmptyInputList() {
    GdnRestListResponse<ItemDetailRestWeb> result = this.modelConverterImpl
        .convertToItemDetailRestWebGdnRestListResponse(new ArrayList<ProductItemDetailVO>(),
            ModelConverterImplTest.PAGE, ModelConverterImplTest.TOTAL_SIZE, ModelConverterImplTest.REQUEST_ID);
    assertEquals(result.getContent().size(), (0));
  }

  @Test
  public void convertToItemPriceResponseTest() {
    PriceDTO result = this.modelConverterImpl.convertToItemPriceResponse(this.price1);

    verify(this.gdnMapperHelper).mapBean(this.price1, PriceDTO.class);

    assertNotNull(result);
    assertEquals(result.getChannel(), this.price1.getChannel());
    assertEquals(result.getCurrency(), this.price1.getCurrency());
    assertEquals(result.getOfferPrice(), (this.price1.getOfferPrice()));
  }

  @Test
  public void convertToItemPriceResponseTestWithNullPrice() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemPriceResponse(null));
  }

  @Test
  public void convertToItemPriceRestWebGdnRestListResponseTest() {
    GdnRestListResponse<SimpleItemResponse> result = this.modelConverterImpl
        .convertToItemPriceRestWebGdnRestListResponse(this.itemPriceList, ModelConverterImplTest.REQUEST_ID);
    assertEquals(result.getContent().size(), (1));
    assertEquals(result.getContent().get(0), (this.itemPriceRestWeb));
  }

  @Test
  public void convertToItemPriceRestWebGdnRestListResponseTestWithEmptyInputList() {
    GdnRestListResponse<SimpleItemResponse> result = this.modelConverterImpl
        .convertToItemPriceRestWebGdnRestListResponse(new ArrayList<ItemPriceVO>(), ModelConverterImplTest.REQUEST_ID);
    assertEquals(result.getContent().size(), (0));
  }

  @Test
  public void convertToItemPriceRestWebGdnRestListResponseTestWithNullParameter() {
    this.modelConverterImpl.convertToItemPriceRestWebGdnRestListResponse(null, null);
  }

  @Test
  public void convertToItemPriceTest() {
    Price result = this.modelConverterImpl.convertToItemPrice(this.priceRequest);

    verify(this.gdnMapperHelper).mapBean(this.priceRequest, Price.class);

    assertNotNull(result);
    assertEquals(result.getChannel(), this.priceRequest.getChannel());
    assertEquals(result.getCurrency(), this.priceRequest.getCurrency());
    assertEquals(result.getOfferPrice(), (this.priceRequest.getOfferPrice()));
  }

  @Test
  public void convertToItemPriceTestWithNullPriceRequest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemPrice(null));
  }

  @Test
  public void convertToItemResponseTest() {
    ItemResponse result = this.modelConverterImpl.convertToItemResponse(this.itemDetail);
    verify(this.gdnMapperHelper).mapBean(this.itemDetail, ItemResponse.class);

    assertNotNull(result);
    assertEquals(result, (this.itemResponse1));
  }

  @Test
  public void convertToItemResponseTestWithNullItem() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemResponse(null));
  }

  @Test
  public void convertToItemSalePriceResponseTest() throws ReflectiveOperationException {
    DiscountPriceDTO result = this.modelConverterImpl.convertToItemSalePriceResponse(this.salePrice);

    assertNotNull(result);
    assertEquals(result.getStartDateTime(), this.salePrice.getStartDateTime());
    assertEquals(result.getEndDateTime(), this.salePrice.getEndDateTime());
    assertEquals(result.getDiscountPrice(), (this.salePrice.getDiscountPrice()));
  }

  @Test
  public void convertToItemSalePriceResponseTestWithNullSalePrice() throws ReflectiveOperationException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemSalePriceResponse(null));
  }

  @Test
  public void convertToItemSalePriceTest() throws ReflectiveOperationException {
    DiscountPrice result = this.modelConverterImpl.convertToItemSalePrice(this.salePriceRequest);

    assertNotNull(result);
    assertEquals(result.getStartDateTime(), this.salePriceRequest.getStartDateTime());
    assertEquals(result.getEndDateTime(), this.salePriceRequest.getEndDateTime());
    assertEquals(result.getDiscountPrice(), (this.salePriceRequest.getDiscountPrice()));
  }

  @Test
  public void convertToItemSalePriceTestWithNullSalePriceRequest() throws ReflectiveOperationException {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemSalePrice(null));
  }

  @Test
  public void convertToItemSummaryPageResponseNullObject() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemSummaryListResponse(ModelConverterImplTest.REQUEST_ID, 1, 1, null));
  }

  @Test
  public void convertToItemSummaryPageResponseSuccess() throws Exception {
    ItemSummaryPageResponseVo voObject = new ItemSummaryPageResponseVo();
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setVersion(2l);
    itemSummaryResponseVO.setItemSku(ITEM_SKU);
    voObject.setItemSummaryResponses(Collections.singletonList(itemSummaryResponseVO));
    voObject.getItemSummaryResponses().get(0).setProductScore(90);
    ItemSummaryPageResponse pageResponses = new ItemSummaryPageResponse();
    ItemSummaryResponse itemSummaryResponse = new ItemSummaryResponse();
    itemSummaryResponse.setItemSku(ITEM_SKU);
    pageResponses.setItemSummaryResponses(Collections.singletonList(itemSummaryResponse));
    when(this.gdnMapperHelper.mapBean(voObject, ItemSummaryPageResponse.class)).thenReturn(pageResponses);
    GdnRestListResponse<ItemSummaryResponse> itemSummaryPageResponse =
        this.modelConverterImpl.convertToItemSummaryListResponse(ModelConverterImplTest.REQUEST_ID, 10, 1, voObject);
    verify(this.gdnMapperHelper).mapBean(voObject, ItemSummaryPageResponse.class);
    Assertions.assertEquals(itemSummaryPageResponse.getContent().get(0).getVersion(),
        voObject.getItemSummaryResponses().get(0).getVersion());
    Assertions.assertEquals(itemSummaryPageResponse.getContent().get(0).getItemSku(),
        voObject.getItemSummaryResponses().get(0).getItemSku());
  }

  @Test
  public void convertToItemSummaryRequestVoTest() {
    UpdateItemSummaryRequest updateRequest = new UpdateItemSummaryRequest();
    UpdateItemSummaryRequestVo updateRequestVo = new UpdateItemSummaryRequestVo();
    HashSet<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig config = new ItemViewConfig();
    config.setBuyable(true);
    itemViewConfigs.add(config);
    updateRequestVo.setItemViewConfigs(itemViewConfigs);
    HashSet<Price> prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(10000);
    prices.add(price);
    updateRequestVo.setPrice(prices);
    when(this.gdnMapperHelper.mapBean(updateRequest, UpdateItemSummaryRequestVo.class)).thenReturn(updateRequestVo);
    this.modelConverterImpl.convertToUpdateItemSummaryVo(updateRequest);
    verify(this.gdnMapperHelper).mapBean(updateRequest, UpdateItemSummaryRequestVo.class);
    verify(this.channelService, times(2)).getDefaultChannel();
  }

  @Test
  public void convertToItemSummaryRequestVoTestWithNullConfig() {
    UpdateItemSummaryRequest updateRequest = new UpdateItemSummaryRequest();
    UpdateItemSummaryRequestVo updateRequestVo = new UpdateItemSummaryRequestVo();
    when(this.gdnMapperHelper.mapBean(updateRequest, UpdateItemSummaryRequestVo.class)).thenReturn(updateRequestVo);
    this.modelConverterImpl.convertToUpdateItemSummaryVo(updateRequest);
    verify(this.gdnMapperHelper).mapBean(updateRequest, UpdateItemSummaryRequestVo.class);
  }

  @Test
  public void convertToItemSummarySingleResponseSuccess() {
    ItemSummaryPageResponseVo voObject = new ItemSummaryPageResponseVo();
    voObject.setItemSummaryResponses(Arrays.asList(new ItemSummaryResponseVO()));
    when(this.gdnMapperHelper.mapBean(voObject.getItemSummaryResponses().get(0), ItemSummaryResponse.class))
        .thenReturn(new ItemSummaryResponse());
    this.modelConverterImpl.convertToItemSummarySingleResponse(ModelConverterImplTest.REQUEST_ID, voObject);
    verify(this.gdnMapperHelper).mapBean(voObject.getItemSummaryResponses().get(0), ItemSummaryResponse.class);
  }


  @Test
  public void convertToItemSummarySingleResponseWithEmptyListVoObject() {
    ItemSummaryPageResponseVo voObject = new ItemSummaryPageResponseVo();
    voObject.setItemSummaryResponses(new ArrayList<ItemSummaryResponseVO>());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemSummarySingleResponse(ModelConverterImplTest.REQUEST_ID, voObject));
  }

  @Test
  public void convertToItemSummarySingleResponseWithNullListVoObject() {
    ItemSummaryPageResponseVo voObject = new ItemSummaryPageResponseVo();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemSummarySingleResponse(ModelConverterImplTest.REQUEST_ID, voObject));
  }

  @Test
  public void convertToItemSummarySingleResponseWithNullVoObject() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemSummarySingleResponse(ModelConverterImplTest.REQUEST_ID, null));
  }

  @Test
  public void convertToItemWithNullItemRequest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItem(null));
  }

  @Test
  public void convertToMasterCatalogResponseTest() {
    this.modelConverterImpl.convertToMasterCatalogResponse(this.masterCatalog1);
  }

  @Test
  public void convertToMasterCatalogResponseCategoryDTOTest() {
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE);
    category.setCatgroupId(CATENTRY_ID);
    masterCatalog1.setCategory(category);
    this.modelConverterImpl.convertToMasterCatalogResponse(this.masterCatalog1);
  }

  @Test
  public void convertToMasterCatalogResponseTestWithNullMasterCatalog() {
    this.modelConverterImpl.convertToMasterCatalogResponse(null);
  }


  @Test
  public void convertToMasterCatalogTest() {
    this.modelConverterImpl.convertToMasterCatalog(this.masterCatalogRequest);

    verify(this.gdnMapperHelper).mapBean(this.masterCatalogRequest, MasterCatalog.class);
  }

  @Test
  public void convertToMasterCatalogTestWithNullMasterCatalogRequest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToMasterCatalog(null));
  }

  private MasterDataDetailWithProductAndItemsResponseVo generateMasterDataDetailWithProductAndItemsResponseVo() {
    Product product = this.setupProduct();
    Item item = this.setupItem();
    item.setSourceItemCode(SOURCE_ITEM_CODE);
    item.setContentChanged(true);
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
      new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVOs.add(productAndItemsVO);
    masterDataDetailResponseVo.setProductAndItems(productAndItemsVOs);
    return masterDataDetailResponseVo;
  }

  @Test
  public void convertToMasterDataDetailResponse() {
    ProductAndItemsDTO productAndItemsDTO = new ProductAndItemsDTO();
    productAndItemsDTO.setItems(Arrays.asList(itemResponse1));
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
      generateMasterDataDetailWithProductAndItemsResponseVo();
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
        .thenReturn(masterDataDetailWithProductAndItemsResponse);
    MasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
  }

  @Test
  public void convertToMasterDataDetailResponseDocumentTest() {
    Product product = this.setupProduct();
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(product);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
        .thenReturn(masterDataDetailWithProductAndItemsResponse);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    MasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
    assertEquals(DOCUMENT_TYPE_1, response.getProductAndItems().get(0).getProduct().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, response.getProductAndItems().get(0).getProduct().getDocumentType().get(1));
  }

  @Test
  public void convertToMasterDataDetailResponseDocumentTest2() {
    Product product = this.setupProduct();
    categoryResponse.setDocumentType(null);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(product);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
        .thenReturn(masterDataDetailWithProductAndItemsResponse);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    MasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
    assertTrue(CollectionUtils.isEmpty(response.getProductAndItems().get(0).getProduct().getDocumentType()));
  }

  @Test
  public void convertToMasterDataDetailResponseDocumentOverrideLateFulfilmentTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "overrideLateFulfillmentByProductType", true);
    Product product = this.setupProduct();
    categoryResponse.setDocumentType(null);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(product);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
        .thenReturn(masterDataDetailWithProductAndItemsResponse);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    MasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
    assertTrue(CollectionUtils.isEmpty(response.getProductAndItems().get(0).getProduct().getDocumentType()));
  }

  @Test
  public void convertToMasterDataDetailResponseDocumentTest1() {
    Product product = this.setupProduct();
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(product);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
        .thenReturn(masterDataDetailWithProductAndItemsResponse);
    MasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
    assertTrue(CollectionUtils.isEmpty(response.getProductAndItems().get(0).getProduct().getDocumentType()));
  }

  @Test
  public void convertToMasterDataDetailResponseDocumentExceptionTest() {
    Product product = this.setupProduct();
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(product);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
        .thenReturn(masterDataDetailWithProductAndItemsResponse);
    Mockito.doThrow(RuntimeException.class).when(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    MasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
    assertTrue(CollectionUtils.isEmpty(response.getProductAndItems().get(0).getProduct().getDocumentType()));
  }

  private MasterDataDetailWithProductAndItemsResponseVo getMasterDataDetailWithProductAndItemsResponseVo(
      Product product) {
    Item item = this.setupItem();
    item.setSourceItemCode(SOURCE_ITEM_CODE);
    item.setContentChanged(true);
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVOs.add(productAndItemsVO);
    masterDataDetailResponseVo.setProductAndItems(productAndItemsVOs);
    ProductAndItemsDTO productAndItemsDTO = new ProductAndItemsDTO();
    productAndItemsDTO.setItems(Arrays.asList(itemResponse1));
    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(SYSTEM_PARAMETER_VALUE);
    masterCatalog.setCategory(category);
    masterDataDetailWithProductAndItemsResponse.getProductAndItems().get(0).getProduct()
        .setMasterCatalog(masterCatalog);
    return masterDataDetailResponseVo;
  }

  @Test
  public void toMasterDataDetailWithProductAndItemResponseTest() throws Exception {
    Product product = this.setupProduct();
    MasterDataDetailWithProductAndItemResponse response = new MasterDataDetailWithProductAndItemResponse();
    Item item = this.setupItem();
    MasterDataDetailWithProductAndItemResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemResponseVo();
    masterDataDetailResponseVo.setProduct(product);
    masterDataDetailResponseVo.setItem(item);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo,
        MasterDataDetailWithProductAndItemResponse.class)).thenReturn(response);
    this.modelConverterImpl.toMasterDataDetailWithProductAndItemResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void toMasterDataDetailWithProductAndItemResponseSyncProductTest() throws Exception {
    Product product = this.setupProduct();
    MasterDataDetailWithProductAndItemResponse response = new MasterDataDetailWithProductAndItemResponse();
    product.setSynchronized(true);
    Item item = this.setupItem();
    MasterDataDetailWithProductAndItemResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemResponseVo();
    masterDataDetailResponseVo.setProduct(product);
    masterDataDetailResponseVo.setItem(item);
    Map<String, MasterDataProduct> productCodeMasterDataProductMap = new HashMap<>();
    productCodeMasterDataProductMap.put(product.getProductCode(), product.getMasterDataProduct());
    masterDataDetailResponseVo.setMasterDataProducts(productCodeMasterDataProductMap);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo,
        MasterDataDetailWithProductAndItemResponse.class)).thenReturn(response);
    this.modelConverterImpl.toMasterDataDetailWithProductAndItemResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void toMasterDataDetailWithProductAndItemResponseSyncProductOverrideLatefulfilmentTest() throws Exception {
    Product product = this.setupProduct();
    MasterDataDetailWithProductAndItemResponse response = new MasterDataDetailWithProductAndItemResponse();
    response.setItem(new ItemResponse());
    product.setSynchronized(true);
    Item item = this.setupItem();
    MasterDataDetailWithProductAndItemResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemResponseVo();
    masterDataDetailResponseVo.setProduct(product);
    masterDataDetailResponseVo.setItem(item);
    Map<String, MasterDataProduct> productCodeMasterDataProductMap = new HashMap<>();
    productCodeMasterDataProductMap.put(product.getProductCode(), product.getMasterDataProduct());
    masterDataDetailResponseVo.setMasterDataProducts(productCodeMasterDataProductMap);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo,
        MasterDataDetailWithProductAndItemResponse.class)).thenReturn(response);
    this.modelConverterImpl.toMasterDataDetailWithProductAndItemResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    Assertions.assertEquals(false, response.getItem().getLateFulfillment());
  }

  @Test
  public void convertToPristineMasterDataDetailResponse() throws Exception {
    Product product = this.setupProduct();
    Item item = this.setupItem();
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVOs.add(productAndItemsVO);
    masterDataDetailResponseVo.setProductAndItems(productAndItemsVOs);

    this.modelConverterImpl.convertToPristineMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void convertToPristineMasterDataDetailResponse_synchronizedProduct() throws Exception {
    Product product = this.setupProduct();
    product.setSynchronized(Boolean.TRUE);
    Item item = this.setupItem();
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVOs.add(productAndItemsVO);
    masterDataDetailResponseVo.setProductAndItems(productAndItemsVOs);

    this.modelConverterImpl.convertToPristineMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void convertToProduct() {
    Product result = this.modelConverterImpl.convertToProduct(this.productRequest);
    assertEquals(result, (this.productObject));
  }

  @Test
  public void convertToProductWithVideoData() {
    // Here we are not populating video data because
    // we are not updating in the update api call but we are updating video via
    // generate-product-score api
    VideoAddEditRequest videoAddEditRequest = new VideoAddEditRequest();
    videoAddEditRequest.setVideoId(VIDEO_ID);
    videoAddEditRequest.setVideoUrl(VIDEO_URL);
    this.productRequest.setVideoAddEditRequest(videoAddEditRequest);
    Product result = this.modelConverterImpl.convertToProduct(this.productRequest);
    assertEquals(result, (this.productObject));
    assertNull(result.getVideo());
  }

  @Test
  public void convertToProductWithPreOrder() {
    productRequest.setPreOrder(preOrderDTO);
    Product product = this.setupProduct();
    product.setPreOrder(preOrder);
    when(gdnMapperHelper.mapBean(productRequest, Product.class)).thenReturn(product);
    Product result = this.modelConverterImpl.convertToProduct(this.productRequest);
    assertTrue(result.getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, result.getPreOrder().getPreOrderValue());
  }

  @Test
  public void convertToProductAndItemsDTOTest() {
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderTypeWeekTest() {
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(TOTAL_DAYS, result.getProduct().getPreOrder().getPreOrderValue());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderTypeDaysTest() {
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, result.getProduct().getPreOrder().getPreOrderValue());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderDateLessThanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderDateLessThanCurrentDateConvertDetailsFalseTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo, false);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderDateGreaterThanCurrentDateConvertDetailsFalseTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo, false);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderDateLessThanCurrentDateConvertDetailsTrueTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo, true);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertFalse(result.getProduct().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductAndItemsDTOWithPreOrderDateGreaterThanCurrentDateConvertDetailsTrueTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo, true);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProduct().getProductScore().getTotalScore(), 0);
    assertFalse(result.getItems().get(0).isMarkForDelete());
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
  }

  @Test
  public void convertToProductAndItemsDTONullTest() {
    productanditemsvo.getProduct().setProductScore(null);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
  }

  @Test
  public void convertToProductAndItemsDTO_WholesalePriceExistsTest() {
    itemAForGetProductAndItems.setWholesalePriceExists(true);
    itemAForGetProductAndItems.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    itemAForGetProductAndItems.setContentChanged(true);
    itemAForGetProductAndItems.setSourceItemCode(SOURCE_ITEM_CODE);
    itemBForGetProductAndItems.setWholesalePriceExists(false);
    this.itemList = new ArrayList<>();
    this.itemList.add(this.itemAForGetProductAndItems);
    this.itemList.add(this.itemBForGetProductAndItems);
    this.productanditemsvo.setItems(this.itemList);
    itemResponse1.setContentChanged(true);
    itemResponse1.setSourceItemCode(SOURCE_ITEM_CODE);
    when(this.gdnMapperHelper.mapBean(itemAForGetProductAndItems, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item : productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
    assertTrue(result.getItems().get(0).getWholesalePriceActivated());
    assertNull(result.getItems().get(1).getWholesalePriceActivated());
    assertTrue(result.getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, result.getItems().get(0).getSourceItemCode());
  }

  @Test
  public void convertToProductAndItemsDTOTest_WithMasterDataItem() {
    productanditemsvo.getItems().get(0).getMasterDataItem().setHash(HASH_VALUE);
    productanditemsvo.getItems().get(0).setMarkForDelete(true);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.productanditemsvo.getItems().get(0).getMasterDataItem(),
            this.productanditemsvo.getProduct().getMasterDataProduct()))
        .thenReturn(this.productanditemsvo.getItems().get(0).getMasterDataItem());
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    verify(masterDataConstructorService, times(2))
        .constructItemDimensionFields(this.productanditemsvo.getItems().get(0).getMasterDataItem(),
            this.productanditemsvo.getProduct().getMasterDataProduct());
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertEquals(StringUtils.SPACE, productanditemsvo.getItems().get(0).getMasterDataItem().getHash());
    assertTrue(result.getItems().get(0).isMarkForDelete());
  }

  @Test
  public void convertToProductAndItemsDTOTestWithNullProductAndItemsVO() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProductAndItemsDTO(null));
  }

  @Test
  public void convertToProductAndItemsDTO_nullMasterDataProduct_success() {
    this.productanditemsvo.getProduct().setMasterDataProduct(null);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertNull(result.getProduct().getMasterCatalog());
  }

  @Test
  public void convertToProductAndItemInfoResponseTest() {
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
  }

  @Test
  public void convertToProductAndItemInfoResponseWithPreOrderTest() {
    productInfoResponse.setPreOrder(preOrderDTO);
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
      .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, result.getProduct().getPreOrder().getPreOrderValue());
    assertNotNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseWithPreOrderForWeekTypeTest() {
    preOrderDTO.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    productInfoResponse.setPreOrder(preOrderDTO);
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class)).thenReturn(
      productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(TOTAL_DAYS, result.getProduct().getPreOrder().getPreOrderValue());
    assertNotNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseWithDateLessThanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertFalse(result.getProduct().getPreOrder().getIsPreOrder());
    assertNull(result.getProduct().getPreOrder().getPreOrderType());
    assertNull(result.getProduct().getPreOrder().getPreOrderValue());
    assertNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseWithPreorderDateGreaterthanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(Constants.DATE, result.getProduct().getPreOrder().getPreOrderType());
    assertNotNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseWithDateMoreThanCurrentDateAndTypeWeekTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 1);
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderValue(10);
    preOrderDTO.setPreOrderDate(cal.getTime());
    preOrderDTO.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderDTO.setIsPreOrder(true);
    productInfoResponse.setPreOrder(preOrderDTO);
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(TOTAL_DAYS, result.getProduct().getPreOrder().getPreOrderValue());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
  }

  @Test
  public void convertToProductAndItemInfoResponseDocumentTest() {
    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(SYSTEM_PARAMETER_VALUE);
    masterCatalog.setCategory(category);
    productInfoResponse.setMasterCatalog(masterCatalog);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(DOCUMENT_TYPE_1, result.getProduct().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.getProduct().getDocumentType().get(1));
  }

  @Test
  public void convertToProductAndItemInfoResponseDocumentTest2() {
    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(SYSTEM_PARAMETER_VALUE);
    masterCatalog.setCategory(category);
    productInfoResponse.setMasterCatalog(masterCatalog);
    categoryResponse.setDocumentType(null);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemInfoResponseDocumentTest1() {
    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(SYSTEM_PARAMETER_VALUE);
    masterCatalog.setCategory(category);
    productInfoResponse.setMasterCatalog(masterCatalog);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemInfoResponseDocumentExceptionTest() {
    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(SYSTEM_PARAMETER_VALUE);
    masterCatalog.setCategory(category);
    productInfoResponse.setMasterCatalog(masterCatalog);
    Mockito.doThrow(RuntimeException.class).when(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertEquals(itemViewConfigDTO, result.getItem().getChannelItemViewConfig());
    assertEquals(priceDTO, result.getItem().getChannelPrice());
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemInfoResponseNullMasterDataItemTest() {
    itemVo.setMasterDataItem(null);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponse.class)).thenReturn(itemInfoResponse);
    ProductAndItemInfoResponse result =
        this.modelConverterImpl.convertToProductAndItemInfoResponse(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponse.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
  }

  @Test
  public void convertToProductAndItemInfoResponseTestNullProductAndItemsVO() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProductAndItemsDTO(null));
  }

  @Test
  public void convertToProductAndItemsRequestVOTest() {
    productVo.setPreOrder(preOrder);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    org.springframework.beans.BeanUtils.copyProperties(itemDTO, itemActivationRequest);
    itemActivationRequest.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));
    when(this.gdnMapperHelper.mapBean(any(ProductDTO.class), eq(ProductVo.class)))
        .thenReturn(productVo);
    when(this.gdnMapperHelper.mapBean(any(ItemDTO.class), eq(ItemVo.class)))
        .thenReturn(itemVo);
    try {
      Assertions.assertThrows(IllegalArgumentException.class, () -> this.modelConverterImpl.convertToProductAndItemsRequestVO(
          new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest))));
    } finally {
      verify(this.gdnMapperHelper, times(2)).mapBean(any(), any());
      verify(this.listUtil).distinct(Mockito.anyList(), any());
    }
  }

  @Test
  public void convertToProductAndItemsRequestVO_WithPreOrderTest() {
    ProductAndItemsVO productAndItemsRequestVO = new ProductAndItemsVO(null, this.itemList);
    productDTO.setPreOrder(preOrderDTO);
    productDTO.setProductSku(PRODUCT_SKU);
    productDTO.setFbbActivated(true);
    productObject.setPreOrder(preOrder);
    productObject.setProductSku(PRODUCT_SKU);
    productVo.setPreOrder(preOrder);
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setItemSku(itemVo.getItemSku());
    itemPickupPointVo.setFbbActivated(true);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    productVo.setFbbActivated(true);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    org.springframework.beans.BeanUtils.copyProperties(itemDTO, itemActivationRequest);
    itemActivationRequest.setForceReview(true);
    itemPickupPointActivationRequest.setFbbActivated(true);
    itemActivationRequest.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));
    when(this.gdnMapperHelper.mapBean(any(ProductDTO.class), eq(ProductVo.class)))
        .thenReturn(productVo);
    when(this.gdnMapperHelper.mapBean(any(ItemDTO.class), eq(ItemVo.class)))
        .thenReturn(itemVo);
    when(this.listUtil.distinct(Mockito.anyList(), any())).thenReturn(Arrays.asList(itemVo));
    ProductItemsVo result = this.modelConverterImpl.convertToProductAndItemsRequestVO(
        new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest)));
    verify(this.gdnMapperHelper, times(2)).mapBean(any(), any());
    verify(this.listUtil).distinct(Mockito.anyList(), any());
    assertNotNull(result);
    assertTrue(result.getProductVo().getPreOrder().getIsPreOrder());
    Assertions.assertTrue(result.getProductVo().isTakenDown());
    Assertions.assertTrue(result.getProductVo().isForceReview());
    assertEquals(PREORDER_TYPE, result.getProductVo().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, result.getProductVo().getPreOrder().getPreOrderValue());
    assertTrue(result.getProductVo().isFbbActivated());
  }

  @Test
  public void convertToProductAndItemsRequestVO_WithPreOrderFalseAndDateNullTest() {
    ProductAndItemsVO productAndItemsRequestVO = new ProductAndItemsVO(null, this.itemList);
    preOrderDTO.setIsPreOrder(false);
    preOrderDTO.setPreOrderDate(null);
    preOrderDTO.setPreOrderType(null);
    preOrderDTO.setPreOrderValue(null);
    productDTO.setPreOrder(preOrderDTO);
    productDTO.setProductSku(PRODUCT_SKU);
    productAndItemsRequestDTO.setProduct(productDTO);
    productAndItemsRequestDTO.setItems(Arrays.asList(itemDTO));
    preOrder.setIsPreOrder(false);
    preOrder.setPreOrderDate(null);
    preOrder.setPreOrderType(null);
    preOrder.setPreOrderValue(null);
    productObject.setPreOrder(preOrder);
    productObject.setProductSku(PRODUCT_SKU);
    productAndItemsRequestVO.setProduct(productObject);
    productVo.setPreOrder(preOrder);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    org.springframework.beans.BeanUtils.copyProperties(itemDTO, itemActivationRequest);
    itemActivationRequest.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));
    when(this.gdnMapperHelper.mapBean(any(ProductDTO.class), eq(ProductVo.class)))
        .thenReturn(productVo);
    when(this.gdnMapperHelper.mapBean(any(ItemDTO.class), eq(ItemVo.class)))
        .thenReturn(itemVo);
    ProductItemsVo result =
        this.modelConverterImpl.convertToProductAndItemsRequestVO(new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest)));
    verify(this.gdnMapperHelper, times(2)).mapBean(any(), any());
    verify(this.listUtil).distinct(Mockito.anyList(), any());
    assertNotNull(result);
    assertFalse(result.getProductVo().getPreOrder().getIsPreOrder());
    assertNull(result.getProductVo().getPreOrder().getPreOrderType());
    assertNull(result.getProductVo().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemsRequestVO_WithDisablePreOrderDelaySwitchOffTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "disablePreOrderDelay", false);
    ProductAndItemsVO productAndItemsRequestVO = new ProductAndItemsVO(null, this.itemList);
    preOrderDTO.setIsPreOrder(true);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    preOrderDTO.setPreOrderDate(cal.getTime());
    preOrderDTO.setPreOrderType(null);
    preOrderDTO.setPreOrderValue(null);
    productDTO.setPreOrder(preOrderDTO);
    productDTO.setProductSku(PRODUCT_SKU);
    productAndItemsRequestDTO.setProduct(productDTO);
    productAndItemsRequestDTO.setItems(Arrays.asList(itemDTO));
    preOrder.setIsPreOrder(false);
    preOrder.setPreOrderDate(null);
    preOrder.setPreOrderType(null);
    preOrder.setPreOrderValue(null);
    productObject.setPreOrder(preOrder);
    productObject.setProductSku(PRODUCT_SKU);
    productAndItemsRequestVO.setProduct(productObject);
    productVo.setPreOrder(preOrder);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    org.springframework.beans.BeanUtils.copyProperties(itemDTO, itemActivationRequest);
    itemActivationRequest.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));
    when(this.gdnMapperHelper.mapBean(any(ProductDTO.class), eq(ProductVo.class)))
      .thenReturn(productVo);
    when(this.gdnMapperHelper.mapBean(any(ItemDTO.class), eq(ItemVo.class)))
      .thenReturn(itemVo);
    ProductItemsVo result =
      this.modelConverterImpl.convertToProductAndItemsRequestVO(new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest)));
    verify(this.gdnMapperHelper, times(2)).mapBean(any(), any());
    verify(this.listUtil).distinct(Mockito.anyList(), any());
    assertNotNull(result);
    assertTrue(result.getProductVo().getPreOrder().getIsPreOrder());
    cal.add(Calendar.HOUR_OF_DAY, 7);
    assertEquals(cal.getTime(), result.getProductVo().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemsRequestVO_WithDisablePreOrderDelaySwitchOnTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "disablePreOrderDelay", true);
    ProductAndItemsVO productAndItemsRequestVO = new ProductAndItemsVO(null, this.itemList);
    preOrderDTO.setIsPreOrder(true);
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    preOrderDTO.setPreOrderDate(cal.getTime());
    preOrderDTO.setPreOrderType(null);
    preOrderDTO.setPreOrderValue(null);
    productDTO.setPreOrder(preOrderDTO);
    productDTO.setProductSku(PRODUCT_SKU);
    productAndItemsRequestDTO.setProduct(productDTO);
    productAndItemsRequestDTO.setItems(Arrays.asList(itemDTO));
    preOrder.setIsPreOrder(false);
    preOrder.setPreOrderDate(null);
    preOrder.setPreOrderType(null);
    preOrder.setPreOrderValue(null);
    productObject.setPreOrder(preOrder);
    productObject.setProductSku(PRODUCT_SKU);
    productAndItemsRequestVO.setProduct(productObject);
    productVo.setPreOrder(preOrder);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    org.springframework.beans.BeanUtils.copyProperties(itemDTO, itemActivationRequest);
    itemActivationRequest.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));
    when(this.gdnMapperHelper.mapBean(any(ProductDTO.class), eq(ProductVo.class)))
      .thenReturn(productVo);
    when(this.gdnMapperHelper.mapBean(any(ItemDTO.class), eq(ItemVo.class)))
      .thenReturn(itemVo);
    ProductItemsVo result =
      this.modelConverterImpl.convertToProductAndItemsRequestVO(new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest)));
    verify(this.gdnMapperHelper, times(2)).mapBean(any(), any());
    verify(this.listUtil).distinct(Mockito.anyList(), any());
    assertNotNull(result);
    assertTrue(result.getProductVo().getPreOrder().getIsPreOrder());
    assertEquals(cal.getTime(), result.getProductVo().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemsRequestVO_WithNullPreOrderTest() {
    ProductAndItemsVO productAndItemsRequestVO = new ProductAndItemsVO(null, this.itemList);
    productDTO.setPreOrder(null);
    productDTO.setProductSku(PRODUCT_SKU);
    productAndItemsRequestDTO.setProduct(productDTO);
    productAndItemsRequestDTO.setItems(Arrays.asList(itemDTO));
    productObject.setPreOrder(null);
    productObject.setProductSku(PRODUCT_SKU);
    productAndItemsRequestVO.setProduct(productObject);
    productVo.setPreOrder(preOrder);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    org.springframework.beans.BeanUtils.copyProperties(itemDTO, itemActivationRequest);
    itemActivationRequest.setItemPickupPoints(Arrays.asList(itemPickupPointActivationRequest));
    when(this.gdnMapperHelper.mapBean(any(ProductDTO.class), eq(ProductVo.class))).thenReturn(productVo);
    when(this.gdnMapperHelper.mapBean(any(ItemDTO.class), eq(ItemVo.class))).thenReturn(itemVo);
    try {
      Assertions.assertThrows(IllegalArgumentException.class, () -> this.modelConverterImpl.convertToProductAndItemsRequestVO(
          new ProductAndItemActivationRequest(productDTO, Arrays.asList(itemActivationRequest))));
    } finally {
      verify(this.gdnMapperHelper, times(2)).mapBean(any(), any());
      verify(this.listUtil).distinct(Mockito.anyList(), any());
    }
  }

  @Test
  public void convertToProductAndItemsRequestVOTestWithNullProductAndItemsRequest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProductAndItemsRequestVO(null));
  }

  @Test
  public void convertToProductResponseRestWeb() {
    ProductResponse result = this.modelConverterImpl.convertToProductResponse(this.productObject);
    assertEquals(result, (this.productResponse));
  }

  @Test
  public void convertToProductResponseRestWebWithNullProduct() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProductResponse(null));
  }

  @Test
  public void convertToProductResponseRestList() {
    List<ProductResponse> result = this.modelConverterImpl.convertToProductResponseList(Arrays.asList(productObject));
    assertEquals(result, (Arrays.asList(this.productResponse)));
  }

  @Test
  public void convertToProductSummaryResponseTest() {
    ProductAndItemSolr productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    productAndItemSolr.setMasterCatalog(ModelConverterImplTest.MASTER_CATALOG_STRING);
    productAndItemSolr.setSalesCatalog(ModelConverterImplTest.SALES_CATALOG_LIST);
    List<ProductAndItemSolr> productAndItems = Arrays.asList(productAndItemSolr);
    when(this.solrPage.getContent()).thenReturn(productAndItems);
    when(this.gdnMapperHelper.mapBean(any(), eq(ProductSummaryResponse.class)))
        .thenReturn(new ProductSummaryResponse());
    this.modelConverterImpl
        .convertToProductSummaryResponse(this.solrPage, ModelConverterImplTest.REQUEST_ID, ModelConverterImplTest.PAGE,
            ModelConverterImplTest.SIZE);
    verify(this.solrConstructorService).constructMasterCatalog(ModelConverterImplTest.MASTER_CATALOG_STRING);
    verify(this.solrConstructorService).constructSalesCatalogs(ModelConverterImplTest.SALES_CATALOG_LIST);
    verify(this.gdnMapperHelper).mapBean(any(), eq(ProductSummaryResponse.class));
  }

  @Test
  public void convertToProductViewConfigResponseTest() {
    ItemViewConfigDTO result = this.modelConverterImpl.convertToProductViewConfigResponse(this.viewConfig);

    verify(this.gdnMapperHelper).mapBean(this.viewConfig, ItemViewConfigDTO.class);
    assertNotNull(result);
    assertEquals(result, (this.productViewConfigResponse));
  }

  @Test
  public void convertToProductViewConfigResponseTestWitNullProductViewConfig() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProductViewConfigResponse(null));
  }

  @Test
  public void convertToProductViewConfigResponseTestWitNullProductViewConfigRequest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToItemViewConfig(null));
  }

  @Test
  public void convertToProductViewConfigTest() {
    ItemViewConfig result = this.modelConverterImpl.convertToItemViewConfig(this.productViewConfigRequest);

    verify(this.gdnMapperHelper).mapBean(this.productViewConfigRequest, ItemViewConfig.class);
    assertNotNull(result);
    assertEquals(result, (this.viewConfig));
  }

  @Test
  public void convertToItemViewConfigTest() {
    Map<String, ItemViewConfig> response =
        modelConverterImpl.convertToItemViewConfigMap(Collections.singletonList(itemViewConfigAndItemSkuRequest));
    Assertions.assertTrue(response.containsKey(ITEM_SKU));
    Assertions.assertTrue(response.get(ITEM_SKU).isBuyable());
    Assertions.assertTrue(response.get(ITEM_SKU).isDiscoverable());
    Assertions.assertTrue(response.get(ITEM_SKU).getItemDiscoverableSchedules().isDiscoverable());
    Assertions.assertTrue(response.get(ITEM_SKU).getItemBuyableSchedules().isBuyable());
  }

  @Test
  public void convertToProductWithNullProductRequest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProduct(null));
  }

  @Test
  public void convertToSalesCatalogResponseTest() {
    this.modelConverterImpl.convertToSalesCatalogResponse(this.salesCatalog);
    verify(this.gdnMapperHelper).mapBean(this.salesCatalog, SalesCatalogDTO.class);
  }

  @Test
  public void convertToSalesCatalogResponseTestWithNullSalesCatalog() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.modelConverterImpl.convertToSalesCatalogResponse(null));
  }

  @Test
  public void convertToSalesCatalogTestWithNullSalesCatalog() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToSalesCatalog(null));
  }

  @Test
  public void convertToSalesCatalogTestWithNullSalesCatalogRequest() {
    this.modelConverterImpl.convertToSalesCatalog(this.salesCatalogRequest);

    verify(this.gdnMapperHelper).mapBean(this.salesCatalogRequest, SalesCatalog.class);
  }

  @Test
  public void convertToSimpleProductRequestVoTest() {
    ArrayList<SimpleProductDTO> simpleProductDTOs = new ArrayList<>();
    SimpleProductDTO simpleProduct = new SimpleProductDTO();
    simpleProduct.setProductCode(ModelConverterImplTest.PRODUCT_CODE);
    simpleProduct.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    simpleProductDTOs.add(simpleProduct);
    SimpleProductListRequest simpleProductListRequest = new SimpleProductListRequest(simpleProductDTOs);

    List<SimpleProductRequestVo> expectedResult = new ArrayList<>();
    SimpleProductRequestVo requestVo = new SimpleProductRequestVo();
    requestVo.setProductCode(ModelConverterImplTest.PRODUCT_CODE);
    requestVo.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    expectedResult.add(requestVo);

    List<SimpleProductRequestVo> result =
        this.modelConverterImpl.convertToSimpleProductRequestVo(simpleProductListRequest);

    assertEquals(result, (expectedResult));
  }

  @Test
  public void convertToSimpleProductResponsesTest() {
    List<Product> products = new ArrayList<>();
    Product product = this.setupProduct();
    products.add(product);

    ArrayList<SimpleProductDTO> simpleProducts = new ArrayList<>();
    SimpleProductDTO simpleProduct = new SimpleProductDTO();
    simpleProduct.setProductCode(ModelConverterImplTest.PRODUCT_CODE);
    simpleProduct.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    simpleProducts.add(simpleProduct);
    SimpleProductListResponse expectedResult = new SimpleProductListResponse(simpleProducts);

    SimpleProductListResponse result = this.modelConverterImpl.convertToSimpleProductResponses(products);

    assertEquals(result, (expectedResult));
  }

  @Test
  public void convertToSimpleProductsAndItemsDTOTest() {

    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();
    List<Item> itemList = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    Product product = setupProduct();
    Item item = setupItem();
    itemList.add(item);
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(itemList);
    productAndItemsVOList.add(productAndItemsVO);
    masterDataDetailWithProductAndItemsResponseVo.setProductAndItems(productAndItemsVOList);
    List<SimpleProductsAndItemsResponse> response =
        this.modelConverterImpl.convertToSimpleProductsAndItemsDTO(masterDataDetailWithProductAndItemsResponseVo);
    assertEquals(1, response.size());
    assertNotNull(response);
    assertEquals(ModelConverterImplTest.PRODUCT_SKU, response.get(0).getSimpleProductDTO().getProductSku());

  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.viewConfig = new ItemViewConfig();
    viewConfig.setChannel(Constants.DEFAULT);
    this.productViewConfigRequest = new ItemViewConfigRequest();
    productViewConfigRequest.setChannel(Constants.DEFAULT);
    this.productViewConfigResponse = new ItemViewConfigDTO();

    this.handlingFeeRequestRestWebList = new ArrayList<>();
    this.handlingFeeRequestRestWeb = new HandlingFeeRequestRestWeb();
    this.handlingFeeRequestRestWeb.setItemSku(ModelConverterImplTest.CATENTRY_ID);
    this.handlingFeeRequestRestWeb.setQuantity(ModelConverterImplTest.QUANTITY);
    this.handlingFeeRequestRestWebList.add(this.handlingFeeRequestRestWeb);

    this.handlingFeeResponse = new HandlingFeeResponse();
    this.handlingFeeResponse.setTotalHandlingFee(ModelConverterImplTest.TOTAL_HANDLING_FEE);

    this.mapper = new ObjectMapper();

    this.insertInventoryLevel2Request =
        FileUtils.readFileToString(new File(ModelConverterImplTest.PATH_FILE_INSERT_INVENTORY_LEVEL2_REQUEST_JSON));

    this.transactionVO = new ProductForTransactionVO();
    this.transactionVO.setItemSku(ModelConverterImplTest.ITEM_SKU);

    this.productResponseRestWeb = new ProductForTransactionResponse();
    this.productResponseRestWeb.setItemSku(ModelConverterImplTest.ITEM_SKU);

    this.systemParameter = new SystemParameter();
    this.systemParameter.setStoreId(ModelConverterImplTest.STORE_ID);
    this.systemParameter.setValue(SYSTEM_PARAMETER_VALUE);

    this.systemParameterForSystemResponse = new SystemParameter();
    this.systemParameterForSystemResponse.setStoreId(ModelConverterImplTest.STORE_ID);
    this.systemParameterForSystemResponse.setDescription(ModelConverterImplTest.DESCRIPTION);
    this.systemParameterForSystemResponse.setValue(ModelConverterImplTest.VALUE_FORMAT1);
    this.systemParameterForSystemResponse.setVariable(ModelConverterImplTest.VARIABLE);

    this.systemParameterRequest = new SystemParameterRequest();
    this.systemParameterRequest.setDescription(ModelConverterImplTest.DESCRIPTION);
    this.systemParameterRequest.setValue(ModelConverterImplTest.VALUE_FORMAT1);
    this.systemParameterRequest.setVariable(ModelConverterImplTest.VARIABLE);

    this.itemDetail_1 = new ProductItemDetailVO.ItemDetailVOBuilder().itemName(ModelConverterImplTest.ITEM_NAME)
        .itemSku(ModelConverterImplTest.ITEM_SKU).merchantCode(ModelConverterImplTest.MERCHANTID).build();

    this.itemDetails = Arrays.asList(this.itemDetail_1);

    this.itemDetailRestWeb =
        new ItemDetailRestWeb.ItemDetailRestWebBuilder().setItemName(ModelConverterImplTest.ITEM_NAME)
            .setItemSku(ModelConverterImplTest.ITEM_SKU).setMerchantCode(ModelConverterImplTest.MERCHANTID).build();

    this.itemPrice = new ItemPriceVO.ItemPriceBuilder().setItemSku(ModelConverterImplTest.ITEM_PRICE)
        .setOfferPrice(ModelConverterImplTest.OFFER_PRICE).setBuyable(ModelConverterImplTest.BUYABLE).build();

    this.itemPriceList = Arrays.asList(this.itemPrice);

    this.itemPriceRestWeb =
        new SimpleItemResponse.SimpleItemResponseWebBuilder().setItemSku(ModelConverterImplTest.ITEM_PRICE)
            .setOfferPrice(ModelConverterImplTest.OFFER_PRICE).setBuyable(ModelConverterImplTest.BUYABLE).build();

    this.productRequest = new ProductRequest();
    this.productObject = new Product();
    this.productResponse = new ProductResponse();

    when(this.gdnMapperHelper.mapBean(this.transactionVO, ProductForTransactionResponse.class))
        .thenReturn(this.productResponseRestWeb);

    when(this.gdnMapperHelper.mapBean(this.productRequest, Product.class)).thenReturn(this.productObject);
    when(this.gdnMapperHelper.mapBean(this.productObject, ProductResponse.class)).thenReturn(this.productResponse);

    // item request
    this.itemRequest1 = new ItemRequest();
    this.salePriceRequest = new DiscountPriceDTO();

    this.priceRequest = new PriceRequest();

    this.hashSetPriceRequest = new HashSet<>();
    this.hashSetPriceRequest.add(this.priceRequest);

    this.masterItemAttributeValueRequest = new MasterDataItemAttributeValueDTO();

    this.masterItemAttributeValuesRequest = new ArrayList<>();
    this.masterItemAttributeValuesRequest.add(this.masterItemAttributeValueRequest);

    this.masterItemImageRequest = new MasterDataItemImageDTO();

    this.masterItemImagesRequest = new ArrayList<>();
    this.masterItemImagesRequest.add(this.masterItemImageRequest);

    this.masterDataItemRequest = new MasterDataItemDTO();
    this.masterDataItemRequest.setMasterDataItemAttributeValues(this.masterItemAttributeValuesRequest);
    this.masterDataItemRequest.setMasterDataItemImages(this.masterItemImagesRequest);

    this.itemRequest1 = new ItemRequest();
    this.itemRequest1.setPrice(this.hashSetPriceRequest);
    this.itemRequest1.setMasterDataItem(this.masterDataItemRequest);

    // item
    this.salePrice = new DiscountPrice();

    this.price1 = new Price();

    this.hashSetPrice = new HashSet<>();
    this.hashSetPrice.add(this.price1);

    this.masterItemAttributeValue = new MasterDataItemAttributeValue();
    this.masterItemAttributeValue.setMasterDataAttribute(new MasterDataAttribute());
    this.masterItemAttributeValues = new ArrayList<>();
    this.masterItemAttributeValues.add(this.masterItemAttributeValue);

    this.masterItemImage = new MasterDataItemImage();
    masterItemImage.setSequence(0);
    masterItemImage.setMainImage(true);
    masterItemImage.setLocationPath("xyz");
    this.masterItemImages = new ArrayList<>();
    this.masterItemImages.add(this.masterItemImage);

    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setMasterDataItemAttributeValues(this.masterItemAttributeValues);
    this.masterDataItem.setMasterDataItemImages(this.masterItemImages);

    this.itemDetail = new Item();
    this.itemDetail.setPrice(this.hashSetPrice);
    this.itemDetail.setMasterDataItem(this.masterDataItem);

    when(this.gdnMapperHelper.mapBean(this.itemRequest1, Item.class)).thenReturn(this.itemDetail);

    // item response
    this.salePriceResponse = new DiscountPriceDTO();

    this.priceResponse = new PriceDTO();
    this.listOfSalePriceResponse = new ArrayList<>();
    this.listOfSalePriceResponse.add(this.salePriceResponse);
    this.priceResponse.setListOfDiscountPrices(this.listOfSalePriceResponse);

    this.hashSetPriceResponse = new HashSet<>();
    this.hashSetPriceResponse.add(this.priceResponse);

    this.masterItemAttributeValueResponse = new MasterDataItemAttributeValueDTO();

    this.masterItemAttributeValuesResponse = new ArrayList<>();
    this.masterItemAttributeValuesResponse.add(this.masterItemAttributeValueResponse);

    this.masterItemImageResponse = new MasterDataItemImageDTO();

    this.masterItemImagesResponse = new ArrayList<>();
    this.masterItemImagesResponse.add(this.masterItemImageResponse);

    this.masterDataItemResponse = new MasterDataItemDTO();
    this.masterDataItemResponse.setMasterDataItemAttributeValues(this.masterItemAttributeValuesResponse);
    this.masterDataItemResponse.setMasterDataItemImages(this.masterItemImagesResponse);

    this.itemResponse1 = new ItemResponse();
    this.itemResponse1.setPrice(this.hashSetPriceResponse);
    this.itemResponse1.setMasterDataItem(this.masterDataItemResponse);

    when(this.gdnMapperHelper.mapBean(this.itemDetail, ItemResponse.class)).thenReturn(this.itemResponse1);

    this.priceRequest = new PriceRequest();
    this.priceRequest.setChannel(ModelConverterImplTest.CHANNEL_ALL);
    this.priceRequest.setCurrency(ModelConverterImplTest.CURRENCY);
    this.priceRequest.setOfferPrice(ModelConverterImplTest.OFFER_PRICE);

    this.price1 = new Price();
    this.price1.setChannel(ModelConverterImplTest.CHANNEL_ALL);
    this.price1.setCurrency(ModelConverterImplTest.CURRENCY);
    this.price1.setOfferPrice(ModelConverterImplTest.OFFER_PRICE);

    when(this.gdnMapperHelper.mapBean(this.priceRequest, Price.class)).thenReturn(this.price1);

    this.priceResponse.setChannel(ModelConverterImplTest.CHANNEL_ALL);
    this.priceResponse.setCurrency(ModelConverterImplTest.CURRENCY);
    this.priceResponse.setOfferPrice(ModelConverterImplTest.OFFER_PRICE);

    when(this.gdnMapperHelper.mapBean(this.price1, PriceDTO.class)).thenReturn(this.priceResponse);

    this.salePriceRequest.setStartDateTime(ModelConverterImplTest.START_DATE_TIME);
    this.salePriceRequest.setEndDateTime(ModelConverterImplTest.END_DATE_TIME);
    this.salePriceRequest.setDiscountPrice(ModelConverterImplTest.SALE_PRICE);

    this.salePriceResponse.setStartDateTime(ModelConverterImplTest.START_DATE_TIME);
    this.salePriceResponse.setEndDateTime(ModelConverterImplTest.END_DATE_TIME);
    this.salePriceResponse.setDiscountPrice(ModelConverterImplTest.SALE_PRICE);

    this.productForGetProductAndItems = new Product();
    this.productForGetProductAndItems.setStoreId(DEFAULT_STORE_ID);
    this.productForGetProductAndItems.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    this.productForGetProductAndItems.setForceReview(true);
    this.productForGetProductAndItems.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setVariantCreation(true);
    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    masterDataProduct.setMasterDataProductAttributes(Arrays.asList(masterDataProductAttribute));
    this.productForGetProductAndItems.setMasterDataProduct(masterDataProduct);

    this.itemAForGetProductAndItems = new Item();
    this.itemAForGetProductAndItems.setMasterDataItem(this.masterDataItem);
    this.itemAForGetProductAndItems.setItemSku(ModelConverterImplTest.ITEM_SKU);

    this.itemBForGetProductAndItems = new Item();
    this.itemBForGetProductAndItems.setMasterDataItem(this.masterDataItem);
    this.itemAForGetProductAndItems.setItemSku(ModelConverterImplTest.ITEM_SKU_B);
    this.itemAForGetProductAndItems.setWholesalePriceExists(true);
    this.itemAForGetProductAndItems.setActivePromoBundlings(new HashSet<>());

    this.itemCForGetProductAndItems = new Item();
    this.itemCForGetProductAndItems.setMasterDataItem(this.masterDataItem);
    this.itemCForGetProductAndItems.setItemSku(ModelConverterImplTest.ITEM_SKU_B);

    this.itemList = new ArrayList<>();
    this.itemList.add(this.itemAForGetProductAndItems);
    this.itemList.add(this.itemBForGetProductAndItems);

    this.productanditemsvo = new ProductAndItemsVO();
    this.productanditemsvo.setProduct(this.productForGetProductAndItems);
    this.productanditemsvo.setItems(this.itemList);

    // error
    // this.productAndItemsVO.setListOfItemAndStockVO(this.listOfItemAndStockVO);

    this.itemAndStockADTOForGet = new ItemResponse();
    this.itemAndStockADTOForGet.setItemSku(ModelConverterImplTest.ITEM_SKU);
    this.itemAndStockBDTOForGet = new ItemResponse();
    this.itemAndStockBDTOForGet.setItemSku(ModelConverterImplTest.ITEM_SKU);

    this.listOfItemAndStockDTO = new ArrayList<>();
    this.listOfItemAndStockDTO.add(this.itemAndStockADTOForGet);
    this.listOfItemAndStockDTO.add(this.itemAndStockBDTOForGet);

    this.productAndItemsResponse1 = new ProductAndItemsResponse();
    this.productAndItemsResponse1.setProduct(new ProductResponse());
    this.productAndItemsResponse1.getProduct().setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    this.productAndItemsResponse1.setItems(this.listOfItemAndStockDTO);

    this.masterCatalog1 = new MasterCatalog();
    this.masterCatalogRequest = new MasterCatalogRequest();
    this.masterCatalogResponse = new MasterCatalogDTO();
    this.salesCatalog = new SalesCatalog();
    this.salesCatalogRequest = new SalesCatalogRequest();
    this.salesCatalogResponse = new SalesCatalogDTO();

    this.productAndItemsRequestDTO = new ProductAndItemsRequest();

    this.itemDTO = new ItemDTO();
    this.productDTO = new ProductDTO();

    productResponseForGetProductAndItems = new ProductResponse();
    productResponseForGetProductAndItems.setProductSku(PRODUCT_SKU);
    productResponseForGetProductAndItems.setForceReview(true);
    productL3Response = new ProductL3Response();
    productL3Response.setProductSku(PRODUCT_SKU);
    when(gdnMapperHelper.mapBean(productForGetProductAndItems, ProductResponse.class))
        .thenReturn(productResponseForGetProductAndItems);
    when(gdnMapperHelper.mapBean(productForGetProductAndItems, ProductL3Response.class))
        .thenReturn(productL3Response);
    when(gdnMapperHelper.mapBean(itemAForGetProductAndItems, ItemResponse.class)).thenReturn(itemAndStockADTOForGet);
    when(gdnMapperHelper.mapBean(itemBForGetProductAndItems, ItemResponse.class)).thenReturn(itemAndStockBDTOForGet);

    when(this.gdnMapperHelper.mapBean(this.masterCatalogRequest, MasterCatalog.class)).thenReturn(this.masterCatalog1);
    when(this.gdnMapperHelper.mapBean(this.masterCatalog1, MasterCatalogDTO.class))
        .thenReturn(this.masterCatalogResponse);
    when(this.gdnMapperHelper.mapBean(this.salesCatalogRequest, SalesCatalog.class)).thenReturn(this.salesCatalog);
    when(this.gdnMapperHelper.mapBean(this.salesCatalog, SalesCatalogDTO.class)).thenReturn(this.salesCatalogResponse);

    when(this.gdnMapperHelper.mapBean(this.viewConfig, ItemViewConfigDTO.class))
        .thenReturn(this.productViewConfigResponse);
    when(this.gdnMapperHelper.mapBean(any(), eq(ItemViewConfig.class)))
        .thenReturn(this.viewConfig);


    when(this.gdnMapperHelper.mapBean(this.productDTO, Product.class)).thenReturn(this.productObject);

    when(this.gdnMapperHelper.mapBean(this.itemDTO, Item.class)).thenReturn(this.itemRequestVO);

    this.productSimple = new Product();
    this.productSimple.setMerchantCode(MERCHANT_ID);
    this.productSimple.setProductType(PRODUCT_TYPE);
    this.productSimple.setProductCatentryId(PRODUCT_CATENTRY_ID);
    this.productSimple.setProductSku(PRODUCT_SKU);
    this.productSimple.setMasterDataProduct(new MasterDataProduct());
    this.productSimple.getMasterDataProduct().setBrand(BRAND);

    this.itemSimple = new Item();
    this.itemSimple.setItemSku(ITEM_SKU);
    this.itemSimple.setMerchantSku(MERCHANT_SKU);
    this.itemSimple.setItemCatentryId(CATENTRY_ID);
    this.itemSimple.getPrice().add(this.price1);
    this.itemSimple.setMasterDataItem(this.masterDataItem);
    Set<ItemViewConfig> itemViewConfig2 = new HashSet<>();
    itemViewConfig2.add(new ItemViewConfig(true, true, null, null, null));
    this.itemSimple.setItemViewConfigs(itemViewConfig2);

    productInfoResponse = new ProductInfoResponse();
    productInfoResponse.setProductSku(PRODUCT_SKU);
    itemInfoResponse = new ItemInfoResponse();
    itemInfoResponse.setItemSku(ITEM_SKU);
    priceDTO = new PriceDTO();
    priceDTO.setChannel(DEFAULT_CHANNEL);
    itemInfoResponse.setPrice(Stream.of(priceDTO).collect(toSet()));
    itemViewConfigDTO = new ItemViewConfigDTO(true, true, DEFAULT_CHANNEL, null, null);
    itemInfoResponse.setItemViewConfigs(Stream.of(itemViewConfigDTO).collect(toSet()));
    productAndItemInfoResponse = new ProductAndItemInfoResponse(productInfoResponse, itemInfoResponse);

    itemInfoResponseV2 = new ItemInfoResponseV2();
    itemInfoResponseV2.setItemSku(ITEM_SKU);
    productAndItemInfoResponseV2 = new ProductAndItemInfoResponseV2(productInfoResponse, itemInfoResponseV2);

    comboDetailVo = new ComboDetailVo();
    comboDetailVo.setTotal(TOTAL_OTHER_COMBO);

    comboDetailResponse = new ComboDetailResponse();
    comboDetailResponse.setTotal(TOTAL_OTHER_COMBO);

    productTypeByProductSkuMap = new HashMap<>();

    this.upsertOfflineItemRequest = new UpsertOfflineItemRequest();
    this.upsertOfflineItemRequest.setOfflineItemId(OFFLINE_ITEM_ID);
    this.upsertOfflineItemRequest.setItemSku(ITEM_SKU);
    this.upsertOfflineItemRequest.setOfferPrice(OFFER_PRICE);
    this.upsertOfflineItemRequest.setPickupPointCode(PICKUP_POINT_CODE);

    this.upsertOfflineItemRequests = new ArrayList<>();
    this.upsertOfflineItemRequests.add(this.upsertOfflineItemRequest);
    when(this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE))
        .thenReturn(systemParameter);

    categoryDTO.setCategoryCode(SYSTEM_PARAMETER_VALUE);

    itemViewConfigAndItemSkuRequest = new ItemViewConfigAndItemSkuRequest();
    itemViewConfigAndItemSkuRequest.setItemSku(ITEM_SKU);
    itemViewConfigAndItemSkuRequest.setChannel(Constants.DEFAULT);
    itemViewConfigAndItemSkuRequest.setDiscoverable(true);
    itemViewConfigAndItemSkuRequest.setBuyable(true);
    ItemBuyableScheduleDTO itemBuyableScheduleDTO = new ItemBuyableScheduleDTO();
    itemBuyableScheduleDTO.setBuyable(true);
    ItemDiscoverableScheduleDTO itemDiscoverableScheduleDTO = new ItemDiscoverableScheduleDTO();
    itemDiscoverableScheduleDTO.setDiscoverable(true);
    itemViewConfigAndItemSkuRequest.setItemBuyableSchedules(itemBuyableScheduleDTO);
    itemViewConfigAndItemSkuRequest.setItemDiscoverableSchedules(itemDiscoverableScheduleDTO);

    masterDataDetailWithProductAndItemsResponse = new MasterDataDetailWithProductAndItemsResponse();
    ProductAndItemsDTO productAndItemsDTO = new ProductAndItemsDTO();
    itemResponse1.setSourceItemCode(SOURCE_ITEM_CODE);
    itemResponse1.setContentChanged(true);
    productAndItemsDTO.setItems(Arrays.asList(itemResponse1));
    ProductAndItemsDTO productAndItemsDTO1 = new ProductAndItemsDTO();
    productAndItemsDTO1.setItems(Arrays.asList(itemResponse1));
    productAndItemsDTO1.setProduct(productResponse);
    masterDataDetailWithProductAndItemsResponse.setProductAndItems(Arrays.asList(productAndItemsDTO1));

    categoryResponse = new CategoryResponse();
    categoryResponse.setDocumentType(DOCUMENT_TYPE);

    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);

    preOrderDTO = new PreOrderDTO();
    preOrderDTO.setIsPreOrder(true);
    preOrderDTO.setPreOrderType(PREORDER_TYPE);
    preOrderDTO.setPreOrderValue(PREORDER_VALUE);
    preOrderDTO.setPreOrderDate(cal.getTime());

    preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(PREORDER_TYPE);
    preOrder.setPreOrderValue(PREORDER_VALUE);
    preOrder.setPreOrderDate(cal.getTime());

    preOrderVO =
        PreOrderVO.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE).build();

    quickEditUpdateRequest = new QuickEditUpdateRequest();
    quickEditUpdateRequest.setItemSku(ITEM_SKU);
    quickEditUpdateRequest.setPrice(priceDTO);
    quickEditUpdateRequest.setWholeSaleActivated(true);
    quickEditUpdateRequest.setOff2OnActiveFlag(true);
    quickEditUpdateRequest.setSellerSku(MERCHANT_SKU);
    quickEditUpdateRequest.setPickupPointCode(PICKUP_POINT_CODE);

    itemPickupPointQuickEditRequest1 = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest1.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest1.setPrice(priceDTO);
    itemPickupPointQuickEditRequest1.setWholeSaleActivated(true);
    itemPickupPointQuickEditRequest1.setOff2OnActiveFlag(true);
    itemPickupPointQuickEditRequest1.setMerchantSku(MERCHANT_SKU);
    itemPickupPointQuickEditRequest1.setPickupPointCode(PICKUP_POINT_CODE);

    preOrderVO = PreOrderVO.builder().isPreOrder(true).preOrderType(PREORDER_TYPE).preOrderValue(PREORDER_VALUE)
        .preOrderDate(cal.getTime()).build();

    BeanUtils.copyProperties(productVo, productanditemsvo.getProduct());
    BeanUtils.copyProperties(itemVo, productanditemsvo.getItems().get(0));
    productItemsVo = new ProductItemsVo(productVo, Collections.singletonList(itemVo));
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setName(PICKUP_POINT_NAME);
    businessPartnerPickupPoint1.setCode(PICKUP_POINT_CODE_1);
    businessPartnerPickupPoint1.setName(PICKUP_POINT_NAME_1);

    ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest = new ItemPickupPointQuickEditRequest();
    itemPickupPointQuickEditRequest.setItemSku(ITEM_SKU);
    itemPickupPointQuickEditRequest.setPrice(priceDTO);
    ItemPickupPointDeleteRequest itemPickupPointDeleteRequest = new ItemPickupPointDeleteRequest();
    itemPickupPointDeleteRequest.setItemSku(ITEM_SKU);
    itemPickupPointUpdateRequest.setAddPickupPointRequests(Collections.singletonList(itemPickupPointQuickEditRequest));
    itemPickupPointUpdateRequest.setDeletePickupPointRequests(Collections.singletonList(itemPickupPointDeleteRequest));
    itemPickupPointUpdateRequest.setQuickEditUpdateRequests(Collections.singletonList(itemPickupPointQuickEditRequest));

    basicProductAndItemDTO.setPreOrder(preOrder);
    basicProductAndItemDTO.setMasterDataProductAttributes(Arrays.asList(masterDataProductAttribute));
    basicProductAndItemDTO.setItemCatalogVOS(Arrays.asList(new ItemCatalogVO()));

    basicItemDTO = new BasicItemDTO();
    basicItemDTO.setItemViewConfigs(ALL_VIEW_CONFIGS);
  }


  private Item setupItem() {
    Item item = new Item();
    item.setItemSku(ModelConverterImplTest.ITEM_SKU);
    item.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    item.setMerchantSku(ModelConverterImplTest.MERCHANT_SKU);
    item.setItemCode(ModelConverterImplTest.ITEM_CODE);
    item.setSynchronized(ModelConverterImplTest.IS_SYNCHRONIZED);
    item.setItemCatentryId(ModelConverterImplTest.ITEM_CATENTRY_ID);
    item.setContentChanged(true);
    item.setSourceItemCode(SOURCE_ITEM_CODE);
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(ModelConverterImplTest.PRISTINE_ID);
    pristineDataItem.setPristineProductName(ModelConverterImplTest.PRISTINE_PRODUCT_NAME);
    item.setPristineDataItem(pristineDataItem);
    List<MasterDataItemImage> masterDataItemImages = new ArrayList<>();
    masterDataItemImages.add(new MasterDataItemImage());
    HashSet<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(new ItemViewConfig(ModelConverterImplTest.BUYABLE, ModelConverterImplTest.DISCOVERABLE,
        ModelConverterImplTest.CHANNEL, null, null));
    item.setItemViewConfigs(itemViewConfigs);
    Set<Price> prices = new HashSet<>();
    prices.add(new Price());
    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setSkuCode(ModelConverterImplTest.SKU_CODE);
    masterDataItem.setGeneratedItemName(ModelConverterImplTest.GENERATED_ITEM_NAME);
    masterDataItem.setUpcCode(ModelConverterImplTest.UPC_CODE);
    masterDataItem.setActivated(ModelConverterImplTest.IS_ACTIVATED);
    masterDataItem.setViewable(ModelConverterImplTest.IS_VIEWABLE);
    masterDataItem.setHash(ModelConverterImplTest.HASH);
    masterDataItem.setInventoryType(ModelConverterImplTest.INVENTORY_TYPE);
    masterDataItem.setDangerousLevel(ModelConverterImplTest.DANGEROUS_LEVEL);
    masterDataItem.setItemDeliveryWeight(ModelConverterImplTest.ITEM_DELIVERY_WEIGHT);
    masterDataItem.setItemHeight(ModelConverterImplTest.ITEM_HEIGHT);
    masterDataItem.setItemLength(ModelConverterImplTest.ITEM_LENGTH);
    masterDataItem.setItemWeight(ModelConverterImplTest.ITEM_WEIGHT);
    masterDataItem.setItemWidth(ModelConverterImplTest.ITEM_WIDTH);
    masterDataItem.setMasterDataItemImages(masterDataItemImages);
    masterDataItem.setProductCode(ModelConverterImplTest.PRODUCT_CODE);
    item.setMasterDataItem(masterDataItem);
    item.setLateFulfillment(ModelConverterImplTest.IS_LATE_FULFILLMENT);
    item.setPickupPointCode(ModelConverterImplTest.PICKUP_POINT_CODE);
    item.setTicketTemplateCode(ModelConverterImplTest.TICKET_TEMPLATE_CODE);
    item.setPrice(prices);
    return item;
  }

  private Product setupProduct() {
    List<ProductAttribute> definingAttributes = new ArrayList<>();
    List<ProductAttributeDetail> descriptiveAttributes = new ArrayList<>();
    List<ProductSpecialAttribute> productSpecialAttributes = new ArrayList<>();
    List<SalesCatalog> salesCatalogs = new ArrayList<>();
    List<MasterDataProductImage> masterDataProductImages = new ArrayList<>();
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    List<SalesCategorySequence> salesCategorySequences = new ArrayList<>();
    List<ItemCatalogVO> itemCatalogs = new ArrayList<>();
    SalesCategorySequence salesCategorySequence = new SalesCategorySequence(CATEGORY_CODE, 2);
    salesCategorySequences.add(salesCategorySequence);
    salesCatalogs.add(new SalesCatalog());

    Product product = new Product();
    product.setProductSku(ModelConverterImplTest.PRODUCT_SKU);
    product.setProductCode(ModelConverterImplTest.PRODUCT_CODE);
    product.setProductType(ProductType.BIG_PRODUCT);
    product.setSettlementType(ModelConverterImplTest.SETTLEMENT_TYPE);
    product.setMerchantCode(ModelConverterImplTest.MERCHANT_CODE);
    product.setSynchronized(ModelConverterImplTest.IS_SYNCHRONIZED);
    product.setProductCatentryId(ModelConverterImplTest.PRODUCT_CATENTRY_ID);
    product.setDefiningAttributes(definingAttributes);
    product.setDescriptiveAttributes(descriptiveAttributes);
    product.setProductSpecialAttributes(productSpecialAttributes);
    product.setProductScore(new ProductScore(10, 10, 10, 10, 10, 10, 10, 10, 10, 1, 90));
    product.setCurationStatus(CurationStatus.APPROVED);
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(ModelConverterImplTest.CATALOG_CODE);
    Category category = new Category();
    category.setCategoryCode(ModelConverterImplTest.CATEGORY_CODE);
    category.setCatgroupId(ModelConverterImplTest.CATGROUP_ID);
    masterCatalog.setCategory(category);
    product.setMasterCatalog(masterCatalog);
    product.setSalesCatalogs(salesCatalogs);

    MasterDataProduct masterDataProduct = new MasterDataProduct();
    masterDataProduct.setBrand(ModelConverterImplTest.BRAND);
    masterDataProduct.setShippingWeight(ModelConverterImplTest.SHIPPING_WEIGHT);
    masterDataProduct.setSpecificationDetail(ModelConverterImplTest.SPECIFICATION_DETAIL);
    masterDataProduct.setProductName(ModelConverterImplTest.PRODUCT_NAME);
    masterDataProduct.setDescription(ModelConverterImplTest.DESCRIPTION);
    masterDataProduct.setLongDescription(ModelConverterImplTest.LONG_DESCRIPTION);
    masterDataProduct.setUniqueSellingPoint(ModelConverterImplTest.UNIQUE_SELLING_POINT);
    masterDataProduct.setActivated(ModelConverterImplTest.IS_ACTIVATED);
    masterDataProduct.setViewable(ModelConverterImplTest.IS_VIEWABLE);
    masterDataProduct.setProductStory(ModelConverterImplTest.PRODUCT_STORY);
    masterDataProduct.setUom(ModelConverterImplTest.UOM);
    masterDataProduct.setMasterDataProductImages(masterDataProductImages);
    masterDataProduct.setMasterDataProductAttributes(masterDataProductAttributes);
    masterDataProduct.setUrl(ModelConverterImplTest.URL);
    masterDataProduct.setLength(ModelConverterImplTest.LENGTH);
    masterDataProduct.setWidth(ModelConverterImplTest.WIDTH);
    masterDataProduct.setHeight(ModelConverterImplTest.HEIGHT);
    masterDataProduct.setWeight(ModelConverterImplTest.WEIGHT);
    masterCatalog.setCatalogCode(ModelConverterImplTest.CATALOG_CODE);
    product.setMasterDataProduct(masterDataProduct);
    product.setSalesCategorySequences(salesCategorySequences);
    itemCatalogs.add(new ItemCatalogVO(CATALOG_CODE, new ArrayList<>()));
    product.setItemCatalogs(itemCatalogs);
    product.setInstallationRequired(ModelConverterImplTest.INSTALLATION_REQUIRED);
    return product;
  }

  @Test
  public void convertToSimplePristineProductResponse() throws Exception {
    List<SimplePristineProductRequestVo> simplePristineProductRequestVoList = new ArrayList<>();
    SimplePristineProductRequestVo simplePristineProductRequestVo = new SimplePristineProductRequestVo();
    Set<SimpleProductRequestVo> simpleProductRequestVoSet = new HashSet<>();
    simplePristineProductRequestVo.setPristineId(PRISTINE_ID);
    SimpleProductRequestVo simpleProductRequestVo = new SimpleProductRequestVo();
    simpleProductRequestVo.setProductCode(PRODUCT_CODE);
    simpleProductRequestVo.setProductSku(PRODUCT_SKU);
    simpleProductRequestVo.setSynchronized(true);
    simpleProductRequestVoSet.add(simpleProductRequestVo);
    simplePristineProductRequestVo.setSimpleProductDTOList(simpleProductRequestVoSet);
    simplePristineProductRequestVoList.add(simplePristineProductRequestVo);
    List<SimplePristineProductResponse> response =
        this.modelConverterImpl.convertToSimplePristineProductResponse(simplePristineProductRequestVoList);
    assertNotNull(response);
    assertEquals(response.size(), 1);
    assertEquals(PRISTINE_ID, response.get(0).getPristineId());
  }

  @Test
  public void convertToSimpleProductMasterDataDetailResponseTest() throws Exception {

    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();
    Map<String, MasterDataProduct> productMasterDataProductMap = new HashMap<>();

    List<Item> itemList = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    Product product = setupProduct();
    productMasterDataProductMap.put(product.getProductCode(), product.getMasterDataProduct());
    Item item = setupItem();
    itemList.add(item);
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(itemList);
    productAndItemsVOList.add(productAndItemsVO);
    masterDataDetailWithProductAndItemsResponseVo.setProductAndItems(productAndItemsVOList);
    masterDataDetailWithProductAndItemsResponseVo.setMasterDataProducts(productMasterDataProductMap);
    List<SimpleProductMasterDataDetailResponse> response = this.modelConverterImpl
        .convertToSimpleProductMasterDataDetailResponse(masterDataDetailWithProductAndItemsResponseVo);
    assertNotNull(response);
    assertEquals(1, response.size());
    assertEquals(ModelConverterImplTest.PRODUCT_SKU, response.get(0).getProductSku());

  }

  @Test
  public void convertToSimpleProductMasterDataDetailResponseTest_withItemCatalogs() throws Exception {

    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailWithProductAndItemsResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();
    Map<String, MasterDataProduct> productMasterDataProductMap = new HashMap<>();

    List<Item> itemList = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    Product product = setupProduct();
    productMasterDataProductMap.put(product.getProductCode(), product.getMasterDataProduct());
    Item item = setupItem();
    itemList.add(item);
    productAndItemsVO.setProduct(product);
    productAndItemsVO.setItems(itemList);
    productAndItemsVOList.add(productAndItemsVO);
    masterDataDetailWithProductAndItemsResponseVo.setProductAndItems(productAndItemsVOList);
    masterDataDetailWithProductAndItemsResponseVo.setMasterDataProducts(productMasterDataProductMap);
    List<SimpleProductMasterDataDetailResponse> response = this.modelConverterImpl
        .convertToSimpleProductMasterDataDetailResponse(masterDataDetailWithProductAndItemsResponseVo);
    assertNotNull(response);
    assertEquals(1, response.size());
    assertEquals(ModelConverterImplTest.PRODUCT_SKU, response.get(0).getProductSku());

  }

  @Test
  public void convertToPristineDataItemTest() throws ReflectiveOperationException {

    PristineDataItemDto pristineDataItemDto = new PristineDataItemDto();
    pristineDataItemDto.setPristineId(PRISTINE_ID);
    PristineDataItem pristineDataItem = this.modelConverterImpl.convertToPristineDataItem(pristineDataItemDto);
    assertEquals(PRISTINE_ID, pristineDataItem.getPristineId());
  }

  @Test
  public void convertToPristineDataItemExceptionTest() throws ReflectiveOperationException {

    Assertions.assertThrows(IllegalArgumentException.class, () -> this.modelConverterImpl.convertToPristineDataItem(null));

  }

  @Test
  public void convertToPristineMasterDataDetailResponseTest() throws Exception {
    Product product = this.setupProduct();
    Item item = this.setupItem();
    List<Item> items = new ArrayList<>();
    items.add(item);
    PristineMasterDataDetailWithProductAndItemsResponseVo pristineMasterDataDetailResponseVo =
        new PristineMasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVOs.add(productAndItemsVO);
    pristineMasterDataDetailResponseVo.setProductAndItems(productAndItemsVOs);
    pristineMasterDataDetailResponseVo.setPristineMasterProductAndItems(productAndItemsVOs);
    PristineMasterDataDetailWithProductAndItemsResponse response =
        this.modelConverterImpl.convertToPristineMasterDataDetailResponse(pristineMasterDataDetailResponseVo);
    verify(this.gdnMapperHelper)
        .mapBean(pristineMasterDataDetailResponseVo, PristineMasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void convertToPristineMasterDataDetailResponseDocumentTest() {
    PristineMasterDataDetailResponse pristineMasterDataDetailResponse = new PristineMasterDataDetailResponse();
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(pristineMasterDataDetailResponse);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class))
        .thenReturn(pristineMasterDataDetailResponse);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    PristineMasterDataDetailResponse result =
        this.modelConverterImpl.convertToPristineMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertEquals(DOCUMENT_TYPE_1, result.getProductAndItems().get(0).getProduct().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.getProductAndItems().get(0).getProduct().getDocumentType().get(1));
  }

  @Test
  public void convertToPristineMasterDataDetailResponseDocumentTest1() {
    PristineMasterDataDetailResponse pristineMasterDataDetailResponse = new PristineMasterDataDetailResponse();
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(pristineMasterDataDetailResponse);
    categoryResponse.setDocumentType(StringUtils.EMPTY);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class))
        .thenReturn(pristineMasterDataDetailResponse);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(null);
    PristineMasterDataDetailResponse result =
        this.modelConverterImpl.convertToPristineMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(CollectionUtils.isEmpty(result.getProductAndItems().get(0).getProduct().getDocumentType()));
  }

  @Test
  public void convertToPristineMasterDataDetailResponseDocumentExceptionTest() {
    PristineMasterDataDetailResponse pristineMasterDataDetailResponse = new PristineMasterDataDetailResponse();
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        getMasterDataDetailWithProductAndItemsResponseVo(pristineMasterDataDetailResponse);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class))
        .thenReturn(pristineMasterDataDetailResponse);
    Mockito.doThrow(RuntimeException.class).when(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    PristineMasterDataDetailResponse result =
        this.modelConverterImpl.convertToPristineMasterDataDetailResponse(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, PristineMasterDataDetailResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    assertTrue(CollectionUtils.isEmpty(result.getProductAndItems().get(0).getProduct().getDocumentType()));
  }

  private MasterDataDetailWithProductAndItemsResponseVo getMasterDataDetailWithProductAndItemsResponseVo(
      PristineMasterDataDetailResponse pristineMasterDataDetailResponse) {
    Product product = this.setupProduct();
    product.setSynchronized(Boolean.TRUE);
    Item item = this.setupItem();
    List<Item> items = new ArrayList<>();
    items.add(item);
    MasterDataDetailWithProductAndItemsResponseVo masterDataDetailResponseVo =
        new MasterDataDetailWithProductAndItemsResponseVo();
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVOs.add(productAndItemsVO);
    masterDataDetailResponseVo.setProductAndItems(productAndItemsVOs);
    PristineProductAndItemsDTO pristineProductAndItemsDTO = new PristineProductAndItemsDTO();
    PristineItemResponse pristineItemResponse = new PristineItemResponse();
    PristineProductResponse pristineProductResponse = new PristineProductResponse();
    pristineProductResponse.setProductCode(PRODUCT_CODE);
    MasterCatalogDTO masterCatalog = new MasterCatalogDTO();
    CategoryDTO category = new CategoryDTO();
    category.setCategoryCode(SYSTEM_PARAMETER_VALUE);
    masterCatalog.setCategory(category);
    pristineProductResponse.setMasterCatalog(masterCatalog);
    pristineProductAndItemsDTO.setProduct(pristineProductResponse);
    pristineProductAndItemsDTO.setItems(Collections.singletonList(pristineItemResponse));
    pristineMasterDataDetailResponse.setProductAndItems(Collections.singletonList(pristineProductAndItemsDTO));
    return masterDataDetailResponseVo;
  }

  @Test
  public void convertToSimpleProductResponseTest_product_getProductSpecialAttributes_containsGuaranteeAND_LamaGaransi()
      throws Exception {

    ProductSpecialAttribute productSpecialAttribute1 = new ProductSpecialAttribute();
    productSpecialAttribute1.setAttributeName(GARANSI);
    productSpecialAttribute1.setAttributeValue(GUARANTEE_TYPE);
    ProductSpecialAttribute productSpecialAttribute2 = new ProductSpecialAttribute();
    productSpecialAttribute2.setAttributeName(LAMA_GARANSI);
    productSpecialAttribute2.setAttributeValue(GUARANTEE_DURATION);

    List<ProductSpecialAttribute> productSpecialAttributes = new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes.add(productSpecialAttribute1);
    productSpecialAttributes.add(productSpecialAttribute2);

    productSimple.setProductSpecialAttributes(productSpecialAttributes);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(GUARANTEE_TYPE + " " + GUARANTEE_DURATION, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToSimpleProductResponseItemViewConfigPriceNullTest()
      throws Exception {

    ProductSpecialAttribute productSpecialAttribute1 = new ProductSpecialAttribute();
    productSpecialAttribute1.setAttributeName(GARANSI);
    productSpecialAttribute1.setAttributeValue(GUARANTEE_TYPE);
    ProductSpecialAttribute productSpecialAttribute2 = new ProductSpecialAttribute();
    productSpecialAttribute2.setAttributeName(LAMA_GARANSI);
    productSpecialAttribute2.setAttributeValue(GUARANTEE_DURATION);

    List<ProductSpecialAttribute> productSpecialAttributes = new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes.add(productSpecialAttribute1);
    productSpecialAttributes.add(productSpecialAttribute2);

    productSimple.setProductSpecialAttributes(productSpecialAttributes);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());
    this.itemSimple.setItemViewConfigs(null);
    this.itemSimple.setPrice(null);
    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(GUARANTEE_TYPE + " " + GUARANTEE_DURATION, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToItemSummaryResponseTest() {
    ItemInfoVO itemInfoVO = new ItemInfoVO();
    itemInfoVO.setProductCode(PRODUCT_CODE);
    itemInfoVO.setProductName(PRODUCT_NAME);
    itemInfoVO.setProductSku(PRODUCT_SKU);
    itemInfoVO.setItemCode(ITEM_CODE);
    itemInfoVO.setItemName(ITEM_NAME);
    itemInfoVO.setItemSku(ITEM_SKU);
    itemInfoVO.setMerchantCode(MERCHANT_CODE);
    itemInfoVO.setImageUrl(IMAGE_URL);

    List<ItemSummaryResponse> itemSummaryResponseList =
        this.modelConverterImpl.convertToItemSummaryResponse(Arrays.asList(itemInfoVO));

    assertEquals(PRODUCT_CODE, itemSummaryResponseList.get(0).getProductCode());
    assertEquals(PRODUCT_NAME, itemSummaryResponseList.get(0).getProductName());
    assertEquals(PRODUCT_SKU, itemSummaryResponseList.get(0).getProductSku());
    assertEquals(ITEM_CODE, itemSummaryResponseList.get(0).getItemCode());
    assertEquals(ITEM_NAME, itemSummaryResponseList.get(0).getGeneratedItemName());
    assertEquals(ITEM_SKU, itemSummaryResponseList.get(0).getItemSku());
    assertEquals(MERCHANT_CODE, itemSummaryResponseList.get(0).getMerchantCode());
    assertEquals(IMAGE_URL, itemSummaryResponseList.get(0).getMasterDataItemImages().get(0).getLocationPath());
  }

  @Test
  public void
  convertToSimpleProductResponseTest_product_getProductSpecialAttributes_containsGuaranteeAND_notContainsLamaGaransi()
      throws Exception {

    ProductSpecialAttribute productSpecialAttribute1 = new ProductSpecialAttribute();
    productSpecialAttribute1.setAttributeName(GARANSI);
    productSpecialAttribute1.setAttributeValue(GUARANTEE_TYPE);

    List<ProductSpecialAttribute> productSpecialAttributes = new ArrayList<ProductSpecialAttribute>();
    productSpecialAttributes.add(productSpecialAttribute1);

    productSimple.setProductSpecialAttributes(productSpecialAttributes);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(GUARANTEE_TYPE, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void
  convertToSimpleProductResponseTest_product_getProductSpecialAttributes_containsLamaGaransiAND_notContainsGuarantee()
      throws Exception {

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToSimpleProductResponseTest_product_getProductSpecialAttributes_isNull() throws Exception {
    this.productSimple.setProductSpecialAttributes(null);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToSimpleProductResponseTest_product_getProductSpecialAttributes_isEmpty() throws Exception {
    this.productSimple.setProductSpecialAttributes(new ArrayList<>());
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToSimpleProductResponseTest_product_getProductSpecialAttributes_containsNull() throws Exception {
    this.productSimple.setProductSpecialAttributes(new ArrayList<>());
    this.productSimple.getProductSpecialAttributes().add(null);

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToSimpleProductResponseTest_product_getProductSpecialAttributes_getAttributeName_isNull()
      throws Exception {
    //productSimple.setProductSpecialAttributes();1.setAttributeName(null);

    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.itemSimple.getMasterDataItem(), this.productSimple.getMasterDataProduct()))
        .thenReturn(this.itemSimple.getMasterDataItem());

    SimpleProductResponse result =
        this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, this.itemSimple);
    assertEquals(null, result.getItemDetail().getWarrantyInfo());
    verify(this.productHelperService).getSettlementType(this.productSimple, this.itemSimple);
  }

  @Test
  public void convertToSimpleProductResponseTestWithNullItem() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToSimpleProductResponse(this.productSimple, null));
  }

  @Test
  public void convertToSimpleProductResponseTestWithNullProduct() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToSimpleProductResponse(null, this.itemSimple));
  }

  @Test
  public void convertToOfflineItemList() throws ReflectiveOperationException {
    OfflineItemRequest request = new OfflineItemRequest();
    this.offlineItemRequestList = new ArrayList<>();
    offlineItemRequestList.add(request);

    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setMerchantCode(MERCHANT_CODE);

    when(this.gdnMapperHelper.mapBean(request, OfflineItem.class)).thenReturn(offlineItem);

    List<UpsertOfflineItemRequest> result =
        this.modelConverterImpl.convertToOfflineItemList(offlineItemRequestList, MERCHANT_CODE, STORE_ID);
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void convertToOfflineItems() throws ReflectiveOperationException {
    OfflineItem offlineItem = new OfflineItem();
    offlineItem.setMerchantCode(MERCHANT_CODE);
    offlineItem.setOfflineItemId(OFFLINE_ITEM_ID);
    offlineItem.setItemSku(ITEM_SKU);
    offlineItem.setOfferPrice(OFFER_PRICE);
    offlineItem.setPickupPointCode(PICKUP_POINT_CODE);

    when(this.gdnMapperHelper.mapBean(this.upsertOfflineItemRequest, OfflineItem.class)).thenReturn(offlineItem);

    List<OfflineItem> result =
        this.modelConverterImpl.convertToOfflineItems(this.upsertOfflineItemRequests, MERCHANT_CODE, STORE_ID);
    assertNotNull(result);
    assertEquals(1, result.size());

    OfflineItem actual = result.get(0);
    assertEquals(offlineItem, actual);
  }

  @Test
  public void convertProductAttributeRequestToMasterDataProductAttributeTest() throws Exception {
    AttributeRequest attributeRequest = new AttributeRequest();
    attributeRequest.setAttributeCode(ModelConverterImplTest.ATTRIBUTE_CODE);
    attributeRequest.setAttributeType(AttributeType.DEFINING_ATTRIBUTE.toString());
    attributeRequest.setDescription(ModelConverterImplTest.DESCRIPTION.getBytes());

    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setAllowedAttributeCode(ModelConverterImplTest.ALLOWED_ATTRIBUTE_CODE);
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest
        .setPredefinedAllowedAttributeCode(ModelConverterImplTest.PREDEFINED_ALLOWED_ATTRIBUTE_CODE);

    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    productAttributeValueRequest.setAllowedAttributeValue(allowedAttributeValueRequest);
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueRequest);
    productAttributeValueRequest.setDescriptiveAttributeValue(ModelConverterImplTest.DESCRIPTIVE_ATTRIBUTE_VALUE);
    productAttributeValueRequest
        .setDescriptiveAttributeValueType(ModelConverterImplTest.DESCRIPTIVE_ATTRIBUTE_VALUE_TYPE);

    List<ProductAttributeValueRequest> productAttributeValueRequests = new ArrayList<ProductAttributeValueRequest>();
    productAttributeValueRequests.add(productAttributeValueRequest);

    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    productAttributeRequest.setAttribute(attributeRequest);
    productAttributeRequest.setOwnByProductItem(ModelConverterImplTest.BOOLEAN_FALSE);
    productAttributeRequest.setSequence(ModelConverterImplTest.SEQUENCE);
    productAttributeRequest.setProductAttributeValues(productAttributeValueRequests);

    this.modelConverterImpl.convertProductAttributeRequestToMasterDataProductAttribute(productAttributeRequest);
  }

  @Test
  public void convertItemToOfflineProductResponseTest_merchantSKUNotFound_success() {
    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setErrorCode(ProductErrorCodesEnum.MERCHANT_SKU_NOT_FOUND.getCode());
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU_NOT_FOUND);
    List<OfflineItemResponseDetail> expectedResult = new ArrayList<>();
    expectedResult.add(offlineItemResponseDetail);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU_NOT_FOUND);

    itemDetail.setMerchantSku(MERCHANT_SKU);
    itemDetail.setItemSku(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    items.add(itemDetail);

    List<OfflineItemResponseDetail> result =
        modelConverterImpl.convertItemToOfflineProductResponse(merchantSkus, items, productTypeByProductSkuMap);

    for (int i = 0; i < expectedResult.size(); i++) {
      assertEquals(expectedResult.get(i).getMerchantSku(), result.get(i).getMerchantSku());
      assertEquals(expectedResult.get(i).getItemSku(), result.get(i).getItemSku());
      assertEquals(expectedResult.get(i).getErrorCode(), result.get(i).getErrorCode());
    }
  }

  @Test
  public void convertItemToOfflineProductResponse_duplicateMerchantSKU_success() {
    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setErrorCode(ProductErrorCodesEnum.DUPLICATE_MERCHANT_SKU.getCode());
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU_DUPLICATE);
    List<OfflineItemResponseDetail> expectedResult = new ArrayList<>();
    expectedResult.add(offlineItemResponseDetail);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU_DUPLICATE);

    itemDetail.setMerchantSku(MERCHANT_SKU_DUPLICATE);
    itemDetail.setItemSku(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    items.add(itemDetail);
    items.add(itemDetail);

    List<OfflineItemResponseDetail> result =
        modelConverterImpl.convertItemToOfflineProductResponse(merchantSkus, items, productTypeByProductSkuMap);

    for (int i = 0; i < expectedResult.size(); i++) {
      assertEquals(expectedResult.get(i).getMerchantSku(), result.get(i).getMerchantSku());
      assertEquals(expectedResult.get(i).getItemSku(), result.get(i).getItemSku());
      assertEquals(expectedResult.get(i).getErrorCode(), result.get(i).getErrorCode());
    }
  }

  @Test
  public void convertItemToOfflineProductResponse_merchantSku_success() {
    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(ITEM_SKU);
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);
    List<OfflineItemResponseDetail> expectedResult = new ArrayList<>();
    expectedResult.add(offlineItemResponseDetail);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    itemDetail.setMerchantSku(MERCHANT_SKU);
    itemDetail.setItemSku(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    items.add(itemDetail);

    List<OfflineItemResponseDetail> result =
        modelConverterImpl.convertItemToOfflineProductResponse(merchantSkus, items, productTypeByProductSkuMap);

    for (int i = 0; i < expectedResult.size(); i++) {
      assertEquals(expectedResult.get(i).getMerchantSku(), result.get(i).getMerchantSku());
      assertEquals(expectedResult.get(i).getItemSku(), result.get(i).getItemSku());
      assertEquals(expectedResult.get(i).getErrorCode(), result.get(i).getErrorCode());
    }
  }

  @Test
  public void convertItemToOfflineProductResponse_exceptionBopis_success() {
    OfflineItemResponseDetail offlineItemResponseDetail = new OfflineItemResponseDetail();
    offlineItemResponseDetail.setItemSku(null);
    offlineItemResponseDetail.setMerchantSku(MERCHANT_SKU);
    offlineItemResponseDetail.setErrorCode(ProductErrorCodesEnum.BOPIS_PRODUCT_CANNOT_BE_CONVERTED_TO_CNC.getCode());
    List<OfflineItemResponseDetail> expectedResult = new ArrayList<>();
    expectedResult.add(offlineItemResponseDetail);

    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    itemDetail.setProductSku(PRODUCT_SKU);
    itemDetail.setMerchantSku(MERCHANT_SKU);
    itemDetail.setItemSku(ITEM_SKU);
    List<Item> items = new ArrayList<>();
    items.add(itemDetail);

    productTypeByProductSkuMap.put(PRODUCT_SKU, ProductType.BOPIS);

    List<OfflineItemResponseDetail> result =
        modelConverterImpl.convertItemToOfflineProductResponse(merchantSkus, items, productTypeByProductSkuMap);

    for (int i = 0; i < expectedResult.size(); i++) {
      assertEquals(expectedResult.get(i).getMerchantSku(), result.get(i).getMerchantSku());
      assertEquals(expectedResult.get(i).getItemSku(), result.get(i).getItemSku());
      assertEquals(expectedResult.get(i).getErrorCode(), result.get(i).getErrorCode());
    }
  }

  @Test
  public void convertToProductAndItemsDTOs_onlyOneData_success() {
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    productAndItemsVOs.add(this.productanditemsvo);
    List<ProductAndItemsResponse> result = this.modelConverterImpl.convertToProductAndItemsDTOs(productAndItemsVOs);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.get(0).getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.get(0).getItems().size(), (2));
  }

  @Test
  public void convertToProductAndItemsDTOs_emptyData_success() {
    List<ProductAndItemsResponse> result = this.modelConverterImpl.convertToProductAndItemsDTOs(new ArrayList<>());

    assertNotNull(result);
    assertEquals(result.size(), 0);
  }

  @Test
  public void testConvertComboDetailVoToComboDetailResponseWithNullVo() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertComboDetailVoToComboDetailResponse(null));
  }

  @Test
  public void convertComboResponseVOToComboResponseTest() {
    ComboResponseVO comboResponseVO = new ComboResponseVO();
    ComboResponse response = modelConverterImpl.convertComboResponseVOToComboResponse(comboResponseVO);
    verify(gdnMapperHelper).mapBean(comboResponseVO, ComboResponse.class);
  }

  @Test
  public void convertComboResponseVOToComboResponseTest_ThrowException() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> modelConverterImpl.convertComboResponseVOToComboResponse(null));
  }

  @Test
  public void convertWholesaleVOToWholesaleResponseTest() {
    WholesaleVO wholesaleVO = new WholesaleVO();
    WholesaleResponse response = modelConverterImpl.convertWholesaleVOToWholesaleResponse(wholesaleVO);
    verify(gdnMapperHelper).mapBean(wholesaleVO, WholesaleResponse.class);
  }

  @Test
  public void convertWholesaleVOToWholesaleResponseTest_ThrowException() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> modelConverterImpl.convertWholesaleVOToWholesaleResponse(null));
  }

  @Test
  public void convertActiveComboRequestToActiveComboRequestVOTest() {
    ActiveComboRequest activeComboRequest = new ActiveComboRequest();
    ActiveComboRequestVO response =
        modelConverterImpl.convertActiveComboRequestToActiveComboRequestVO(activeComboRequest);
    verify(gdnMapperHelper).mapBean(activeComboRequest, ActiveComboRequestVO.class);
  }

  @Test
  public void convertActiveComboRequestToActiveComboRequestVOTest_ThrowException() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  modelConverterImpl.convertActiveComboRequestToActiveComboRequestVO(null));
  }

  @Test
  public void testConvertComboDetailVoToComboDetailResponseSuccess() {
    when(gdnMapperHelper.mapBean(comboDetailVo, ComboDetailResponse.class)).thenReturn(comboDetailResponse);
    ComboDetailResponse result = this.modelConverterImpl.convertComboDetailVoToComboDetailResponse(comboDetailVo);
    assertEquals(TOTAL_OTHER_COMBO, result.getTotal());
    verify(gdnMapperHelper, times(1)).mapBean(comboDetailVo, ComboDetailResponse.class);
  }

  @Test
  public void testConvertToProductDetailResponse() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    ProductDetailVo productDetailVo = new ProductDetailVo();
    productDetailVo.setProductCode(PRODUCT_CODE);
    productDetailVo.setMinPrice(MIN_PRICE);
    productDetailVo.setItemSkus(itemSkus);

    List<ProductDetailResponse> result =
        modelConverterImpl.convertToProductDetailResponse(Collections.singletonList(productDetailVo));
    assertEquals(PRODUCT_CODE, result.get(0).getProductCode());
    assertEquals(MIN_PRICE, result.get(0).getMinPrice(), 0);
    assertEquals(result.get(0).getItemSkus(), itemSkus);
  }

  @Test
  public void testconvertToActiveProductDetailVoList() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setProductCode(PRODUCT_CODE);
    activeProductDetailVo.setProductSku(PRODUCT_SKU);
    activeProductDetailVo.setStatus(STATUS);
    ActiveProductDetailVo activeProductDetailVo2 = new ActiveProductDetailVo();
    activeProductDetailVo2.setProductCode(PRODUCT_CODE);
    activeProductDetailVo2.setProductSku(PRODUCT_SKU);
    activeProductDetailVo2.setStatus(STATUS);
    ItemNameSkuVO itemNameSkuVO = new ItemNameSkuVO();
    itemNameSkuVO.setItemName(ITEM_NAME);
    itemNameSkuVO.setItemSku(ITEM_SKU);
    List<ItemNameSkuVO> itemNameSkuVOList = new ArrayList<>();
    itemNameSkuVOList.add(itemNameSkuVO);
    activeProductDetailVo.setItemDetailVOList(itemNameSkuVOList);
    List<ActiveProductResponse> result =
        modelConverterImpl.convertToActiveProductResponse(Arrays.asList(activeProductDetailVo, activeProductDetailVo2));
    assertEquals(PRODUCT_CODE, result.get(0).getProductCode());
    assertEquals(PRODUCT_SKU, result.get(0).getProductSku());
    assertEquals(STATUS, result.get(0).getStatus());
    assertEquals(1, result.get(0).getItemDetailResponses().size());
  }

  @Test
  public void testconvertToActiveProductDetailVoList_withoutItem() {
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setProductCode(PRODUCT_CODE);
    activeProductDetailVo.setProductSku(PRODUCT_SKU);
    activeProductDetailVo.setStatus(STATUS);
    List<ActiveProductResponse> result =
        modelConverterImpl.convertToActiveProductResponse(Collections.singletonList(activeProductDetailVo));
    assertEquals(PRODUCT_CODE, result.get(0).getProductCode());
    assertEquals(PRODUCT_SKU, result.get(0).getProductSku());
    assertEquals(STATUS, result.get(0).getStatus());
  }


  @Test
  public void testconvertToActiveProductDetailVoList_withItem() {
    ActiveProductDetailVo activeProductDetailVo = new ActiveProductDetailVo();
    activeProductDetailVo.setProductCode(PRODUCT_CODE);
    activeProductDetailVo.setProductSku(PRODUCT_SKU);
    activeProductDetailVo.setStatus(STATUS);
    activeProductDetailVo.setItemAndPickupPointBasicDetailVo(new ItemAndPickupPointBasicDetailVo());
    List<ActiveProductResponse> result =
        modelConverterImpl.convertToActiveProductResponse(Collections.singletonList(activeProductDetailVo));
    assertEquals(PRODUCT_CODE, result.get(0).getProductCode());
    assertEquals(PRODUCT_SKU, result.get(0).getProductSku());
    assertEquals(STATUS, result.get(0).getStatus());
  }

  @Test
  public void testToOfficialStoreRequestVO() {
    ProductDetailRequest productDetailRequest = new ProductDetailRequest();
    productDetailRequest.setMerchantCodes(Collections.singletonList(MERCHANT_CODE));
    productDetailRequest.setBuyable(true);
    productDetailRequest.setArchived(true);
    productDetailRequest.setDiscoverable(true);
    OfficialStoreRequestVO result = modelConverterImpl.toOfficialStoreRequestVO(productDetailRequest);
    assertEquals(Collections.singletonList(MERCHANT_CODE), result.getMerchantCodes());
    assertTrue(result.getBuyable());
    assertTrue(result.getArchived());
    assertTrue(result.getDiscoverable());

  }

  @Test
  public void tesToActiveProductsRequestVO() {
    ActiveProductRequest activeProductRequest = new ActiveProductRequest();
    activeProductRequest.setMerchantCode(MERCHANT_CODE);
    activeProductRequest.setBuyable(true);
    activeProductRequest.setDiscoverable(true);
    ActiveProductsRequestVO result = modelConverterImpl.toActiveProductsRequestVO(activeProductRequest);
    assertEquals(MERCHANT_CODE, result.getMerchantCode());
    assertTrue(result.getBuyable());
    assertTrue(result.getDiscoverable());

  }

  @Test
  public void toSimpleProductAndItemsMasterDataDetailResponseTest() {
    SimpleMasterDataDetailWithProductAndItemsResponseVo simpleMasterDataDetailWithProductAndItemsResponse =
        new SimpleMasterDataDetailWithProductAndItemsResponseVo();
    modelConverterImpl
        .toSimpleProductAndItemsMasterDataDetailResponse(simpleMasterDataDetailWithProductAndItemsResponse);

  }

  @Test
  public void toSimpleProductAndItemsMasterDataDetailV2ResponseTest() {
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo
        simpleMasterDataDetailWithProductAndItemsResponse =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    modelConverterImpl
        .toSimpleProductAndItemsMasterDataDetailV2Response(simpleMasterDataDetailWithProductAndItemsResponse);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.listUtil);
    verifyNoMoreInteractions(this.itemService);
    verifyNoMoreInteractions(this.productHelperService);
    verifyNoMoreInteractions(this.objectMapper);
  }


  @Test
  public void convertProductToProductResponseTest() {
    Product product = getTransactionVO();
    Mockito.when(gdnMapperHelper.mapBean(product, ProductResponse.class)).thenReturn(getProductResponse(product));
    ProductResponse productResponse = this.modelConverterImpl.convertProductToProductResponse(product);
    Assertions.assertNotNull(productResponse);
    Assertions.assertEquals(product.getMasterCatalog().getCatalogCode(), productResponse.getMasterCatalog().getCatalogCode());
    Assertions.assertEquals(2, productResponse.getDescriptiveAttributes().size());
    Mockito.verify(gdnMapperHelper).mapBean(product, ProductResponse.class);
  }

  @Test
  public void convertToProductAndItemsDTOTest_disabledSyncCategory() {
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentType() {
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertEquals(DOCUMENT_TYPE_1, result.getProduct().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.getProduct().getDocumentType().get(1));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentExceptionType() {
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    Mockito.doThrow(RuntimeException.class).when(productHelperService).getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        SYSTEM_PARAMETER_VALUE);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemsDTOTest_emptySystemParameterValue() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenReturn(systemParameter);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentType1() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenReturn(systemParameter);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    Mockito.when(productHelperService.getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertEquals(DOCUMENT_TYPE_1, result.getProduct().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.getProduct().getDocumentType().get(1));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentType2() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenReturn(systemParameter);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    Mockito.when(productHelperService.getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
        SYSTEM_PARAMETER_VALUE)).thenReturn(null);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemsDTOTestExceptionDocumentType() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenReturn(systemParameter);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    Mockito.doThrow(RuntimeException.class).when(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemsDTOTest_systemParameterException() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenThrow(ApplicationRuntimeException.class);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentTest() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertEquals(DOCUMENT_TYPE_1, result.getProduct().getDocumentType().get(0));
    assertEquals(DOCUMENT_TYPE_2, result.getProduct().getDocumentType().get(1));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentTest2() {
    systemParameter.setValue(null);
    categoryResponse.setDocumentType(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(categoryResponse);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentTest1() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.when(productHelperService
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE)).thenReturn(null);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  @Test
  public void convertToProductAndItemsDTOTestDocumentExceptionTest() {
    systemParameter.setValue(null);
    Mockito.when(
        this.systemParameterService.findValueByStoreIdAndVariable(DEFAULT_STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenThrow(ApplicationRuntimeException.class);
    Mockito.doThrow(RuntimeException.class).when(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    productResponseForGetProductAndItems.setMasterCatalog(new MasterCatalogDTO());
    productResponseForGetProductAndItems.getMasterCatalog().setCategory(categoryDTO);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
            SYSTEM_PARAMETER_VALUE);
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(CollectionUtils.isEmpty(result.getProduct().getDocumentType()));
  }

  private ProductResponse getProductResponse(Product product) {
    ProductResponse productResponse = new ProductResponse();
    productResponse.setMasterCatalog(
        new MasterCatalogDTO(product.getMasterDataProduct().getMasterCatalog().getCatalogCode(),
            new CategoryDTO(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode(),
                product.getMasterDataProduct().getMasterCatalog().getCategory().getCatgroupId())));
    List<ProductAttributeDetailDTO> productAttributeDetailDTOS = new ArrayList<>();
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    productAttributeDetailDTO.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO.setAttributeName(ATTRIBUTE_NAME);
    productAttributeDetailDTO.setAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    ProductAttributeDetailDTO productAttributeDetailDTO2 = new ProductAttributeDetailDTO();
    productAttributeDetailDTO2.setAttributeValue(VALUE);
    productAttributeDetailDTO2.setAttributeCode(ATTRIBUTE_CODE);
    productAttributeDetailDTO2.setAttributeName(ATTRIBUTE_NAME);
    productAttributeDetailDTOS.add(productAttributeDetailDTO);
    productAttributeDetailDTOS.add(productAttributeDetailDTO2);
    productResponse.setDescriptiveAttributes(productAttributeDetailDTOS);
    return productResponse;
  }

  private Product getTransactionVO() {
    Product product = new Product();
    MasterDataProduct masterDataProduct = new MasterDataProduct();
    MasterDataProductAttribute masterDataProductAttribute1 = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(DESCRIPTIVE_ATTRIBUTE_VALUE);
    masterDataProductAttribute1.setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValue));
    masterDataProductAttribute1.setMasterDataAttribute(masterDataAttribute);
    MasterDataProductAttribute masterDataProductAttribute2 = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute2 = new MasterDataAttribute();
    masterDataAttribute2.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    masterDataAttribute2.setAttributeCode(ATTRIBUTE_CODE);
    masterDataAttribute2.setAttributeName(ATTRIBUTE_NAME);
    MasterDataProductAttributeValue masterDataProductAttributeValue2 = new MasterDataProductAttributeValue();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(VALUE);
    predefinedAllowedAttributeValue.setPredefinedAllowedAttributeCode(PREDEFINED_ALLOWED_ATTRIBUTE_CODE);
    masterDataProductAttributeValue2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    masterDataProductAttribute2.setMasterDataProductAttributeValues(Arrays.asList(masterDataProductAttributeValue2));
    masterDataProductAttribute2.setMasterDataAttribute(masterDataAttribute2);
    masterDataProduct
        .setMasterDataProductAttributes(Arrays.asList(masterDataProductAttribute1, masterDataProductAttribute2));
    MasterCatalog masterCatalog = new MasterCatalog();
    masterCatalog.setCatalogCode(CATALOG_CODE);
    masterCatalog.setCategory(new Category(CATEGORY_CODE, CATGROUP_ID));
    masterDataProduct.setMasterCatalog(masterCatalog);
    product.setMasterDataProduct(masterDataProduct);
    return product;
  }

  @Test
  public void convertItemToPromoTypesResponseCampaignCodeNull() {
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setMerchantPromoDiscount(true);
    itemSummaryResponseVO.setPromoBundling(true);
    Price price = new Price();
    price.setChannel(ChannelName.DEFAULT.getDescription());
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setDiscountPrice(100000);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    Price androidPrice = new Price();
    androidPrice.setChannel(ChannelName.ANDROID.getDescription());
    androidPrice.setListOfDiscountPrices(new ArrayList<>());
    itemSummaryResponseVO.setPrice(new HashSet<>(Arrays.asList(price, androidPrice)));
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = new ItemSummaryPageResponseVo(Arrays.asList(itemSummaryResponseVO), 0, 10);
    modelConverterImpl.setPromoTypesForListOfItems(itemSummaryPageResponseVo);
    Assertions.assertNotNull(itemSummaryPageResponseVo);
    Assertions.assertEquals(2, itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getPromoTypes().size());
  }

  @Test
  public void convertItemToPromoTypesOngoingCampaignResponse() {
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setMerchantPromoDiscount(true);
    itemSummaryResponseVO.setPromoBundling(true);
    Price price = new Price();
    price.setChannel(ChannelName.DEFAULT.getDescription());
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(PRODUCT_CODE);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice.setDiscountPrice(100000);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    Price androidPrice = new Price();
    androidPrice.setChannel(ChannelName.ANDROID.getDescription());
    androidPrice.setListOfDiscountPrices(new ArrayList<>());
    itemSummaryResponseVO.setPrice(new HashSet<>(Arrays.asList(price, androidPrice)));
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = new ItemSummaryPageResponseVo(Arrays.asList(itemSummaryResponseVO), 0, 10);
    modelConverterImpl.setPromoTypesForListOfItems(itemSummaryPageResponseVo);
    Assertions.assertNotNull(itemSummaryPageResponseVo);
    Assertions.assertEquals(3, itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getPromoTypes().size());
  }

  @Test
  public void convertItemToPromoTypesUpcomingCampaignResponse() {
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setMerchantPromoDiscount(true);
    itemSummaryResponseVO.setPromoBundling(true);
    Price price = new Price();
    price.setChannel(ChannelName.DEFAULT.getDescription());
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(PRODUCT_CODE);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));
    discountPrice.setDiscountPrice(100000);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    Price androidPrice = new Price();
    androidPrice.setChannel(ChannelName.ANDROID.getDescription());
    androidPrice.setListOfDiscountPrices(new ArrayList<>());
    itemSummaryResponseVO.setPrice(new HashSet<>(Arrays.asList(price, androidPrice)));
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = new ItemSummaryPageResponseVo(Arrays.asList(itemSummaryResponseVO), 0, 10);
    modelConverterImpl.setPromoTypesForListOfItems(itemSummaryPageResponseVo);
    Assertions.assertNotNull(itemSummaryPageResponseVo);
    Assertions.assertEquals(2, itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getPromoTypes().size());
  }

  @Test
  public void convertItemToPromoTypesExpiredCampaignResponse() {
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setMerchantPromoDiscount(true);
    itemSummaryResponseVO.setPromoBundling(true);
    Price price = new Price();
    price.setChannel(ChannelName.DEFAULT.getDescription());
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(PRODUCT_CODE);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(2))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setDiscountPrice(100000);
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    Price androidPrice = new Price();
    androidPrice.setChannel(ChannelName.ANDROID.getDescription());
    androidPrice.setListOfDiscountPrices(new ArrayList<>());
    itemSummaryResponseVO.setPrice(new HashSet<>(Arrays.asList(price, androidPrice)));
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = new ItemSummaryPageResponseVo(Arrays.asList(itemSummaryResponseVO), 0, 10);
    modelConverterImpl.setPromoTypesForListOfItems(itemSummaryPageResponseVo);
    Assertions.assertNotNull(itemSummaryPageResponseVo);
    Assertions.assertEquals(2, itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getPromoTypes().size());
  }


  @Test
  public void convertItemToPromoTypesResponseWithoutCampaign() {
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setMerchantPromoDiscount(true);
    itemSummaryResponseVO.setPromoBundling(true);
    Price price = new Price();
    price.setChannel(ChannelName.DEFAULT.getDescription());
    Price androidPrice = new Price();
    androidPrice.setChannel(ChannelName.ANDROID.getDescription());
    androidPrice.setListOfDiscountPrices(new ArrayList<>());
    itemSummaryResponseVO.setPrice(new HashSet<>(Arrays.asList(price, androidPrice)));
    ItemSummaryPageResponseVo itemSummaryPageResponseVo = new ItemSummaryPageResponseVo(Arrays.asList(itemSummaryResponseVO), 0, 10);
    modelConverterImpl.setPromoTypesForListOfItems(itemSummaryPageResponseVo);
    Assertions.assertNotNull(itemSummaryPageResponseVo);
    Assertions.assertEquals(2, itemSummaryPageResponseVo.getItemSummaryResponses().get(0).getPromoTypes().size());
  }

  @Test
  public void convertToProductAndItemsDTOTest_emptyPromoBundling() {
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertTrue(result.getProduct().isForceReview());
    assertEquals(result.getProduct().getDescriptiveAttributes().size(), (0));
  }

  @Test
  public void convertToItemSummaryDetailsPageResponseSuccess() throws Exception {
    ItemSummaryPageResponseVo voObject = new ItemSummaryPageResponseVo();
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setVersion(2l);
    itemSummaryResponseVO.setOriginalSellingPrice(10);
    itemSummaryResponseVO.setItemSku(ITEM_SKU);
    itemSummaryResponseVO.setPickupPointCode(PICKUP_POINT_CODE);
    voObject.setItemSummaryResponses(Collections.singletonList(itemSummaryResponseVO));
    voObject.getItemSummaryResponses().get(0).setProductScore(90);
    GdnRestListResponse<ItemSummaryDetailResponse> itemSummaryDetailPageResponse =
        this.modelConverterImpl.convertToItemSummaryDetailListResponse(ModelConverterImplTest.REQUEST_ID, 10, 1, voObject,
            Constants.ALL);
    Assertions.assertEquals(itemSummaryDetailPageResponse.getContent().get(0).getVersion(),
        voObject.getItemSummaryResponses().get(0).getVersion());
    Assertions.assertEquals(itemSummaryDetailPageResponse.getContent().get(0).getItemSku(),
        voObject.getItemSummaryResponses().get(0).getItemSku());
    Assertions.assertEquals(10,itemSummaryDetailPageResponse.getContent().get(0).getOriginalPrice(),0);
  }

  @Test
  public void convertToItemSummaryDetailPageResponseTest() {
    ItemSummaryPageResponseVo result = new ItemSummaryPageResponseVo();
    result.setTotalNum(10);
    result.setTotalPage(1000);
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setSuspended(true);
    itemSummaryResponseVO.setUpcCode(UPC_CODE);
    itemSummaryResponseVO.setItemSku(ITEM_SKU);
    itemSummaryResponseVO.setPrice(null);
    OfflineItemPriceVO offlineItemPriceVO = new OfflineItemPriceVO();
    offlineItemPriceVO.setOfferPrice(1000L);
    itemSummaryResponseVO.setSalesCatalogs(Arrays.asList(salesCatalog));
    itemSummaryResponseVO.setItemViewConfigs(new HashSet<>(Arrays.asList(viewConfig)));
    itemSummaryResponseVO.setMasterDataItemImages(masterItemImages);
    itemSummaryResponseVO.setOfflinePrices(Arrays.asList(offlineItemPriceVO));
    itemSummaryResponseVO.setPromoTypes(Arrays.asList("promo-type"));
    itemSummaryResponseVO.setActivePromoBundlings(new HashSet<>(Arrays.asList("CAMPAIGN")));
    masterCatalog1.setCatalogCode(CATEGORY_CODE);
    masterCatalog1.setCategory(new Category(CATEGORY_CODE, CATENTRY_ID));
    itemSummaryResponseVO.setMasterCatalog(masterCatalog1);
    itemSummaryResponseVO.setB2bFields(new com.gdn.x.product.model.entity.B2bFields());
    result.setItemSummaryResponses(Arrays.asList(itemSummaryResponseVO));
    ItemSummaryDetailPageResponse response = this.modelConverterImpl.convertToItemSummaryDetailPageResponse(result,
        null);
    Assertions.assertEquals(10, response.getTotalNum());
    Assertions.assertEquals(1000, response.getTotalPage());
    Assertions.assertEquals(UPC_CODE, response.getItemSummaryDetailResponses().get(0).getUpcCode());
    Assertions.assertEquals(ITEM_SKU, response.getItemSummaryDetailResponses().get(0).getItemSku());
    Assertions.assertEquals(1, response.getItemSummaryDetailResponses().get(0).getSalesCatalogs().size());
    Assertions.assertEquals(1, response.getItemSummaryDetailResponses().get(0).getItemViewConfigs().size());
    Assertions.assertEquals(1, response.getItemSummaryDetailResponses().get(0).getMasterDataItemImages().size());
    Assertions.assertEquals(1, response.getItemSummaryDetailResponses().get(0).getOfflinePrices().size());
    Assertions.assertEquals(1, response.getItemSummaryDetailResponses().get(0).getActivePromoBundlings().size());
    Assertions.assertEquals(1, response.getItemSummaryDetailResponses().get(0).getPromoTypes().size());
  }

  @Test
  public void convertToItemSummaryDetailPageResponseWithPriceTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("true"));
    ItemSummaryPageResponseVo result = new ItemSummaryPageResponseVo();
    result.setTotalNum(10);
    result.setTotalPage(1000);
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setSuspended(true);
    itemSummaryResponseVO.setUpcCode(UPC_CODE);
    itemSummaryResponseVO.setItemSku(ITEM_SKU);
    itemSummaryResponseVO.setPrice(Collections.singleton(price1));
    OfflineItemPriceVO offlineItemPriceVO = new OfflineItemPriceVO();
    offlineItemPriceVO.setOfferPrice(1000L);
    itemSummaryResponseVO.setSalesCatalogs(Arrays.asList(salesCatalog));
    ItemViewConfig itemViewConfig2 = new ItemViewConfig();
    itemViewConfig2.setChannel(Constants.CNC);
    itemSummaryResponseVO.setItemViewConfigs(new HashSet<>(Arrays.asList(viewConfig, itemViewConfig2)));
    itemSummaryResponseVO.setMasterDataItemImages(masterItemImages);
    itemSummaryResponseVO.setOfflinePrices(Arrays.asList(offlineItemPriceVO));
    itemSummaryResponseVO.setPromoTypes(Arrays.asList("promo-type"));
    itemSummaryResponseVO.setActivePromoBundlings(new HashSet<>(Arrays.asList("CAMPAIGN")));
    masterCatalog1.setCatalogCode(CATEGORY_CODE);
    masterCatalog1.setCategory(new Category(CATEGORY_CODE, CATENTRY_ID));
    itemSummaryResponseVO.setMasterCatalog(masterCatalog1);
    itemSummaryResponseVO.setItemViewConfigB2b(new HashSet<>(Arrays.asList(new ItemViewConfig())));
    result.setItemSummaryResponses(Arrays.asList(itemSummaryResponseVO));
    ItemSummaryDetailPageResponse response = this.modelConverterImpl.convertToItemSummaryDetailPageResponse(result,
        Constants.ALL);
    Assertions.assertEquals(price1.getOfferPrice(),
      response.getItemSummaryDetailResponses().get(0).getPrice().stream().findFirst().get().getOfferPrice(),0);
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch",
        Boolean.valueOf("false"));
  }

  @Test
  public void convertToItemSummaryDetailPageResponseWithEmptyPriceTest() {
    ItemSummaryPageResponseVo result = new ItemSummaryPageResponseVo();
    result.setTotalNum(10);
    result.setTotalPage(1000);
    ItemSummaryResponseVO itemSummaryResponseVO = new ItemSummaryResponseVO();
    itemSummaryResponseVO.setSuspended(true);
    itemSummaryResponseVO.setUpcCode(UPC_CODE);
    itemSummaryResponseVO.setItemSku(ITEM_SKU);
    itemSummaryResponseVO.setPrice(Collections.emptySet());
    OfflineItemPriceVO offlineItemPriceVO = new OfflineItemPriceVO();
    offlineItemPriceVO.setOfferPrice(1000L);
    itemSummaryResponseVO.setSalesCatalogs(Arrays.asList(salesCatalog));
    itemSummaryResponseVO.setItemViewConfigs(new HashSet<>(Arrays.asList(viewConfig)));
    itemSummaryResponseVO.setMasterDataItemImages(masterItemImages);
    itemSummaryResponseVO.setOfflinePrices(Arrays.asList(offlineItemPriceVO));
    itemSummaryResponseVO.setPromoTypes(Arrays.asList("promo-type"));
    itemSummaryResponseVO.setActivePromoBundlings(new HashSet<>(Arrays.asList("CAMPAIGN")));
    masterCatalog1.setCatalogCode(CATEGORY_CODE);
    masterCatalog1.setCategory(new Category(CATEGORY_CODE, CATENTRY_ID));
    itemSummaryResponseVO.setMasterCatalog(masterCatalog1);
    itemSummaryResponseVO.setDistribution(true);
    result.setItemSummaryResponses(Arrays.asList(itemSummaryResponseVO));
    ItemSummaryDetailPageResponse response = this.modelConverterImpl.convertToItemSummaryDetailPageResponse(result,
        Constants.ALL);
    Assertions.assertEquals(10, response.getTotalNum());
    Assertions.assertEquals(1000, response.getTotalPage());
    Assertions.assertEquals(UPC_CODE, response.getItemSummaryDetailResponses().get(0).getUpcCode());
    Assertions.assertEquals(ITEM_SKU, response.getItemSummaryDetailResponses().get(0).getItemSku());
    Assertions.assertTrue(response.getItemSummaryDetailResponses().get(0).isDistribution());
  }


  @Test
  public void convertToItemSummaryDetailsPageResponseFailed() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.modelConverterImpl.convertToItemSummaryDetailListResponse(ModelConverterImplTest.REQUEST_ID, 10, 1, null,
            Constants.ALL));
  }

  @Test
  public void convertToProductL3ResponseTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "ranchIntegrationEnabled", true);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    productanditemsvo.getProduct().setUrl(URL);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertEquals(URL, result.getUrl());
  }

  @Test
  public void convertToProductL3ResponseAddDeleteVariantSwitchTest() throws JsonProcessingException {
    ReflectionTestUtils.setField(modelConverterImpl, "addDeleteVariantSwitch", true);
    ReflectionTestUtils.setField(modelConverterImpl, "ranchIntegrationEnabled", true);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    Video video = new Video();
    video.setCoverImagePath(COVER_IMAGE_PATH);
    video.setFinalUrl(FINAL_IMAGE_URL);
    this.productanditemsvo.getProduct().setVideo(video);
    DistributionInfoDTO distributionInfoDTO = new DistributionInfoDTO();
    distributionInfoDTO.setProductName(PRODUCT_NAME);
    productanditemsvo.getProduct().getMasterDataProduct()
        .setDistributionInfo(mapper.writeValueAsString(distributionInfoDTO));
    when(objectMapper.readValue(productanditemsvo.getProduct().getMasterDataProduct().getDistributionInfo(),
        DistributionInfoDTO.class)).thenReturn(distributionInfoDTO);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(objectMapper).readValue(productanditemsvo.getProduct().getMasterDataProduct().getDistributionInfo(),
        DistributionInfoDTO.class);
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertEquals(COVER_IMAGE_PATH , result.getCoverImagePath());
    assertEquals(FINAL_IMAGE_URL , result.getVideoUrl());
    assertEquals(PRODUCT_NAME, result.getDistributionInfoDTO().getProductName());
  }

  @Test
  public void convertToProductL3ResponseAddDeleteVariantExceptionSwitchTest() throws JsonProcessingException {
    ReflectionTestUtils.setField(modelConverterImpl, "addDeleteVariantSwitch", true);
    ReflectionTestUtils.setField(modelConverterImpl, "ranchIntegrationEnabled", true);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    Video video = new Video();
    video.setCoverImagePath(COVER_IMAGE_PATH);
    video.setFinalUrl(FINAL_IMAGE_URL);
    this.productanditemsvo.getProduct().setVideo(video);
    DistributionInfoDTO distributionInfoDTO = new DistributionInfoDTO();
    distributionInfoDTO.setProductName(PRODUCT_NAME);
    productanditemsvo.getProduct().getMasterDataProduct()
        .setDistributionInfo(mapper.writeValueAsString(distributionInfoDTO));
    doThrow(JsonProcessingException.class).when(objectMapper)
        .readValue(productanditemsvo.getProduct().getMasterDataProduct().getDistributionInfo(),
            DistributionInfoDTO.class);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(objectMapper).readValue(productanditemsvo.getProduct().getMasterDataProduct().getDistributionInfo(),
        DistributionInfoDTO.class);
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertEquals(COVER_IMAGE_PATH , result.getCoverImagePath());
    assertEquals(FINAL_IMAGE_URL , result.getVideoUrl());
  }

  @Test
  public void convertToProductL3ResponseAddDeleteVariantSwitchItemMFDTrueTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "addDeleteVariantSwitch", true);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    productanditemsvo.getItems().get(0).setMarkForDelete(true);
    productanditemsvo.getItems().get(0).setPermanentDelete(true);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (1));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
  }

  @Test
  public void convertToProductL3ResponseNullTest() {
    productanditemsvo.getProduct().setProductScore(null);
    productanditemsvo.getProduct().setBundleProduct(true);
    productanditemsvo.getProduct().setSizeChartCode(ModelConverterImplTest.SIZE_CHART_CODE);
    productanditemsvo.getProduct().setDimensionsMissing(Boolean.TRUE);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertTrue(result.isBundleProduct());
    Assertions.assertEquals(ModelConverterImplTest.SIZE_CHART_CODE, result.getSizeChartCode());
    Assertions.assertEquals(Boolean.TRUE,result.getDimensionsMissing());
  }

  @Test
  public void convertToProductL3ResponseDimensionMissingNullMissingFieldPresentTest() {
    productanditemsvo.getProduct().setProductScore(null);
    productanditemsvo.getProduct().setBundleProduct(true);
    productanditemsvo.getProduct().setSizeChartCode(ModelConverterImplTest.SIZE_CHART_CODE);
    productanditemsvo.getProduct().setMissingFields(Set.of(Constants.DIMENSIONS_MISSING));
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertTrue(result.isBundleProduct());
    Assertions.assertEquals(ModelConverterImplTest.SIZE_CHART_CODE, result.getSizeChartCode());
    Assertions.assertEquals(Boolean.TRUE,result.getDimensionsMissing());
  }

  @Test
  public void convertToProductL3Response_WholesalePriceExistsTest() {
    itemAForGetProductAndItems.setWholesalePriceExists(true);
    itemAForGetProductAndItems.setActivePromoBundlings(new HashSet<>(Arrays.asList(Constants.WHOLESALE_PRICE)));
    itemAForGetProductAndItems.setContentChanged(true);
    itemAForGetProductAndItems.setSourceItemCode(SOURCE_ITEM_CODE);
    itemBForGetProductAndItems.setWholesalePriceExists(true);
    this.itemList = new ArrayList<>();
    this.itemList.add(this.itemAForGetProductAndItems);
    this.itemList.add(this.itemBForGetProductAndItems);
    this.productanditemsvo.setItems(this.itemList);
    itemResponse1.setContentChanged(true);
    itemResponse1.setSourceItemCode(SOURCE_ITEM_CODE);
    productanditemsvo.getProduct().setUrl(URL);
    when(this.gdnMapperHelper.mapBean(itemAForGetProductAndItems, ItemResponse.class)).thenReturn(itemResponse1);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(URL, result.getUrl());
  }

  @Test
  public void convertToProductL3Response_promoDiscountTest() {
    productanditemsvo.getProduct().setSynchronized(true);
    itemAForGetProductAndItems.setMerchantPromoDiscount(true);
    itemAForGetProductAndItems.setSynchronized(true);
    itemAForGetProductAndItems.setMerchantPromoDiscountActive(true);
    itemBForGetProductAndItems.setMerchantPromoDiscount(true);
    itemBForGetProductAndItems.setSynchronized(true);
    itemBForGetProductAndItems.setMerchantPromoDiscountActive(true);
    this.itemList = new ArrayList<>();
    this.itemList.add(this.itemAForGetProductAndItems);
    this.itemList.add(this.itemBForGetProductAndItems);
    this.productanditemsvo.setItems(this.itemList);
    itemResponse1.setContentChanged(true);
    itemResponse1.setSourceItemCode(SOURCE_ITEM_CODE);
    when(this.gdnMapperHelper.mapBean(itemAForGetProductAndItems, ItemResponse.class)).thenReturn(itemResponse1);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
  }

  @Test
  public void convertToProductL3ResponseTest_WithMasterDataItem() {
    productanditemsvo.getItems().get(0).getMasterDataItem().setHash(HASH_VALUE);
    productanditemsvo.getItems().get(0).setMarkForDelete(true);
    when(this.masterDataConstructorService
        .constructItemDimensionFields(this.productanditemsvo.getItems().get(0).getMasterDataItem(),
            this.productanditemsvo.getProduct().getMasterDataProduct()))
        .thenReturn(this.productanditemsvo.getItems().get(0).getMasterDataItem());
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (1));
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
  }

  @Test
  public void convertToProductL3ResponseTestWithNullProductAndItemsVO() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.modelConverterImpl.convertToProductL3Response(null));
  }

  @Test
  public void convertToProductL3Response_nullMasterDataProduct_success() {
    this.productanditemsvo.getProduct().setMasterDataProduct(null);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
    assertNull(result.getMasterCatalog());
  }

  @Test
  public void convertToProductL3ResponseTest_disabledSyncCategory() {
    productL3Response.setMasterCatalog(new MasterCatalogDTO());
    productL3Response.getMasterCatalog().setCategory(categoryDTO);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(Mockito.anyString(), any());
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItemCount(), (2));
  }

  @Test
  public void convertToProductL3ResponseWithPreOrderDateLessThanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(Constants.DATE);
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertFalse(result.isMarkForDelete());
    assertTrue(result.getPreOrderDTO().getIsPreOrder());
    assertEquals(Constants.DATE, result.getPreOrderDTO().getPreOrderType());
  }

  @Test
  public void convertToProductL3ResponseWithPreOrderDateGreaterThanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderValue(10);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    productanditemsvo.getItems().get(0).setActivePromoBundlings(null);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertFalse(result.isMarkForDelete());
    assertTrue(result.getPreOrderDTO().getIsPreOrder());
    assertEquals(Constants.WEEK, result.getPreOrderDTO().getPreOrderType());
    assertEquals(10, (int) result.getPreOrderDTO().getPreOrderValue());
    assertFalse(result.isActiveFreeSamplePromo());
  }

  @Test
  public void convertToProductL3ResponseWithPreOrderNullTest() {
    preOrder.setIsPreOrder(false);
    productanditemsvo.getProduct().setPreOrder(preOrder);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertFalse(result.isMarkForDelete());
    assertFalse(result.getPreOrderDTO().getIsPreOrder());
    assertFalse(result.isActiveFreeSamplePromo());
  }

  @Test
  public void convertToProductL3ResponseWithPreOrderFalseTest() {
    productanditemsvo.getProduct().setPreOrder(null);
    productanditemsvo.getItems().get(0).getActivePromoBundlings().add(Constants.COMPLEMENTARY_FREE_SAMPLE_PRODUCT);
    ProductL3Response result = this.modelConverterImpl.convertToProductL3Response(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductL3Response.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertNotNull(result);
    assertEquals(result.getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getDescriptiveAttributes().size(), (0));
    assertEquals(90.0, result.getProductScore().getTotalScore(), 0);
    assertFalse(result.isMarkForDelete());
    assertFalse(result.getPreOrderDTO().getIsPreOrder());
  }

  @Test
  public void convertToItemListingUpdateRequestVoOnlineTest() {
    quickEditUpdateRequest.setStatus(Constants.ONLINE);
    List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
        .convertToItemListingUpdateRequestVo(new ItemListingUpdateRequest(ProductType.REGULAR, Arrays.asList(quickEditUpdateRequest)));
    Assertions.assertEquals(ITEM_SKU, itemListingUpdateRequestVos.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, itemListingUpdateRequestVos.get(0).getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemListingUpdateRequestVos.get(0).getPickupPointCode());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).getWholesalePriceActivated());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).getOff2OnChannelActive());
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    Assertions.assertTrue(itemViewConfig.isBuyable());
    Assertions.assertTrue(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemListingUpdateRequestVoOfflineTest() {
    quickEditUpdateRequest.setStatus(Constants.OFFLINE);
    List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
        .convertToItemListingUpdateRequestVo(new ItemListingUpdateRequest(ProductType.REGULAR, Arrays.asList(quickEditUpdateRequest)));
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    assertFalse(itemViewConfig.isBuyable());
    assertFalse(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemListingUpdateRequestVoTeaserTest() {
    quickEditUpdateRequest.setStatus(Constants.TEASER);
    List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
        .convertToItemListingUpdateRequestVo(new ItemListingUpdateRequest(ProductType.REGULAR, Arrays.asList(quickEditUpdateRequest)));
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    assertFalse(itemViewConfig.isBuyable());
    Assertions.assertTrue(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemListingUpdateRequestVoB2BTest() {
    quickEditUpdateRequest.setStatus(Constants.B2B);
    List<ItemListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
        .convertToItemListingUpdateRequestVo(new ItemListingUpdateRequest(ProductType.REGULAR, Arrays.asList(quickEditUpdateRequest)));
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    Assertions.assertTrue(itemViewConfig.isBuyable());
    assertFalse(itemViewConfig.isDiscoverable());
  }

  @Test
  public void getItemPickupPointCodeResponseFromItemPickupPointVoTest() {
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setItemName(ITEM_NAME);
    itemPickupPointVo.setItemSku(ITEM_SKU);
    itemPickupPointVo.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointVo.setPickupPointName(PICKUP_POINT_NAME);
    List<ItemPickupPointCodeResponse> itemPickupPointCodeResponses =
        modelConverterImpl.getItemPickupPointCodeResponseFromItemPickupPointVo(Arrays.asList(itemPickupPointVo));
    Assertions.assertEquals(ITEM_SKU, itemPickupPointCodeResponses.get(0).getItemSku());
    Assertions.assertEquals(ITEM_NAME, itemPickupPointCodeResponses.get(0).getItemName());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemPickupPointCodeResponses.get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_NAME, itemPickupPointCodeResponses.get(0).getPickupPointName());

  }

  @Test
  public void toItemPickupPointFromItemTest() {
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku(ITEM_SKU);
    itemVo.setProductSku(PRODUCT_SKU);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    List<ItemPickupPointVo> itemPickupPoint =
        modelConverterImpl.toItemPickupPointFromItemVo(itemVo, Arrays.asList(itemPickupPointActivationRequest), PRODUCT_SKU, true);
    Assertions.assertEquals(ITEM_SKU, itemPickupPoint.get(0).getItemSku());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPoint.get(0).getProductSku());
    Assertions.assertNotNull(itemPickupPoint.get(0).getPrice());
    Assertions.assertNotNull(itemPickupPoint.get(0).getItemViewConfig());
    Assertions.assertTrue(itemPickupPoint.get(0).isDelivery());
  }

  @Test
  public void toItemPickupPointFromItemOnlineFlaseTest() {
    ItemVo itemVo = new ItemVo();
    itemVo.setItemSku(ITEM_SKU);
    itemVo.setProductSku(PRODUCT_SKU);
    itemPickupPointActivationRequest.setItemViewConfigs(ImmutableSet.of(itemViewConfigAndItemSkuRequest));
    itemPickupPointActivationRequest.setPrice(ImmutableSet.of(priceRequest));
    List<ItemPickupPointVo> itemPickupPoint =
        modelConverterImpl.toItemPickupPointFromItemVo(itemVo, Arrays.asList(itemPickupPointActivationRequest, itemPickupPointActivationRequest), PRODUCT_SKU, false);
    Assertions.assertEquals(ITEM_SKU, itemPickupPoint.get(0).getItemSku());
    Assertions.assertEquals(PRODUCT_SKU, itemPickupPoint.get(0).getProductSku());
    Assertions.assertNotNull(itemPickupPoint.get(0).getPrice());
    Assertions.assertNotNull(itemPickupPoint.get(0).getItemViewConfig());
    assertFalse(itemPickupPoint.get(0).isDelivery());
  }

  @Test
  public void convertToProductAndItemsResponseTest() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImagePathList","resize/,commerce/");
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    List<MasterDataProductImageDTO> masterDataProductImagesList = new ArrayList<>();
    MasterDataProductImageDTO masterDataProductImage = new MasterDataProductImageDTO();
    masterDataProductImage.setLocationPath("resize/");
    masterDataProductImagesList.add(masterDataProductImage);
    MasterDataProductImageDTO masterDataProductImage2 = new MasterDataProductImageDTO();
    masterDataProductImage2.setLocationPath("commerce/");
    masterDataProductImagesList.add(masterDataProductImage2);
    MasterDataProductImageDTO masterDataProductImage3 = new MasterDataProductImageDTO();
    masterDataProductImage3.setLocationPath("remained/");
    masterDataProductImagesList.add(masterDataProductImage3);
    masterDataProduct.setMasterDataProductImages(masterDataProductImagesList);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productItemsVo.getProductVo().getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    productItemsVo.getProductVo().setSizeChartCode(SIZE_CHART_CODE);
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(SIZE_CHART_CODE, productAndItemsResponse.getProduct().getSizeChartCode());
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
  }

  @Test
  public void convertToProductAndItemsResponseProductTypeNullTest() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "resizeImageRemoval", true);
    ReflectionTestUtils.setField(modelConverterImpl, "resizeImagePathList", "resize/,commerce/");
    ReflectionTestUtils.setField(modelConverterImpl, "setDefaultProductType", true);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    List<MasterDataProductImageDTO> masterDataProductImagesList = new ArrayList<>();
    MasterDataProductImageDTO masterDataProductImage = new MasterDataProductImageDTO();
    masterDataProductImage.setLocationPath("resize/");
    masterDataProductImagesList.add(masterDataProductImage);
    MasterDataProductImageDTO masterDataProductImage2 = new MasterDataProductImageDTO();
    masterDataProductImage2.setLocationPath("commerce/");
    masterDataProductImagesList.add(masterDataProductImage2);
    MasterDataProductImageDTO masterDataProductImage3 = new MasterDataProductImageDTO();
    masterDataProductImage3.setLocationPath("remained/");
    masterDataProductImagesList.add(masterDataProductImage3);
    masterDataProduct.setMasterDataProductImages(masterDataProductImagesList);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.setProductType(ProductType.REGULAR);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    productItemsVo.getProductVo().setProductType(ProductType.REGULAR);
    List<ProductSpecialAttribute> productSpecialAttributeList = new ArrayList<>();
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE1);
    productSpecialAttribute.setAttributeValue(ATTRIBUTE_VALUE);
    productSpecialAttributeList.add(productSpecialAttribute);
    productItemsVo.getProductVo().setProductSpecialAttributes(productSpecialAttributeList);
    productItemsVo.getProductVo().getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService)
        .sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertFalse(productAndItemsResponse.getProduct().isImeiRequired());
    Assertions.assertEquals(1,
        productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
  }

  @Test
  public void convertToProductAndItemsResponseProductTypeNullSizeChartTrueTest() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "resizeImageRemoval", true);
    ReflectionTestUtils.setField(modelConverterImpl, "resizeImagePathList", "resize/,commerce/");
    ReflectionTestUtils.setField(modelConverterImpl, "setDefaultProductType", true);
    ReflectionTestUtils.setField(modelConverterImpl, "valueTypeAdditionForDefiningAttributes", true);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    List<MasterDataProductImageDTO> masterDataProductImagesList = new ArrayList<>();
    MasterDataProductImageDTO masterDataProductImage = new MasterDataProductImageDTO();
    masterDataProductImage.setLocationPath("resize/");
    masterDataProductImagesList.add(masterDataProductImage);
    MasterDataProductImageDTO masterDataProductImage2 = new MasterDataProductImageDTO();
    masterDataProductImage2.setLocationPath("commerce/");
    masterDataProductImagesList.add(masterDataProductImage2);
    MasterDataProductImageDTO masterDataProductImage3 = new MasterDataProductImageDTO();
    masterDataProductImage3.setLocationPath("remained/");
    masterDataProductImagesList.add(masterDataProductImage3);
    masterDataProduct.setMasterDataProductImages(masterDataProductImagesList);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.setProductType(ProductType.REGULAR);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    productItemsVo.getProductVo().setProductType(ProductType.REGULAR);
    productItemsVo.getProductVo().getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService)
        .sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,
        productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
  }

  @Test
  public void convertToProductAndItemsResponseProductTypeNull2Test() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "resizeImageRemoval", true);
    ReflectionTestUtils.setField(modelConverterImpl, "resizeImagePathList", "resize/,commerce/");
    ReflectionTestUtils.setField(modelConverterImpl, "setDefaultProductType", true);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    List<MasterDataProductImageDTO> masterDataProductImagesList = new ArrayList<>();
    MasterDataProductImageDTO masterDataProductImage = new MasterDataProductImageDTO();
    masterDataProductImage.setLocationPath("resize/");
    masterDataProductImagesList.add(masterDataProductImage);
    MasterDataProductImageDTO masterDataProductImage2 = new MasterDataProductImageDTO();
    masterDataProductImage2.setLocationPath("commerce/");
    masterDataProductImagesList.add(masterDataProductImage2);
    MasterDataProductImageDTO masterDataProductImage3 = new MasterDataProductImageDTO();
    masterDataProductImage3.setLocationPath("remained/");
    masterDataProductImagesList.add(masterDataProductImage3);
    masterDataProduct.setMasterDataProductImages(masterDataProductImagesList);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productItemsVo.getProductVo().getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productResponse.setMasterDataProduct(masterDataProduct);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    productItemsVo.getProductVo().setProductType(null);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService)
        .sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,
        productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(ProductType.REGULAR, productAndItemsResponse.getProduct().getProductType());
  }

  @Test
  public void convertToProductAndItemsResponseL5MfdTest() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImagePathList","resize/,commerce/");
    ReflectionTestUtils.setField(modelConverterImpl,"setItemPickUpPointMarkForDeleteValue",true);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    List<MasterDataProductImageDTO> masterDataProductImagesList = new ArrayList<>();
    MasterDataProductImageDTO masterDataProductImage = new MasterDataProductImageDTO();
    masterDataProductImage.setLocationPath("resize/");
    masterDataProductImagesList.add(masterDataProductImage);
    MasterDataProductImageDTO masterDataProductImage2 = new MasterDataProductImageDTO();
    masterDataProductImage2.setLocationPath("commerce/");
    masterDataProductImagesList.add(masterDataProductImage2);
    MasterDataProductImageDTO masterDataProductImage3 = new MasterDataProductImageDTO();
    masterDataProductImage3.setLocationPath("remained/");
    masterDataProductImagesList.add(masterDataProductImage3);
    masterDataProduct.setMasterDataProductImages(masterDataProductImagesList);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productItemsVo.getProductVo().getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());
    productResponse.setMasterDataProduct(masterDataProduct);
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setFbbActivated(true);
    itemPickupPointVo.setMarkForDelete(true);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    ItemViewConfigDTO itemViewConfig = new ItemViewConfigDTO();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemResponse1.setItemViewConfigs(new HashSet<>(Collections.singletonList(itemViewConfig)));
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
  }

  @Test
  public void convertToProductAndItemsResponseNullMasterDataTest() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImagePathList","resize/,commerce/");
    ReflectionTestUtils.setField(modelConverterImpl,"setItemPickUpPointMarkForDeleteValue",true);
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setFbbActivated(true);
    itemPickupPointVo.setMarkForDelete(true);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    ItemViewConfigDTO itemViewConfig = new ItemViewConfigDTO();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemResponse1.setItemViewConfigs(new HashSet<>(Collections.singletonList(itemViewConfig)));
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
      productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
      .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseSubscribableTrueTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    itemVo.setPreferredSubscriptionType(Collections.emptySet());
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertTrue(productAndItemsResponse.getItems().get(0).isSubscribable());
  }
  @Test
  public void convertToProductAndItemsResponseSubscribableTrueSetSize2Test() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    productVo.setCurationStatus(CurationStatus.APPROVED);
    Set<String> set = new HashSet<>(Arrays.asList(WAREHOUSE, MARKETPLACE));
    itemVo.setPreferredSubscriptionType(set);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertTrue(productAndItemsResponse.getItems().get(0).isSubscribable());
  }
  @Test
  public void convertToProductAndItemsResponseSubscribableTrueSetSize2DiffValueTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    Set<String> set = new HashSet<>(Arrays.asList(ITEM_CODE));
    itemVo.setPreferredSubscriptionType(set);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseSubscribableTrueSetSize1Test() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Set<String> set = new HashSet<>(Arrays.asList(WAREHOUSE));
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setFbbActivated(true);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    itemVo.setPreferredSubscriptionType(set);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertTrue(productAndItemsResponse.getItems().get(0).isSubscribable());
  }

  @Test
  public void convertToProductAndItemsResponseSubscribableTrueSetSize1WhTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    itemResponse1.setSubscribable(true);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Set<String> set = new HashSet<>(Arrays.asList(WAREHOUSE, ITEM_CODE));
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setFbbActivated(true);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    itemVo.setPreferredSubscriptionType(set);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertTrue(productAndItemsResponse.getItems().get(0).isSubscribable());
  }

  @Test
  public void convertToProductAndItemsResponseSubscribableTrueSetSize1MKTTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    Set<String> set = new HashSet<>(Arrays.asList(MARKETPLACE));
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setFbbActivated(false);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    itemVo.setPreferredSubscriptionType(set);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    itemVo.getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertTrue(productAndItemsResponse.getItems().get(0).isSubscribable());
    assertEquals(productAndItemsResponse.getProduct().isHalalProduct(),
        CurationStatus.APPROVED.equals(productVo.getCurationStatus()));
  }
  @Test
  public void convertToProductAndItemsResponseSubscribableTrueSetSize1MKTFbbTrueTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    itemVo.setSubscribable(true);
    productVo.setCurationStatus(CurationStatus.REJECTED);
    Set<String> set = new HashSet<>(Arrays.asList(MARKETPLACE));
    ItemPickupPointVo itemPickupPointVo = new ItemPickupPointVo();
    itemPickupPointVo.setFbbActivated(true);
    itemVo.setItemPickupPointVoList(Collections.singletonList(itemPickupPointVo));
    itemVo.setPreferredSubscriptionType(set);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(Mockito.any(), Mockito.eq(ProductResponse.class))).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(Mockito.any(), Mockito.eq(ItemResponse.class))).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(Mockito.any(), Mockito.eq(ProductResponse.class));
    Mockito.verify(this.gdnMapperHelper).mapBean(Mockito.any(), Mockito.eq(ItemResponse.class));
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    assertFalse(productAndItemsResponse.getItems().get(0).isSubscribable());
    assertEquals(productAndItemsResponse.getProduct().isHalalProduct(),
        CurationStatus.APPROVED.equals(productVo.getCurationStatus()));
  }

  @Test
  public void convertToProductAndItemsResponse_nullProductScoreTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.setProductScore(null);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(
      this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
      .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
      .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponse_noneEmptyHashTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    masterDataItem.setHash(HASH);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
      .thenReturn(productResponse);
    Mockito.when(
      this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
      .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponse_nullMasterDataProductTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getProductVo().setMasterDataProduct(null);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
      .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
      .constructItemDimensionFields(masterDataItem, null);
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponse_nullMasterDataItemTest() throws Exception {
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    MasterDataProductAttributeDTO masterDataProductAttributeDTO = new MasterDataProductAttributeDTO();
    MasterDataProductAttributeValueDTO masterDataProductAttributeValueDTO  = new MasterDataProductAttributeValueDTO();
    masterDataProductAttributeDTO.setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValueDTO));
    masterDataProductAttributeDTO.setMasterDataAttribute(new MasterDataAttributeDTO());
    masterDataProduct.setMasterDataProductAttributes(Collections.singletonList(masterDataProductAttributeDTO));
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productVo.getMasterDataProduct().getMasterDataProductAttributes().get(0).setMasterDataProductAttributeValues(new ArrayList<>());
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productItemsVo.getItemVoList().get(0).setMasterDataItem(null);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productResponse.setMasterDataProduct(new MasterDataProductDTO());
    productResponse.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
      .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.WEEK);
    preOrder.setPreOrderValue(5);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsConvertPreOrderDetailsFalseTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.WEEK);
    preOrder.setPreOrderValue(5);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, false, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsPreOrderTypeDAYSTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.DAYS);
    preOrder.setPreOrderValue(5);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsPreOrderTypeNewDateLessTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    productVo.setCurationStatus(CurationStatus.APPROVED);
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    Date date = new Date();
    Calendar c = Calendar.getInstance();
    c.setTime(date);
    c.add(Calendar.DATE, -1);
    date = c.getTime();
    preOrder.setPreOrderDate(date);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsPreOrderTypeNew_DateTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    Date date = new Date();
    preOrder.setPreOrderDate(date);
    productVo.setPreOrder(preOrder);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsPreOrderTypeNew_DateTest_cncActive() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    Date date = new Date();
    preOrder.setPreOrderDate(date);
    productVo.setPreOrder(preOrder);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsPreOrderTypeNewConvertPreOrderDetailsFalseTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    preOrder.setPreOrderDate(new Date());
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.setProductVo(productVo);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, false, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(DEFAULT_STORE_ID, CATEGORY_CODE_VARIABLE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetails_nullProductScoreTest() {
    productVo.setProductScore(null);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, false, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetails_noneEmptyHashTest() {
    masterDataItem.setHash(HASH);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetails_nullMasterDataProductTest() {
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.getProductVo().setMasterDataProduct(null);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, null);
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetails_nullMasterDataItemTest() {
    productVo.setCurationStatus(CurationStatus.APPROVED);
    productItemsVo.getItemVoList().get(0).setMasterDataItem(null);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
            productItemsVo, true, null, false);
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsWithImeiRequiredFieldTest() {
    ReflectionTestUtils.setField(modelConverterImpl,"imeiAttributeCode",IMEI_ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(modelConverterImpl,"imeiAllowedValues", IMEI_ATTRIBUTE_VALUES);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    ProductSpecialAttribute specialAttribute = new ProductSpecialAttribute();
    specialAttribute.setAttributeCode(IMEI_ATTRIBUTE_CODE);
    specialAttribute.setAttributeValue(IMEI_ATTRIBUTE_VALUES.getFirst());
    productVo.setProductSpecialAttributes(List.of(specialAttribute));
    productItemsVo.getItemVoList().get(0).setMasterDataItem(null);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
      .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
        productItemsVo, true, null, false);
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertTrue(productAndItemsResponse.getProduct().isImeiRequired());
  }

  @Test
  public void convertToProductAndItemsResponseWithConvertPreOrderDetailsWithImeiRequiredFieldTest1() {
    ReflectionTestUtils.setField(modelConverterImpl,"imeiAttributeCode",IMEI_ATTRIBUTE_CODE);
    ReflectionTestUtils.setField(modelConverterImpl,"imeiAllowedValues", IMEI_ATTRIBUTE_VALUES);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    ProductSpecialAttribute specialAttribute = new ProductSpecialAttribute();
    specialAttribute.setAttributeCode(IMEI_ATTRIBUTE_CODE_1);
    specialAttribute.setAttributeValue(IMEI_ATTRIBUTE_VALUES.getFirst());
    productVo.setProductSpecialAttributes(List.of(specialAttribute));
    productItemsVo.getItemVoList().get(0).setMasterDataItem(null);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
      .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
      this.modelConverterImpl.convertToProductAndItemsResponseWithConvertPreOrderDetails(
        productItemsVo, true, null, false);
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertFalse(productAndItemsResponse.getProduct().isImeiRequired());
  }

  @Test
  public void convertToProductAndItemDataResponseConvertPreOrderDetailsTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setDocumentType(DOCUMENT_TYPE);
    productResponse.setMasterCatalog(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    Mockito.when(productHelperService.getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryResponse);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.WEEK);
    preOrder.setPreOrderValue(5);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    productItemsVo.setProductVo(productVo);
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Mockito.verify(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponseConvertPreOrderDetailsConvertPreOrderDetailsFalseTest() {
    productResponse.setMasterCatalog(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    Mockito.when(productHelperService.getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(null);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.WEEK);
    preOrder.setPreOrderValue(5);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    productItemsVo.setProductVo(productVo);
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Mockito.verify(productHelperService).getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponseConvertPreOrderDetailsPreOrderTypeDAYSTest() {
    CategoryResponse categoryResponse = new CategoryResponse();
    productResponse.setMasterCatalog(new MasterCatalogDTO(CATALOG_CODE, new CategoryDTO(CATEGORY_CODE, CATEGORY_CODE)));
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    Mockito.when(productHelperService.getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE)).thenReturn(categoryResponse);
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.DAYS);
    preOrder.setPreOrderValue(5);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    productItemsVo.setProductVo(productVo);
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Mockito.verify(productHelperService)
        .getCategoryResponseByCategoryCode(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, CATEGORY_CODE);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponsePreOrderTypeNewDateLessTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    Date date = new Date();
    Calendar c = Calendar.getInstance();
    c.setTime(date);
    c.add(Calendar.DATE, -1);
    date = c.getTime();
    preOrder.setPreOrderDate(date);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    productItemsVo.setProductVo(productVo);
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponse_DateTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    Date date = new Date();
    preOrder.setPreOrderDate(date);
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    productItemsVo.setProductVo(productVo);
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponseTest() {
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductVo productVo = new ProductVo();
    PreOrder preOrder = new PreOrder();
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderType(Constants.ACTIVE);
    preOrder.setPreOrderValue(5);
    preOrder.setPreOrderDate(new Date());
    productVo.setPreOrder(preOrder);
    productVo.setStoreId(DEFAULT_STORE_ID);
    productItemsVo.setProductVo(productVo);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponse_nullProductScoreTest() {
    productVo.setProductScore(null);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponse_noneEmptyHashTest() {
    masterDataItem.setHash(HASH);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(
        this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
            productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponse_nullMasterDataProductTest() {
    productItemsVo.getProductVo().setMasterDataProduct(null);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, null));
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, null);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToProductAndItemDataResponse_nullMasterDataItemTest() {
    productItemsVo.getItemVoList().get(0).setMasterDataItem(null);
    productVo.setMasterCatalog(new MasterCatalog(CATALOG_CODE, new Category(CATEGORY_CODE, CATEGORY_CODE)));
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class))
        .thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemDataResponse.class)).thenReturn(new ItemDataResponse());
    ProductAndItemDataResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemDataResponse(productItemsVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemDataResponse.class);
    Assertions.assertNotNull(productAndItemsResponse);
  }

  @Test
  public void convertToMasterDataWithProductItemsVoTest() {
    ProductAndItemsDTO productAndItemsDTO = new ProductAndItemsDTO();
    productAndItemsDTO.setItems(Arrays.asList(itemResponse1));
    MasterDataWithProductItemsVo masterDataDetailResponseVo =
      CommonUtil.toMasterDataWithProductItemsVo(
        generateMasterDataDetailWithProductAndItemsResponseVo());
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
      .thenReturn(masterDataDetailWithProductAndItemsResponse);
    when(this.gdnMapperHelper.mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo(),
      ProductResponse.class)).thenReturn(productResponse);
    when(this.gdnMapperHelper.mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getItemVoList().get(0),
      ItemResponse.class)).thenReturn(itemResponse1);
    MasterDataDetailWithProductAndItemsResponse response =
      this.modelConverterImpl.convertToMasterDataWithProductItemsVo(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.gdnMapperHelper).mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo(), ProductResponse.class);
    verify(this.gdnMapperHelper).mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getItemVoList().get(0),
      ItemResponse.class);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
  }

  @Test
  public void convertToMasterDataWithProductItemsVo_synchronizedTest() {
    ProductAndItemsDTO productAndItemsDTO = new ProductAndItemsDTO();
    productAndItemsDTO.setItems(Arrays.asList(itemResponse1));
    MasterDataWithProductItemsVo masterDataDetailResponseVo =
      CommonUtil.toMasterDataWithProductItemsVo(
        generateMasterDataDetailWithProductAndItemsResponseVo());
    masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo().setSynchronized(true);
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
      .thenReturn(masterDataDetailWithProductAndItemsResponse);
    when(this.gdnMapperHelper.mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo(),
      ProductResponse.class)).thenReturn(productResponse);
    when(this.gdnMapperHelper.mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getItemVoList().get(0),
      ItemResponse.class)).thenReturn(itemResponse1);
    MasterDataDetailWithProductAndItemsResponse response =
      this.modelConverterImpl.convertToMasterDataWithProductItemsVo(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo(), ProductResponse.class);
    verify(this.gdnMapperHelper).mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getItemVoList().get(0),
      ItemResponse.class);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
  }

  @Test
  public void convertToMasterDataWithProductItemsVo_withMasterDataItemTest() {
    ProductAndItemsDTO productAndItemsDTO = new ProductAndItemsDTO();
    productAndItemsDTO.setItems(Arrays.asList(itemResponse1));
    MasterDataWithProductItemsVo masterDataDetailResponseVo =
      CommonUtil.toMasterDataWithProductItemsVo(
        generateMasterDataDetailWithProductAndItemsResponseVo());
    masterDataDetailResponseVo.setMasterDataItems(
      Collections.singletonMap(PRODUCT_CODE, masterDataItem));
    when(this.gdnMapperHelper.mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class))
      .thenReturn(masterDataDetailWithProductAndItemsResponse);
    when(this.gdnMapperHelper.mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo(),
      ProductResponse.class)).thenReturn(productResponse);
    when(this.gdnMapperHelper.mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getItemVoList().get(0),
      ItemResponse.class)).thenReturn(itemResponse1);
    MasterDataDetailWithProductAndItemsResponse response =
      this.modelConverterImpl.convertToMasterDataWithProductItemsVo(masterDataDetailResponseVo);
    verify(this.gdnMapperHelper).mapBean(masterDataDetailResponseVo, MasterDataDetailWithProductAndItemsResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    verify(this.masterDataConstructorService).constructItemDimensionFields(masterDataItem,
      null);
    verify(this.gdnMapperHelper).mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getProductVo(), ProductResponse.class);
    verify(this.gdnMapperHelper).mapBean(
      masterDataDetailResponseVo.getProductItemsVos().get(0).getItemVoList().get(0),
      ItemResponse.class);
    assertTrue(response.getProductAndItems().get(0).getItems().get(0).isContentChanged());
    assertEquals(SOURCE_ITEM_CODE, response.getProductAndItems().get(0).getItems().get(0).getSourceItemCode());
  }

  @Test
  public void convertToItemPickupPointListingUpdateRequestVoTest() {
    B2bFields b2bFields= new B2bFields();
    b2bFields.setStatus(Constants.ONLINE);
    itemPickupPointQuickEditRequest1.setB2bFields(b2bFields);
    itemPickupPointQuickEditRequest1.setStatus(Constants.ONLINE);
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
      .convertToItemPickupPointListingUpdateRequestVo(new ItemPickupPointListingUpdateRequest(ProductType.REGULAR, Arrays.asList(
          itemPickupPointQuickEditRequest1)));
    Assertions.assertEquals(ITEM_SKU, itemListingUpdateRequestVos.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, itemListingUpdateRequestVos.get(0).getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemListingUpdateRequestVos.get(0).getPickupPointCode());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).getWholesalePriceActivated());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).isOff2OnChannelActive());
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    Assertions.assertTrue(itemViewConfig.isBuyable());
    Assertions.assertTrue(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemPickupPointListingUpdateRequestVoForBuyableSchedulesTest() {
    B2bFields b2bFields= new B2bFields();
    b2bFields.setStatus(Constants.ONLINE);
    itemPickupPointQuickEditRequest1.setBuyableSchedule(
      BuyableScheduleRequest.builder().startDateTime(new Date()).build());
    itemPickupPointQuickEditRequest1.setB2bFields(b2bFields);
    itemPickupPointQuickEditRequest1.setStatus(Constants.ONLINE);
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
      .convertToItemPickupPointListingUpdateRequestVo(new ItemPickupPointListingUpdateRequest(ProductType.REGULAR, Arrays.asList(
        itemPickupPointQuickEditRequest1)));
    Assertions.assertEquals(ITEM_SKU, itemListingUpdateRequestVos.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, itemListingUpdateRequestVos.get(0).getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemListingUpdateRequestVos.get(0).getPickupPointCode());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).getWholesalePriceActivated());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).isOff2OnChannelActive());
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    Assertions.assertTrue(itemViewConfig.isBuyable());
    Assertions.assertTrue(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemPickupPointListingUpdateRequestVoForDiscoverableSchedulesTest() {
    B2bFields b2bFields= new B2bFields();
    b2bFields.setStatus(Constants.ONLINE);
    itemPickupPointQuickEditRequest1.setDiscoverableSchedule(
      DiscoverableScheduleRequest.builder().startDateTime(new Date()).endDateTime(new Date()).build());
    itemPickupPointQuickEditRequest1.setB2bFields(b2bFields);
    itemPickupPointQuickEditRequest1.setStatus(Constants.ONLINE);
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
      .convertToItemPickupPointListingUpdateRequestVo(new ItemPickupPointListingUpdateRequest(ProductType.REGULAR, Arrays.asList(
        itemPickupPointQuickEditRequest1)));
    Assertions.assertEquals(ITEM_SKU, itemListingUpdateRequestVos.get(0).getItemSku());
    Assertions.assertEquals(MERCHANT_SKU, itemListingUpdateRequestVos.get(0).getMerchantSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, itemListingUpdateRequestVos.get(0).getPickupPointCode());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).getWholesalePriceActivated());
    Assertions.assertTrue(itemListingUpdateRequestVos.get(0).isOff2OnChannelActive());
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    Assertions.assertTrue(itemViewConfig.isBuyable());
    Assertions.assertTrue(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemPickupPointListingUpdateRequestVoOfflineTest() {
    itemPickupPointQuickEditRequest1.setStatus(Constants.OFFLINE);
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
      .convertToItemPickupPointListingUpdateRequestVo(new ItemPickupPointListingUpdateRequest(ProductType.REGULAR, Arrays.asList(
          itemPickupPointQuickEditRequest1)));
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    assertFalse(itemViewConfig.isBuyable());
    assertFalse(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemPickupPointListingUpdateRequestVoTeaserTest() {
    itemPickupPointQuickEditRequest1.setStatus(Constants.TEASER);
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
      .convertToItemPickupPointListingUpdateRequestVo(new ItemPickupPointListingUpdateRequest(ProductType.REGULAR, Arrays.asList(
          itemPickupPointQuickEditRequest1)));
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    assertFalse(itemViewConfig.isBuyable());
    Assertions.assertTrue(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToItemPickupPointListingUpdateRequestVoB2BTest() {
    itemPickupPointQuickEditRequest1.setStatus(Constants.B2B);
    List<ItemPickupPointListingUpdateRequestVo> itemListingUpdateRequestVos = modelConverterImpl
      .convertToItemPickupPointListingUpdateRequestVo(new ItemPickupPointListingUpdateRequest(ProductType.REGULAR, Arrays.asList(
          itemPickupPointQuickEditRequest1)));
    ItemViewConfig itemViewConfig = itemListingUpdateRequestVos.get(0).getItemViewConfigs().stream().findFirst().get();
    Assertions.assertTrue(itemViewConfig.isBuyable());
    assertFalse(itemViewConfig.isDiscoverable());
  }

  @Test
  public void convertToProductAndItemInfoResponseV2Test() {
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class)).thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
  }

  @Test
  public void convertToProductAndItemInfoResponseV2WithPreOrderTest() {
    productInfoResponse.setPreOrder(preOrderDTO);
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(PREORDER_VALUE, result.getProduct().getPreOrder().getPreOrderValue());
    assertNotNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseV2WithPreOrderForWeekTypeTest() {
    preOrderDTO.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    productInfoResponse.setPreOrder(preOrderDTO);
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class)).thenReturn(
        productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
    assertEquals(TOTAL_DAYS, result.getProduct().getPreOrder().getPreOrderValue());
    assertNotNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseV2WithDateLessThanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, -10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertFalse(result.getProduct().getPreOrder().getIsPreOrder());
    assertNull(result.getProduct().getPreOrder().getPreOrderType());
    assertNull(result.getProduct().getPreOrder().getPreOrderValue());
    assertNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseV2WithPreorderDateGreaterthanCurrentDateTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 10);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType("DATE");
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(Constants.DATE, result.getProduct().getPreOrder().getPreOrderType());
    assertNotNull(result.getProduct().getPreOrder().getPreOrderDate());
  }

  @Test
  public void convertToProductAndItemInfoResponseV2WithDateMoreThanCurrentDateAndTypeWeekTest() {
    Date currentDate = new Date();
    Calendar cal = Calendar.getInstance();
    cal.setTime(currentDate);
    cal.add(Calendar.DATE, 1);
    preOrder.setIsPreOrder(true);
    preOrder.setPreOrderDate(cal.getTime());
    preOrder.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrder.setPreOrderValue(10);
    preOrderDTO.setPreOrderDate(cal.getTime());
    preOrderDTO.setPreOrderType(PREORDER_WEEK_TYPE);
    preOrderDTO.setIsPreOrder(true);
    productInfoResponse.setPreOrder(preOrderDTO);
    productVo.setPreOrder(preOrder);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class))
        .thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertTrue(result.getProduct().getPreOrder().getIsPreOrder());
    assertEquals(TOTAL_DAYS, result.getProduct().getPreOrder().getPreOrderValue());
    assertEquals(PREORDER_TYPE, result.getProduct().getPreOrder().getPreOrderType());
  }

  @Test
  public void convertToProductAndItemInfoResponseV2NullMasterDataItemTest() {
    itemVo.setMasterDataItem(null);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class)).thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
  }

  @Test
  public void convertToProductAndItemInfoResponseV2NullMasterCatalogTest() {
    itemVo.setMasterDataItem(null);
    when(this.gdnMapperHelper.mapBean(productVo, ProductInfoResponse.class)).thenReturn(productInfoResponse);
    when(this.gdnMapperHelper.mapBean(itemVo, ItemInfoResponseV2.class)).thenReturn(itemInfoResponseV2);
    productItemsVo.getProductVo().setMasterDataProduct(null);
    ProductAndItemInfoResponseV2 result =
        this.modelConverterImpl.convertToProductAndItemInfoResponseV2(this.productItemsVo);
    verify(this.gdnMapperHelper).mapBean(productVo, ProductInfoResponse.class);
    verify(this.gdnMapperHelper).mapBean(itemVo, ItemInfoResponseV2.class);
    assertNotNull(result);
  }

  @Test
  public void convertToPickupPointDetailResponseListTest() {
    businessPartnerPickupPoint.setFbbActivated(true);
    businessPartnerPickupPoint1.setFbbActivated(true);
    List<PickupPointDetailResponse> response = modelConverterImpl.convertToPickupPointDetailResponseList(
        Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint1), true);
    Assertions.assertEquals(PICKUP_POINT_NAME, response.get(0).getPickupPointName());
    Assertions.assertEquals(PICKUP_POINT_NAME_1, response.get(1).getPickupPointName());
    Assertions.assertEquals(PICKUP_POINT_CODE, response.get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, response.get(1).getPickupPointCode());
  }

  @Test
  public void convertToPickupPointDetailResponseListFalseTest() {
    businessPartnerPickupPoint.setFbbActivated(false);
    businessPartnerPickupPoint1.setFbbActivated(false);
    List<PickupPointDetailResponse> response = modelConverterImpl.convertToPickupPointDetailResponseList(
        Arrays.asList(businessPartnerPickupPoint, businessPartnerPickupPoint1), false);
    Assertions.assertEquals(PICKUP_POINT_NAME, response.get(0).getPickupPointName());
    Assertions.assertEquals(PICKUP_POINT_NAME_1, response.get(1).getPickupPointName());
    Assertions.assertEquals(PICKUP_POINT_CODE, response.get(0).getPickupPointCode());
    Assertions.assertEquals(PICKUP_POINT_CODE_1, response.get(1).getPickupPointCode());
  }

  @Test
  public void covertToItemPickupPointUpdateRequestVoEmptyTest() {
    modelConverterImpl.covertToItemPickupPointUpdateRequestVo(new ItemPickupPointUpdateRequest());
  }

  @Test
  public void covertToItemPickupPointUpdateRequestVoTest() {
    itemPickupPointUpdateRequest.getQuickEditUpdateRequests().get(0).setOff2OnActiveFlag(true);
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        modelConverterImpl.covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    Assertions.assertEquals(ITEM_SKU, itemPickupPointUpdateRequestVo.getAddPickupPointRequests().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0).getItemSku());
  }

  @Test
  public void covertToItemPickupPointUpdateRequestVoNullTest() {
    AddDeleteVariantRequest addDeleteVariantRequest = new AddDeleteVariantRequest();
    AddVariantRequest addVariantRequest = new AddVariantRequest();
    addVariantRequest.setItemSku(ITEM_SKU);
    ProductAttributeDetailDTO productAttributeDetailDTO = new ProductAttributeDetailDTO();
    addVariantRequest.setDefiningAttributes(Collections.singletonList(productAttributeDetailDTO));
    addDeleteVariantRequest.setAddVariantsList(Collections.singletonList(addVariantRequest));
    itemPickupPointUpdateRequest.setAddDeleteVariantRequest(addDeleteVariantRequest);
    BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
    bundleRecipeRequest.setItemSku(ITEM_SKU);
    itemPickupPointUpdateRequest.setBundleRecipesRequests(Collections.singleton(bundleRecipeRequest));
    ItemPickupPointUpdateRequestVo itemPickupPointUpdateRequestVo =
        modelConverterImpl.covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    Assertions.assertEquals(ITEM_SKU, itemPickupPointUpdateRequestVo.getAddPickupPointRequests().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointUpdateRequestVo.getDeletePickupPointRequests().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU, itemPickupPointUpdateRequestVo.getQuickEditUpdateRequests().get(0).getItemSku());
    Assertions.assertEquals(ITEM_SKU,
        itemPickupPointUpdateRequestVo.getBundleRecipeRequests().stream().findFirst().get().getItemSku());
  }

  @Test
  public void toBasicProductAndItemResponse() throws Exception {
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
        .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute().setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute().setVariantCreation(false);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValue));
    BasicProductAndItemResponse basicProductAndItemResponse =
        modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertEquals(preOrder.getIsPreOrder(),
        basicProductAndItemResponse.getProduct().getPreOrder().getIsPreOrder());
    Assertions.assertTrue(basicProductAndItemResponse.getProduct().getDescriptiveAttributes().get(0).isMustShow());
    Assertions.assertEquals(preOrder.getIsPreOrder(), basicProductAndItemResponse.getProduct().getPreOrder().getIsPreOrder());
  }

  @Test
  public void toBasicProductAndItemResponseForSpecailAttributes() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setVariantCreation(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setSkuValue(true);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertEquals(productSpecialAttribute.getAttributeName(),
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes().get(0)
        .getAttributeName());
  }

  @Test
  public void toBasicProductAndItemResponseForSpecailAttributesWithSkuValueFalse() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setVariantCreation(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMarkForDelete(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setSkuValue(false);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0)
      .setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValue));
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertNotEquals(productSpecialAttribute.getAttributeName(),
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes().get(0)
        .getAttributeName());
  }

  @Test
  public void toBasicProductAndItemResponseForSpecailAttributesWithNullAttributes() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    basicProductAndItemDTO.setMasterDataProductAttributes(null);
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertTrue(CollectionUtils.isEmpty(
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes()));
  }


  @Test
  public void toBasicProductAndItemResponseForSpecailAttributesWithCodeMissing() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setVariantCreation(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setSkuValue(false);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE_2);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0)
      .setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValue));
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertFalse(CollectionUtils.isEmpty(
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes()));
  }

  @Test
  public void toBasicProductAndItemResponseForSpecailAttributesWithDefining() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setAttributeType(MasterDataAttributeType.DEFINING_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setVariantCreation(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setSkuValue(true);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertTrue(CollectionUtils.isEmpty(
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes()));
  }


  @Test
  public void toBasicProductAndItemResponseForSpecailAttributesSkuValueFalse() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setVariantCreation(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setSkuValue(false);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0)
      .setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValue));
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertFalse(CollectionUtils.isEmpty(
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes()));
  }

  @Test
  public void toBasicProductAndItemResponseForSpecailAttributesWithDeleted() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "setSpecialAttributesInPDPApi", true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setVariantCreation(false);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setSkuValue(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
      .setMarkForDelete(true);
    ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
    productSpecialAttribute.setAttributeCode(ATTRIBUTE_CODE);
    productSpecialAttribute.setAttributeName(ATTRIBUTE_NAME);
    productSpecialAttribute.setAttributeValue(VALUE);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0)
      .setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValue));
    basicProductAndItemDTO.setProductSpecialAttributesList(
      Collections.singletonList(productSpecialAttribute));
    BasicProductAndItemResponse basicProductAndItemResponse =
      modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertTrue(CollectionUtils.isEmpty(
      basicProductAndItemResponse.getProduct().getDescriptiveAttributes()));
  }



  @Test
  public void toBasicProductAndItemResponse_cncActive() throws Exception {
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute()
        .setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute().setMustShow(true);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0).getMasterDataAttribute().setVariantCreation(false);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    basicProductAndItemDTO.getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(Collections.singletonList(masterDataProductAttributeValue));
    BasicProductAndItemResponse basicProductAndItemResponse =
        modelConverterImpl.toBasicProductAndItemResponse(basicProductAndItemDTO, null);
    Assertions.assertEquals(preOrder.getIsPreOrder(),
        basicProductAndItemResponse.getProduct().getPreOrder().getIsPreOrder());
    Assertions.assertTrue(basicProductAndItemResponse.getProduct().getDescriptiveAttributes().get(0).isMustShow());
    Assertions.assertEquals(preOrder.getIsPreOrder(), basicProductAndItemResponse.getProduct().getPreOrder().getIsPreOrder());
    Assertions.assertEquals(basicProductAndItemResponse.getProduct().getProductCode(),
        basicProductAndItemDTO.getProductCode());
  }

  @Test
  public void convertToProductAndItemsDTO_NullMasterCatalog_success() {
    this.productanditemsvo.getProduct().setMasterCatalog(null);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertNull(result.getProduct().getMasterCatalog());
  }

  @Test
  public void convertToProductAndItemsDTO_NullMasterCatalogNullCategory_success() {
    this.productanditemsvo.getProduct().setMasterCatalog(new MasterCatalog());
    this.productanditemsvo.getProduct().getMasterCatalog().setCategory(null);
    ProductAndItemsResponse result = this.modelConverterImpl.convertToProductAndItemsDTO(this.productanditemsvo);
    verify(gdnMapperHelper).mapBean(productanditemsvo.getProduct(), ProductResponse.class);
    verify(this.productHelperService).getSettlementType(any(Product.class), any(Item.class));
    for (Item item: productanditemsvo.getItems()) {
      verify(gdnMapperHelper).mapBean(item, ItemResponse.class);
      verify(itemService).isPriceEditDisabled(item);
    }
    assertEquals(result.getProduct().getProductSku(), (ModelConverterImplTest.PRODUCT_SKU));
    assertEquals(result.getItems().size(), (2));
    assertNull(result.getProduct().getMasterCatalog());
  }

  @Test
  public void validateAndUpdateViewConfigs_switchOn(){
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch", true);
    itemResponse1.setItemViewConfigs(ALL_VIEW_CONFIGS);
    this.modelConverterImpl.validateAndUpdateViewConfigs(itemResponse1, null);
    Set<ItemViewConfigDTO> filteredConfigs = itemResponse1.getItemViewConfigs();
    assertEquals(1, filteredConfigs.size());
    assertTrue(filteredConfigs.stream().anyMatch(config -> DEFAULT.equals(config.getChannel())));
  }

  @Test
  public void validateAndUpdateViewConfigs_switchOn_nofilter(){
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch", true);
    itemResponse1.setItemViewConfigs(ALL_VIEW_CONFIGS);
    this.modelConverterImpl.validateAndUpdateViewConfigs(itemResponse1, "ALL");
    Set<ItemViewConfigDTO> filteredConfigs = itemResponse1.getItemViewConfigs();
    assertEquals(2, filteredConfigs.size());
    assertTrue(filteredConfigs.stream().anyMatch(config -> CNC.equals(config.getChannel())));
    assertTrue(filteredConfigs.stream().anyMatch(config -> DEFAULT.equals(config.getChannel())));
  }

  private static Set<ItemViewConfigDTO> getAllViewConfigs() {
    return Set.of(constructItemViewConfigDTO(CNC, true, false),constructItemViewConfigDTO(DEFAULT_CHANNEL, true, false));
  }

  private static ItemViewConfigDTO constructItemViewConfigDTO(String channel,  boolean isBuyable,
      boolean isDiscoverable){
    ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
    itemViewConfigDTO.setBuyable(isBuyable);
    itemViewConfigDTO.setDiscoverable(isDiscoverable);
    itemViewConfigDTO.setChannel(channel);
    return itemViewConfigDTO;
  }

  @Test
  public void validateAndUpdateViewConfigs_basicItem_switchOff(){
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch", false);
    basicItemDTO.setItemViewConfigs(ALL_VIEW_CONFIGS);
    this.modelConverterImpl.validateAndUpdateViewConfigs(basicItemDTO, null);
    Set<ItemViewConfigDTO> filteredConfigs = basicItemDTO.getItemViewConfigs();
    assertEquals(1, filteredConfigs.size());
    assertTrue(filteredConfigs.stream().anyMatch(config -> DEFAULT.equals(config.getChannel())));
  }

  @Test
  public void validateAndUpdateViewConfigs_basicItem_switchOn(){
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch", true);
    basicItemDTO.setItemViewConfigs(ALL_VIEW_CONFIGS);
    this.modelConverterImpl.validateAndUpdateViewConfigs(basicItemDTO, null);
    Set<ItemViewConfigDTO> filteredConfigs = basicItemDTO.getItemViewConfigs();
    assertEquals(1, filteredConfigs.size());
    assertTrue(filteredConfigs.stream().anyMatch(config -> DEFAULT.equals(config.getChannel())));
  }

  @Test
  public void validateAndUpdateViewConfigs_basicItem_switchOn_nofilter(){
    ReflectionTestUtils.setField(modelConverterImpl, "cncForWarehouseFeatureSwitch", true);
    basicItemDTO.setItemViewConfigs(ALL_VIEW_CONFIGS);
    this.modelConverterImpl.validateAndUpdateViewConfigs(basicItemDTO, "ALL");
    Set<ItemViewConfigDTO> filteredConfigs = basicItemDTO.getItemViewConfigs();
    assertEquals(2, filteredConfigs.size());
    assertTrue(filteredConfigs.stream().anyMatch(config -> CNC.equals(config.getChannel())));
    assertTrue(filteredConfigs.stream().anyMatch(config -> DEFAULT.equals(config.getChannel())));
  }

  @Test
  public void setSizeAttributeValueTypeNullTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "valueTypeAdditionForDefiningAttributes", true);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(productResponse);
    productResponse.setMasterDataProduct(null);
    modelConverterImpl.setSizeAttributeValueType(productAndItemsResponse);
    Assertions.assertNull(productAndItemsResponse.getProduct().getMasterDataProduct());
  }

  @Test
  public void setSizeAttributeValueTypeTest() {
    ReflectionTestUtils.setField(modelConverterImpl, "valueTypeAdditionForDefiningAttributes", true);
    ProductAndItemsResponse productAndItemsResponse = new ProductAndItemsResponse();
    productAndItemsResponse.setProduct(productResponse);
    MasterDataProductDTO masterDataProductDTO = new MasterDataProductDTO();
    masterDataProductDTO.setMasterDataProductAttributes(new ArrayList<>());
    productResponse.setMasterDataProduct(masterDataProductDTO);
    modelConverterImpl.setSizeAttributeValueType(productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse.getProduct().getMasterDataProduct());
  }

  @Test
  public void updateSizeChartResponseTest() {
    SizeChartResponse sizeChartResponse = new SizeChartResponse();
    SizeChartDataRow sizeChartDataRow = new SizeChartDataRow();
    SizeChartDataColumn sizeChartDataColumn = new SizeChartDataColumn();
    sizeChartDataColumn.setMax(VALUE);
    sizeChartDataColumn.setMin(VALUE);
    sizeChartDataColumn.setValue(VALUE);
    modelConverterImpl.updateSizeChartResponse(sizeChartResponse);
    sizeChartResponse.setSizeChartRows(Collections.singletonList(sizeChartDataRow));
    modelConverterImpl.updateSizeChartResponse(sizeChartResponse);
    sizeChartDataRow.setColumns(Collections.singletonList(sizeChartDataColumn));
    SizeChartResponse response = modelConverterImpl.updateSizeChartResponse(sizeChartResponse);
    Assertions.assertEquals(VALUE, response.getSizeChartRows().get(0).getColumns().get(0).getMax());
    Assertions.assertEquals(VALUE, response.getSizeChartRows().get(0).getColumns().get(0).getValue());
    Assertions.assertEquals(VALUE, response.getSizeChartRows().get(0).getColumns().get(0).getMin());
    sizeChartResponse.getSizeChartRows().get(0).getColumns().get(0).setMin(StringUtils.EMPTY);
    sizeChartResponse.getSizeChartRows().get(0).getColumns().get(0).setMax(StringUtils.EMPTY);
    sizeChartResponse.getSizeChartRows().get(0).getColumns().get(0).setValue(StringUtils.EMPTY);
    response = modelConverterImpl.updateSizeChartResponse(sizeChartResponse);
    Assertions.assertEquals(Constants.ZERO, response.getSizeChartRows().get(0).getColumns().get(0).getMax());
    Assertions.assertEquals(Constants.ZERO, response.getSizeChartRows().get(0).getColumns().get(0).getValue());
    Assertions.assertEquals(Constants.ZERO, response.getSizeChartRows().get(0).getColumns().get(0).getMin());
  }


  @Test
  public void convertToProductAndItemsResponseDuplicateAttributeRemovalTest() throws Exception {
    ReflectionTestUtils.setField(modelConverterImpl, "removeDuplicateProductAttribute", true);
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImageRemoval",true);
    ReflectionTestUtils.setField(modelConverterImpl,"resizeImagePathList","resize/,commerce/");
    MasterDataItemDTO masterDataItemDTO = new MasterDataItemDTO();
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    masterDataItemImageDTO.setLocationPath("resize/");
    MasterDataItemImageDTO masterDataItemImageDTO1 = new MasterDataItemImageDTO();
    masterDataItemImageDTO1.setLocationPath("resizeds/");
    List<MasterDataItemImageDTO> masterDataItemImageDTOList = new ArrayList<>();
    masterDataItemImageDTOList.add(masterDataItemImageDTO);
    masterDataItemImageDTOList.add(masterDataItemImageDTO1);
    masterDataItemDTO.setMasterDataItemAttributeValues(new ArrayList<>());
    masterDataItemDTO.setMasterDataItemImages(masterDataItemImageDTOList);
    itemResponse1.setMasterDataItem(masterDataItemDTO);
    productVo.setCurationStatus(CurationStatus.APPROVED);
    MasterDataProductDTO masterDataProduct = new MasterDataProductDTO();
    List<MasterDataProductImageDTO> masterDataProductImagesList = new ArrayList<>();
    MasterDataProductImageDTO masterDataProductImage = new MasterDataProductImageDTO();
    masterDataProductImage.setLocationPath("resize/");
    masterDataProductImagesList.add(masterDataProductImage);
    MasterDataProductImageDTO masterDataProductImage2 = new MasterDataProductImageDTO();
    masterDataProductImage2.setLocationPath("commerce/");
    masterDataProductImagesList.add(masterDataProductImage2);
    MasterDataProductImageDTO masterDataProductImage3 = new MasterDataProductImageDTO();
    masterDataProductImage3.setLocationPath("remained/");
    masterDataProductImagesList.add(masterDataProductImage3);
    masterDataProduct.setMasterDataProductImages(masterDataProductImagesList);
    masterDataProduct.setMasterDataProductAttributes(new ArrayList<>());

    MasterDataAttributeDTO masterDataAttributeDTO1 = new MasterDataAttributeDTO();
    masterDataAttributeDTO1.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttributeDTO1.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO1 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO1.setMasterDataAttribute(masterDataAttributeDTO1);

    MasterDataAttributeDTO masterDataAttributeDTO2 = new MasterDataAttributeDTO();
    masterDataAttributeDTO2.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttributeDTO2.setAttributeCode(ATTRIBUTE_CODE);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO2 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO2.setMasterDataAttribute(masterDataAttributeDTO2);

    MasterDataAttributeDTO masterDataAttributeDTO3 = new MasterDataAttributeDTO();
    masterDataAttributeDTO3.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttributeDTO3.setVariantCreation(true);
    masterDataAttributeDTO3.setAttributeCode(ATTRIBUTE_CODE_2);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO3 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO3.setMasterDataAttribute(masterDataAttributeDTO3);

    MasterDataAttributeDTO masterDataAttributeDTO4 = new MasterDataAttributeDTO();
    masterDataAttributeDTO4.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    masterDataAttributeDTO4.setAttributeCode(ATTRIBUTE_CODE_3);
    MasterDataProductAttributeDTO masterDataProductAttributeDTO4 = new MasterDataProductAttributeDTO();
    masterDataProductAttributeDTO4.setMasterDataAttribute(masterDataAttributeDTO4);

    masterDataProduct.setMasterDataProductAttributes(
        Arrays.asList(masterDataProductAttributeDTO1, masterDataProductAttributeDTO2, masterDataProductAttributeDTO3,
            masterDataProductAttributeDTO4));

    productItemsVo.getProductVo().getMasterDataProduct().getMasterDataProductAttributes().get(0)
        .setMasterDataProductAttributeValues(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).getMasterDataItem().setMasterDataItemAttributeValues(new ArrayList<>());
    productItemsVo.getProductVo().setSizeChartCode(SIZE_CHART_CODE);
    productResponse.setMasterDataProduct(masterDataProduct);
    productResponse.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(this.masterDataConstructorService.constructItemDimensionFields(itemVo.getMasterDataItem(),
        productVo.getMasterDataProduct())).thenReturn(masterDataItem);
    Mockito.when(this.gdnMapperHelper.mapBean(productVo, ProductResponse.class)).thenReturn(productResponse);
    Mockito.when(this.gdnMapperHelper.mapBean(itemVo, ItemResponse.class)).thenReturn(itemResponse1);
    ProductAndItemsResponse productAndItemsResponse =
        this.modelConverterImpl.convertToProductAndItemsResponse(productItemsVo, null, true);
    Mockito.verify(this.masterDataConstructorService)
        .constructItemDimensionFields(masterDataItem, productVo.getMasterDataProduct());
    Mockito.verify(this.itemService).isPriceEditDisabled(itemVo);
    Mockito.verify(this.productHelperService).getSettlementType(productVo, itemVo);
    Mockito.verify(this.gdnMapperHelper).mapBean(productVo, ProductResponse.class);
    Mockito.verify(this.gdnMapperHelper).mapBean(itemVo, ItemResponse.class);
    Mockito.verify(this.allowedAttributeValuesService).sortDefiningAttribute(productAndItemsResponse.getProduct().getStoreId(), productAndItemsResponse);
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(SIZE_CHART_CODE, productAndItemsResponse.getProduct().getSizeChartCode());
    Assertions.assertNotNull(productAndItemsResponse);
    Assertions.assertEquals(1,productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductImages().size());
    Assertions.assertEquals(3, productAndItemsResponse.getProduct().getMasterDataProduct().getMasterDataProductAttributes().size());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithDescriptiveAttribute() {
    List<MasterDataProductAttribute> attributes = new ArrayList<>();
    MasterDataProductAttribute attribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute.setSkuValue(false);
    masterDataAttribute.setVariantCreation(false);
    attribute.setMasterDataAttribute(masterDataAttribute);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    MasterDataProductAttributeValue masterDataProductAttributeValue1 = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue1.setDescriptiveAttributeValue(VALUE);
    masterDataProductAttributeValue1.setMarkForDelete(true);
    attribute.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValue, masterDataProductAttributeValue1));
    attributes.add(attribute);
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, false);
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithPredefinedAttribute() {
    List<MasterDataProductAttribute> attributes = new ArrayList<>();
    MasterDataProductAttribute attribute = new MasterDataProductAttribute();
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(VALUE);
    masterDataProductAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    MasterDataProductAttributeValue masterDataProductAttributeValue2 = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue2.setMarkForDelete(true);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue2 = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue2.setValue(VALUE);
    masterDataProductAttributeValue2.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
    attribute.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValue, masterDataProductAttributeValue2));
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.PREDEFINED_ATTRIBUTE);
    masterDataAttribute.setSkuValue(false);
    attribute.setMasterDataAttribute(masterDataAttribute);
    attributes.add(attribute);
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, false);
    masterDataAttribute.setSkuValue(true);
    modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, false);
    assertNotNull(result);
    assertEquals(1, result.size());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithMultiValueAttributes_WhenEnabled() {
    ReflectionTestUtils.setField(modelConverterImpl, "productSuitabilityFeatureEnabled", true);
    List<MasterDataProductAttribute> attributes = new ArrayList<>();

    MasterDataProductAttribute descriptiveMultiValue = new MasterDataProductAttribute();
    MasterDataAttribute descriptiveAttribute = new MasterDataAttribute();
    descriptiveAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_MULTIVALUE);
    descriptiveAttribute.setSkuValue(false);
    descriptiveMultiValue.setMasterDataAttribute(descriptiveAttribute);
    MasterDataProductAttributeValue masterDataProductAttributeValue = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue.setDescriptiveAttributeValue(VALUE);
    MasterDataProductAttributeValue masterDataProductAttributeValue1 = new MasterDataProductAttributeValue();
    masterDataProductAttributeValue1.setDescriptiveAttributeValue(VALUE_FORMAT1);
    descriptiveMultiValue.setMasterDataProductAttributeValues(
        Arrays.asList(masterDataProductAttributeValue, masterDataProductAttributeValue1));
    attributes.add(descriptiveMultiValue);

    MasterDataProductAttribute predefinedMultiValue = new MasterDataProductAttribute();
    MasterDataAttribute predefinedAttribute = new MasterDataAttribute();
    predefinedAttribute.setAttributeType(MasterDataAttributeType.PREDEFINED_MULTIVALUE);
    predefinedAttribute.setSkuValue(false);
    predefinedMultiValue.setMasterDataAttribute(predefinedAttribute);
    attributes.add(predefinedMultiValue);

    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, true);
    descriptiveAttribute.setSkuValue(true);
    modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, true);
    assertNotNull(result);
    assertEquals(2, result.size());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithMultiValueAttributes_WhenDisabled() {
    ReflectionTestUtils.setField(modelConverterImpl, "productSuitabilityFeatureEnabled", false);
    List<MasterDataProductAttribute> attributes = new ArrayList<>();

    MasterDataProductAttribute descriptiveMultiValue = new MasterDataProductAttribute();
    MasterDataAttribute descriptiveAttribute = new MasterDataAttribute();
    descriptiveAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_MULTIVALUE);
    descriptiveAttribute.setSkuValue(false);
    descriptiveMultiValue.setMasterDataAttribute(descriptiveAttribute);
    attributes.add(descriptiveMultiValue);

    MasterDataProductAttribute predefinedMultiValue = new MasterDataProductAttribute();
    MasterDataAttribute predefinedAttribute = new MasterDataAttribute();
    predefinedAttribute.setAttributeType(MasterDataAttributeType.PREDEFINED_MULTIVALUE);
    predefinedAttribute.setSkuValue(false);
    predefinedMultiValue.setMasterDataAttribute(predefinedAttribute);
    attributes.add(predefinedMultiValue);

    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, true);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithSkuValueAttribute() {
    List<MasterDataProductAttribute> attributes = new ArrayList<>();
    MasterDataProductAttribute attribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute.setSkuValue(true);
    attribute.setMasterDataAttribute(masterDataAttribute);
    attributes.add(attribute);
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, false);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithVariantCreationAttribute() {
    List<MasterDataProductAttribute> attributes = new ArrayList<>();
    MasterDataProductAttribute attribute = new MasterDataProductAttribute();
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataAttribute.setAttributeType(MasterDataAttributeType.DESCRIPTIVE_ATTRIBUTE);
    masterDataAttribute.setVariantCreation(true);
    attribute.setMasterDataAttribute(masterDataAttribute);
    attributes.add(attribute);
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, false);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithNullAttribute() {
    List<MasterDataProductAttribute> attributes = new ArrayList<>();
    MasterDataProductAttribute attribute = new MasterDataProductAttribute();
    attribute.setMasterDataAttribute(null);
    attributes.add(attribute);
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(attributes, false);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithNullAttributes() {
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(null, false);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

  @Test
  public void fetchDescriptiveAttributeFromMasterDataProductTest_WithEmptyAttributes() {
    ReflectionTestUtils.setField(modelConverterImpl, "productSuitabilityFeatureEnabled", true);
    MasterDataProductAttribute masterDataProductAttribute = new MasterDataProductAttribute();
    List<ProductAttributeDetail> result =
        modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(
            Collections.singletonList(masterDataProductAttribute), false);
    modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(
            Collections.singletonList(masterDataProductAttribute), true);
    MasterDataAttribute masterDataAttribute = new MasterDataAttribute();
    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(
        Collections.singletonList(masterDataProductAttribute), true);
    masterDataAttribute.setAttributeType(MasterDataAttributeType.PREDEFINED_MULTIVALUE);
    masterDataAttribute.setSkuValue(true);
    masterDataProductAttribute.setMasterDataAttribute(masterDataAttribute);
    modelConverterImpl.fetchDescriptiveAttributeFromMasterDataProduct(
        Collections.singletonList(masterDataProductAttribute), true);
    assertNotNull(result);
    assertTrue(result.isEmpty());
  }

}

