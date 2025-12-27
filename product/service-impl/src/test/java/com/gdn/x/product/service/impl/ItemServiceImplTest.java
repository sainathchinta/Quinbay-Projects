package com.gdn.x.product.service.impl;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anySet;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.dozer.DozerBeanMapper;
import org.hamcrest.MatcherAssert;
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
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.beans.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.SliceImpl;
import org.springframework.data.domain.Sort;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.util.CollectionUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.base.mapper.impl.DozerMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.partners.product.pricing.model.enums.PromoBundlingEventType;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedDeactivatedEventModel;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.PristineCategory;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterCatalog;
import com.gdn.x.product.model.entity.MasterDataAttribute;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataItemAttributeValue;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.OfflineItem;
import com.gdn.x.product.model.entity.PickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PriceHistory;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.ActiveComboRequestVO;
import com.gdn.x.product.model.vo.ComboItemVO;
import com.gdn.x.product.model.vo.ComboRuleVO;
import com.gdn.x.product.model.vo.ComboVO;
import com.gdn.x.product.model.vo.DefaultItemSkuVO;
import com.gdn.x.product.model.vo.ItemAndBundlingInfoVO;
import com.gdn.x.product.model.vo.ItemAndItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemCategoryVO;
import com.gdn.x.product.model.vo.ItemInfoVO;
import com.gdn.x.product.model.vo.ItemPickupPointVo;
import com.gdn.x.product.model.vo.ItemPriceVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductVo;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingVO;
import com.gdn.x.product.outbound.api.XCampaignOutbound;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateItemRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductTypeEditRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.api.BusinessPartnerPickupPointService;
import com.gdn.x.product.service.api.BusinessPartnerPromoService;
import com.gdn.x.product.service.api.BusinessPartnerService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheEvictItemService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.CachedService;
import com.gdn.x.product.service.api.CatalogService;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.DataSourceWrapperService;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemHelperService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointSummaryService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.OfflineItemService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductL3SolrReindexStatusService;
import com.gdn.x.product.service.api.ProductL3SolrService;
import com.gdn.x.product.service.api.ProductSearchService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.PromoBundlingService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.util.FormulaUtil;
import com.gdn.x.product.service.util.ItemSummaryUtil;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import com.gdn.x.promotion.rest.web.model.dto.request.SimpleSetStringRequest;
import com.gdn.x.promotion.rest.web.model.promo.bundling.ComboRule;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Sets;

public class ItemServiceImplTest {

  private static final double ITEM_WIDTH = 5.0;

  private static final double ITEM_UPDATED_WIDTH = 4.0;

  private static final double ITEM_WEIGHT = 1.0;

  private static final double ITEM_UPDATED_WEIGHT = 2.0;

  private static final double ITEM_LENGTH = 2.0;

  private static final double ITEM_UPDATED_LENGTH = 3.0;

  private static final double ITEM_DELIVERY_WEIGHT = 3.0;

  private static final String TICKET_TEMPLATE_ID = "ticket-template-id";

  private static final boolean NEED_MASTER_DATA_DETAIL = true;

  private static final String STORE_ID = "10001";

  private static final String REQUEST_ID = "request-id";

  private static final String PICKUP_POINT = "pickup-point";

  private static final String ITEM_SKU = "item-sku";

  private static final String ITEM_SKU_1 = "item-0001-0001-00001";

  private static final String OFFLINE_ITEM_ID = "offline-item-id";

  private static final String ITEM_SKU2 = "item-sku-2";

  private static final String ITEM_SKU3 = "item-sku-3";

  private static final List<String> ITEM_SKUS = Arrays.asList(ITEM_SKU);

  private static final String BLANK = "";

  private static final Item ITEM1 = initializeItem(ITEM_SKU);

  private static final Item ITEM2 = initializeItem(ITEM_SKU2);

  private static final Item ITEM3 = initializeItem(ITEM_SKU3);

  private static final Stream<Item> ITEM_STREAM = Stream.of(ITEM1, ITEM2, ITEM3);

  private static final Stream<Item> ITEM_STREAM_1 = Stream.of(ITEM1, ITEM2, ITEM3);

  private static final String ITEM_CODE = "item-code";

  private static final boolean IS_SYNCHRONIZED = true;

  private static final String VALUE = "value";

  private static final String CHANNEL_DEFAULT = ChannelName.DEFAULT.toString();

  private static final String CHANNEL_WEB = ChannelName.DESKTOP_WEB.toString();

  private static final String ITEM_SKU_NOT_FOUND = "item-sku-not-found";

  private static final boolean IS_NOT_SYNCHRONIZED = false;

  private static final String PRODUCT_SKU_NOT_FOUND = "product-sku-not-found";

  private static final String ITEM_SKU_SYNC_TOBE_UPDATED = "item-sku-sync";

  private static final String ITEM_SKU_UNSYNC_TOBE_UPDATED = "item-sku-unsync";

  private static final String STORE_ID_NOT_FOUND = "store-id-not-found";

  private static final double HEIGHT_UPDATED = 5.0;

  private static final double HEIGHT = 4.0;

  private static final String USERNAME = "username";

  private static final String LEVEL2_MERCHANT_CODE = "level2-merchant-code";

  private static final int STOCK = 2;

  private static final int ORIGINAL_STOCK = 4;

  private static final String MERCHANT_SKU = "merchant-sku";

  private static final int SIZE_OF_PRODUCT_ATTRIBUTES = 1;

  private static final String PRODUCT_SKU_FOR_ADD_ITEMS = "product-sku-for-add-items";

  private static final int SIZE_OF_PRODUCT_ATTRIBUTES_2 = 2;

  private static final String ITEM_SKU_WITH_EMPTY_PRICE = "item-sku-with-empty-price";

  private static final String MERCHANT_CODE = "level2-merchant-code";

  private static final String ITEM_CODE_NOT_ADD = "test1235";

  private static final String PRODUCT_CODE = "PRODUCT-012345";

  private static final String PRODUCT_SKU = "PRODUCT-SKU-CODE";

  private static final boolean DO_ARCHIVE_TRUE = true;

  private static final boolean DO_ARCHIVE_FALSE = false;

  private static final String PRISTINE_ID = "PRISTINE_ID";

  private static final String PRISTINE_PRODUCT_NAME = "pristineProductName";

  private static final String CATEGORY_ID = "53187";

  private static final String CATEGORY_ID_TWO = "53188";

  private static final String PRODUCT_CATEGORY_CODE = "53187";

  private static final String PRODUCT_CATEGORY_CODE_TWO = "53188";

  private static final String CATEGORY_CODE_FOR_UNSYNC_CHECK = "CAT-01";

  private static final String CATEGORY_NAME = "iphone";

  private static final String CATALOG_ID = "12051";

  private static final int LEVEL = 2;

  private static final String CLIENT_ID = "clientId";

  private static final String ATTRIBUTE_MAP = "{\"HANDPHONE\":\"COLOR,ROM\", " + "\"COMPUTER\":\"color,screenSize,processorName,ram,ssd\"}";

  private static final String NEW_MERCHANT_SKU = "new-merchant-sku";

  private static final String PROMO_BUNDLING_NAME = "promoBundlingTest";

  private static final String PROMO_BUNDLING_ID = "promoBundlingId";

  private static final String PROMO_BUNDLING_TYPE = "COMBO";

  private static final String ATTRIBUTE_CODE = "attributeCode";

  private static final String ATTRIBUTE_NAME = "attributeName";

  private static final String ATTRIBUTE_VALUE = "attributeValue";

  private static final String PRODUCT_NAME = "productName";

  private static final Double DISCOUNT_PERCENTAGE = 10.0;

  private static final int QUANTITY = 10;

  private static final String CURRENCY = "currency";

  private static final String PRISTINE_MASTER_ID = "pristineMasterid";

  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PICKUP_POINT_CODE_1 = "pickup-point-code-1";
  private static final String PICKUP_POINT_CODE_2 = "pickup-point-code-2";

  private static final String UPDATED_BY = "updatedBy";

  private static final String MAIN_ITEM_SKU = "mainItemSku";
  private static final String SOURCE_ITEM_CODE = "sourceItemCode";

  private static final String ITEM_NOT_FOUND = "Item not found ";
  private static final int DG_LEVEL = 1;
  private static final String MERCHANT_TYPE = "CM";
  private static final int PAGE = 0;
  private static final int PAGE_SIZE = 10;

  private static final String BP_CODE = "BP-code";
  private static final String PP_CODE = "PP-code";
  private static final String ADJUSTMENT_NAME = "adjustmentName";
  private static final String DESCRIPTION = "description";
  private static final String CAMPAIGN_CODE = "CAMPAIGN_CODE";
  private static final Date START_DATE = new Date();
  private static final Date END_DATE = new Date();
  private static final long DISCOUNT_PRICE = 7000l;
  private static final boolean IS_ACTIVATED = Boolean.TRUE;
  private static final String B2C_SELLER_CHANNEL = "BLIBLI";
  private static final String B2B_SELLER_CHANNEL = "BLIBLI FOR BUSINESS";
  private static final String MASTER_SKU = "masterSku";
  private static final String UPDATED_FIELD_1 = "uf1";
  private static final String UPDATED_FIELD_2 = "uf2";
  private static final String UPDATED_FIELD_3 = "uf3";
  private static final String CNC = "CNC";
  private static final String FETCH_VIEW_CONFIGS_BY_CHANNEL = "DEFAULT, CNC";

  @InjectMocks
  private ItemServiceImpl itemServiceImpl;

  @Mock
  private FormulaUtil formulaUtil;

  @Mock
  private ItemSummaryUtil itemSummaryUtil;

  @Mock
  private ObjectConverterService objectConverterService;

  @Mock
  private ProductService productService;

  @Mock
  private ItemCacheableService itemCacheableService;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ProductCacheableService productCacheableService;

  @Mock
  private CacheEvictHelperService cacheEvictHelperService;

  @Mock
  private ItemRepository itemRepository;

  @Mock
  private ProductHelperService productHelperService;

  @Mock
  private SaveOperationService saveOperationService;

  @Mock
  private ChannelService channelService;

  @Mock
  private ItemPriceService itemPriceService;

  @Mock
  private SkuValidator skuValidator;

  @Mock
  private ProductAndItemSolrRepository productAndItemSolrRepository;

  @Mock
  private ExecutorService executorService;

  @Mock
  private SaveAndPublishService saveAndPublishService;

  @Mock
  private CatalogService catalogService;

  @Mock
  private PristineItemRepository pristineItemRepository;

  @Mock
  private ProductSearchService productSearchService;

  @Mock
  private CacheEvictItemService cacheEvictItemService;

  @Mock
  private OfflineItemService offlineItemService;

  @Mock
  private PromoBundlingService promoBundlingService;

  @Mock
  private ItemHelperService itemHelperService;

  @Mock
  private SystemParameterService systemParameterService;

  @Mock
  private ItemViewConfigService itemViewConfigService;

  @Mock
  private MasterDataCacheService masterDataCacheService;

  @Mock
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Mock
  private ProductL3SolrService productL3SolrService;

  @Mock
  private CacheItemHelperService cacheItemHelperService;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Mock
  private MasterDataService masterDataService;

  @Mock
  private XCampaignOutbound xCampaignOutbound;

  @Mock
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Mock
  private ItemPickupPointRepository itemPickupPointRepository;

  @Mock
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Mock
  private DataSourceWrapperService dataSourceWrapperService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Mock
  private CachedService cachedService;

  @Mock
  private ItemPickupPointSummaryService itemPickupPointSummaryService;

  @Captor
  private ArgumentCaptor<Set<String>> itemSkuCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> itemListCaptor;

  @Captor
  private ArgumentCaptor<Item> itemArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductAndItemsVO> productAndItemsVOArgumentCaptor;

  @Captor
  private ArgumentCaptor<ItemVo> itemVoArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<ItemVo>> itemVoListArgumentCaptor;

  @Captor
  private ArgumentCaptor<Product> productArgumentCaptor;

  @Captor
  private ArgumentCaptor<ItemPickupPoint> itemPickupPointArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<Item>> listArgumentCaptor;

  @Captor
  private ArgumentCaptor<Stream<Item>> streamArgumentCaptor;

  @Captor
  private ArgumentCaptor<ItemViewConfig> itemViewConfigArgumentCaptor;

  private GdnMapper gdnMapper = new DozerMapper(new DozerBeanMapper());

  private Item item;
  private Item item1 = new Item();
  private ItemPickupPoint itemPickupPoint;
  private ItemPickupPoint newItemPickupPoint;
  private NeedCorrectionItemActivationRequest itemActivationRequest;
  Map<String, String> productSkuAndMerchantCodeMap = new HashMap<>();

  private Price price;
  private Price price1;

  private MasterDataItem masterDataItem;

  private MasterDataItemAttributeValue itemAttributeValue;

  private ArrayList<MasterDataItemAttributeValue> itemAttributeValues;

  private HashSet<Price> prices;

  private Price price2;

  private PriceHistory priceHistory;

  private MasterDataAttribute masterDataAttribute;

  private Set<String> itemSkus;

  private List<String> itemSkusList;

  private List<Item> listOfItems;

  private List<ItemPickupPoint> itemPickupPointList;

  private Page<Item> pageOfItems;

  private Item itemWithMasterDataItem;

  private Product product;

  private MasterDataItem masterDataItemUpdated;

  private Item itemUnsyncUpdated;

  private MasterDataItem masterDataItemToBeUpdated;

  private Item itemUnSyncTobeUpdated;

  private Item itemUpdated;

  private Item itemSyncTobeUpdated;

  private ArrayList<String> level2Ids;

  private Item itemRequestVO;

  private Set<ItemViewConfig> setItemViewConfigDefault;

  private Item itemWithGeneratedItemSku;

  private List<Item> listOfItemRequestVO;

  private ProductAttribute productAttribute;

  private Product productWithProductAttribute;

  private List<Item> listOfItemRequestVOForAddItems;

  private Item itemRequestVOForAddItems_1;

  private HashSet<ItemViewConfig> itemViewConfigSetForAddItems;

  private Product productForAddItems;

  private Item itemRequestVOForAddItems_2;

  private Item convertToItemForAddItems_1;

  private Item convertToItemForAddItems_2;

  private ArrayList<Item> convertTolistOfItemsForAddItems;

  private Item convertToItemForAddItems_1_WithMasterDataItem;

  private Item convertToItemForAddItems_2_WithMasterDataItem;

  private ArrayList<Item> convertTolistOfItemsForAddItems_WithMasterDataItem;

  private Product productForAddItems_WithProductAttribute_1;

  private ProductAttribute productAttributeForAddItems;

  private ProductAttribute productAttributeForAddItems_2;

  private Product productForAddItems_WithProductAttribute_2;

  private Product productForAddItems_WithProductAttributes;

  private ProductAttribute productAttributeForAddItems_1;

  private ArrayList<ProductAttribute> productAttributes;

  private Map<String, Item> mapOfItems;

  private Item itemWithEmptyPrice;

  private ProductDetailResponse productDetailResponse;

  private Map<String, MasterDataItem> mapOfMasterDataItems;

  private ProductAndItemSolr productAndItemSolr;

  private Set<String> pristineMasterIds;
  private List<Item> currentItems = new ArrayList<>();

  private List<ItemCategoryVO> itemCategories;

  private List<String> itemSkuList;

  private List<OfflineItem> offlineItemList;

  private OfflineItem offlineItem;

  private ActiveComboRequestVO activeComboRequestVO;

  private ComboVO comboVO;

  private List<ComboVO> comboVOList;

  private Set<String> itemCodes, pristineIdSet;

  private SimpleSetStringRequest simpleSetStringRequest;

  private MandatoryRequestParam mandatoryRequestParam;

  private PromoBundlingDetailResponse promoBundlingDetailResponse;

  private List<PromoBundlingDetailResponse> promoBundlingDetailResponses;

  private ComboRule comboRule, comboRule2;

  private List<ComboRule> comboRuleList;

  private PristineDataItem pristineDataItem;

  private ComboItemVO comboItemVO;

  private List<ComboItemVO> comboItemVOList;

  private MasterDataItemAttributeValue masterDataItemAttributeValue;

  private List<MasterDataItemAttributeValue> masterDataItemAttributeValueList;

  private MasterDataProduct masterDataProduct = new MasterDataProduct();

  private DiscountPrice discountPrice;

  private List<DiscountPrice> discountPrices;

  private List<ItemPriceVO> itemPriceVOS;

  private ProductDomainEventModel productDomainEventModel;

  private SystemParameter systemParameter = new SystemParameter();

  private MasterCatalog masterCatalog = new MasterCatalog();

  private Category category = new Category();

  private ItemViewConfig itemViewConfig;

  private ItemViewConfig cncItemViewConfig;

  private Set<ItemViewConfig> itemViewConfigs;

  private ItemViewConfig itemViewConfigWithBuyableFalse;

  private Set<ItemViewConfig> itemViewConfigsWithBuyableFalse;

  private MasterDataProductAndItemsVO masterDataProductAndItemsVO;
  private Map<String, Boolean> itemsChanged = new HashMap<>();
  private PickupPointUpdateRequest pickupPointUpdateRequest;

  private PickupPoint pickupPoint;
  private PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel;

  private List<ItemPickupPoint> itemPickupPoints;

  private List<ItemPickupPoint> itemPickupPointNeedrevisonList;
  private List<NeedCorrectionItemActivationRequest> itemActivationRequests;
  private ProfileResponse profileResponse = new ProfileResponse();
  private ItemViewConfig b2bItemViewConfig = new ItemViewConfig();

  private Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
  private BusinessPartner businessPartner;
  private EditProductDetailDTO editProductDetailDTO;
  private Map<String, Item> itemMap;
  private MasterSkuMappingEventModel masterSkuMapping;
  private BusinessPartnerPickupPoint businessPartnerPickupPoint;

  @Test
  public void addItemTest() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, ITEM_CODE, pageable))
        .thenReturn(pageOfItems);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU)).thenReturn(this.productWithProductAttribute);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(Item.class))).thenReturn(itemWithGeneratedItemSku);
    boolean result =
        this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO);
    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, ITEM_CODE, pageable);
    Mockito.verify(this.productHelperService).containDefaultChannelPrice(this.itemRequestVO);
    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(this.productHelperService)
        .setItemDetail(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.MERCHANT_CODE,
            ItemServiceImplTest.SIZE_OF_PRODUCT_ATTRIBUTES, this.itemRequestVO);
    Mockito.verify(this.productHelperService)
        .setMasterDataItemFromMasterData(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, this.itemWithGeneratedItemSku);
    Mockito.verify(this.itemPriceService)
        .publishItemPriceChangeEvent(ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID,
            this.productWithProductAttribute, this.itemWithGeneratedItemSku);
    Mockito.verify(this.productHelperService).addItemAttributeToProductAttribute(this.productWithProductAttribute, this.itemWithGeneratedItemSku.getItemSku(),
        this.itemWithGeneratedItemSku.getMasterDataItem().getMasterDataItemAttributeValues());
    Mockito.verify(this.saveOperationService).saveProductAndItems(productAndItemsVOArgumentCaptor.capture(),
        eq(new ArrayList<>()));
    Mockito.verify(this.saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    assertTrue(result);
    assertNotNull(productAndItemsVOArgumentCaptor);
    ProductAndItemsVO productAndItemsVO = productAndItemsVOArgumentCaptor.getValue();
    assertEquals(this.productWithProductAttribute, productAndItemsVO.getProduct());
    assertEquals(this.itemWithGeneratedItemSku, productAndItemsVO.getItems().get(0));
    assertEquals(ITEM_SKU, productAndItemsVO.getItems().get(0).getItemSku());
    assertEquals("ID", productAndItemsVO.getItems().get(0).getPristineDataItem().getId());

  }

  @Test
  public void addItemTestWithContainDefaultChannelPriceFalse() throws Exception {
    this.price.setChannel(ItemServiceImplTest.CHANNEL_WEB);
    this.prices.add(this.price);
    this.itemRequestVO.setPrice(this.prices);
    Mockito.when(this.productHelperService.containDefaultChannelPrice(this.itemRequestVO)).thenReturn(false);

    try {
      this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
          ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO);
    } catch (Exception e) {
      Mockito.verify(this.productHelperService).containDefaultChannelPrice(this.itemRequestVO);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemTestWithItemCodeBlank() throws Exception {
    this.itemRequestVO.setItemCode(null);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO));
  }

  @Test
  public void addItemTestWithNullItemRequestVO() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, null));
  }

  @Test
  public void addItemTestWithNullProductSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, null, this.itemRequestVO));
  }

  @Test
  public void addItemTestWithProductNotFound() throws Exception {
    try {
      this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
          ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND, this.itemRequestVO);
    } catch (Exception e) {
      Mockito.verify(this.productHelperService).containDefaultChannelPrice(this.itemRequestVO);
      Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void addItemTestWithRequestIdBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, null, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO));
  }

  @Test
  public void addItemTestWithStoreIdBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItem(null, ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO));
  }

  @Test
  public void UnarchiveItemNoL5Test() throws Exception {
    item.setArchived(true);
    EditItemResponse editItemResponse = new EditItemResponse();
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    when(
        this.cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(this.itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        itemPickupPoints);
    editItemResponse = this.itemServiceImpl.toggleArchiveItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.USERNAME, DO_ARCHIVE_FALSE);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    assertEquals(editItemResponse.getApiErrorCode(), ApiErrorCode.L5_NOT_PRESENT);
  }

  @Test
  public void archiveItemSameTest() throws Exception {
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setArchived(false);
    item.setCncActivated(false);
    product.setArchived(true);
    this.itemPickupPoints = new ArrayList<>();
    EditItemResponse editItemResponse = new EditItemResponse();
    when(
        this.cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(this.itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    when(this.saveOperationService.saveItemsWithoutUpdatingSolr(
        anyList())).thenReturn(Collections.singletonList(item));
    when(productService.getProduct(anyString(), anyString())).thenReturn(product);
    when(this.saveOperationService.saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP)).thenReturn(product);
    editItemResponse = this.itemServiceImpl.toggleArchiveItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.DO_ARCHIVE_FALSE);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    assertEquals(ApiErrorCode.TOGGLE_ARCHIVE_FAILED_FOR_SAME_FLAG,
        editItemResponse.getApiErrorCode());
  }

  @Test
  public void archiveItemWithAlreadyArchivedTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    item.setArchived(true);
    boolean result = false;
    when(
        this.cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(this.itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Arrays.asList(itemPickupPoint));
    editItemResponse = this.itemServiceImpl.toggleArchiveItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.USERNAME, DO_ARCHIVE_TRUE);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    assertFalse(result);
    assertEquals(ApiErrorCode.TOGGLE_ARCHIVE_FAILED_FOR_SAME_FLAG,
        editItemResponse.getApiErrorCode());
  }

  @Test
  public void archiveItemTest() throws Exception {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    EditItemResponse editItemResponse = new EditItemResponse();
    itemViewConfigs.add(new ItemViewConfig());
    item.setItemViewConfigs(itemViewConfigs);
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    this.itemPickupPoints = new ArrayList<>();
    when(
        this.cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(this.itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    doNothing().when(productAndItemSolrIndexerService)
        .updateSolrOnToggleArchiveItemAction(any(Item.class));
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    editItemResponse = this.itemServiceImpl.toggleArchiveItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.DO_ARCHIVE_TRUE);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).getProduct(anyString(), anyString());
    verify(productAndItemSolrIndexerService).updateProductDetailsInSolr(Arrays.asList(product));
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,true, item, Arrays.asList(itemPickupPoint), false);
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(any(Product.class), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Assertions.assertNull(editItemResponse.getApiErrorCode());
  }

  @Test
  public void archiveItem_mppTurnONTest() throws Exception {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    EditItemResponse editItemResponse = new EditItemResponse();
    itemViewConfigs.add(new ItemViewConfig());
    item.setItemViewConfigs(itemViewConfigs);
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    doNothing().when(productAndItemSolrIndexerService)
        .updateSolrOnToggleArchiveItemAction(any(Item.class));
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    when(
        this.cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    when(this.itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Arrays.asList(itemPickupPoint));
    editItemResponse = this.itemServiceImpl.toggleArchiveItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.DO_ARCHIVE_TRUE);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    Mockito.verify(productService).getProduct(anyString(), anyString());
    verify(productAndItemSolrIndexerService).updateProductDetailsInSolr(Arrays.asList(product));
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,true, item, Arrays.asList(itemPickupPoint), false);
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(any(Product.class), anyList(),
        eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Assertions.assertNull(editItemResponse.getApiErrorCode());
  }

  @Test
  public void assignTicketTemplateToItemsEmptySkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.assignTicketTemplateToItems(ItemServiceImplTest.STORE_ID, this.itemSkusList,
        ItemServiceImplTest.TICKET_TEMPLATE_ID));
  }

  @Test
  public void assignTicketTemplateToItemsNullStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.assignTicketTemplateToItems(null, this.itemSkusList, ItemServiceImplTest.TICKET_TEMPLATE_ID));
  }

  @Test
  public void assignTicketTemplateToItemsTest() {
    this.itemSkusList.add(ItemServiceImplTest.ITEM_SKU);
    List<Item> assignTicketTemplateToItems =
        this.itemServiceImpl.assignTicketTemplateToItems(ItemServiceImplTest.STORE_ID, this.itemSkusList,
            ItemServiceImplTest.TICKET_TEMPLATE_ID);
    assertEquals(this.listOfItemRequestVO, assignTicketTemplateToItems);
    Mockito.verify(this.itemRepository)
        .assignTicketTemplate(ItemServiceImplTest.STORE_ID, this.itemSkusList, ItemServiceImplTest.TICKET_TEMPLATE_ID);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, assignTicketTemplateToItems.get(0));
  }

  @Test
  public void deleteItemTest() throws Exception {
    boolean result = this.itemServiceImpl.deleteItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, false);
    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, this.item.getProductSku());
    Mockito.verify(this.productHelperService).deleteItemAttributeFromProductAttribute(this.product, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(this.saveOperationService)
        .saveProductAndItems(new ProductAndItemsVO(this.product, Arrays.asList(this.item)), new ArrayList<>());
    assertTrue(result);
  }

  @Test
  public void deleteItemTestWithItemNotFound() throws Exception {
    try {
      this.itemServiceImpl.deleteItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU_NOT_FOUND);
    } catch (Exception e) {
      Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
          ItemServiceImplTest.ITEM_SKU_NOT_FOUND, false);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void deleteItemTestWithItemSkuBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.deleteItem(ItemServiceImplTest.STORE_ID, null));
  }

  @Test
  public void deleteItemTestWithProductNotFound() throws Exception {
    this.item.setProductSku(ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND);
    try {
      this.itemServiceImpl.deleteItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    } catch (Exception e) {
      Mockito.verify(this.itemRepository)
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, false);
      Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND);
      assertTrue(e instanceof ApplicationRuntimeException);
    }
  }

  @Test
  public void deleteItemTestWithProductWithNoDefiningAttribute() throws Exception {
    boolean result = this.itemServiceImpl.deleteItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, false);
    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, this.item.getProductSku());
    Mockito.verify(this.productHelperService).deleteItemAttributeFromProductAttribute(this.product, ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(this.saveOperationService)
        .saveProductAndItems(new ProductAndItemsVO(this.product, Arrays.asList(this.item)), new ArrayList<>());
    assertTrue(result);
  }

  @Test
  public void deleteItemTestWithStoreIdBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.deleteItem(null, ItemServiceImplTest.ITEM_SKU));
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalseBlankStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findByStoreIdAndItemSkuAndMarkForDeleteFalse(null, ItemServiceImplTest.ITEM_SKU));
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalseNotItemSkuTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.STORE_ID));
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalseTest() {
    Item result = this.itemServiceImpl.findByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(this.skuValidator).isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, false);
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemCodesByItemSkuIn() {
    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Mockito.when(this.itemRepository.findItemCodesByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus)).thenReturn(Collections.singletonList(item));
    Map<String, String> results = this.itemServiceImpl.getItemCodesByItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    Mockito.verify(this.itemRepository).findItemCodesByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    assertNotNull(results);
    MatcherAssert.assertThat(results.get(ITEM_SKU), equalTo(ITEM_CODE));
  }

  @Test
  public void getItemCodesByItemSkuIn_nullItemCode() {
    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    List<Item> items = Arrays.asList(item, new Item());
    Mockito.when(this.itemRepository.findItemCodesByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus)).thenReturn(items);
    Map<String, String> results = this.itemServiceImpl.getItemCodesByItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    Mockito.verify(this.itemRepository).findItemCodesByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    assertNotNull(results);
    assertEquals(1, results.size());
    assertThat(results.get(ITEM_SKU), equalTo(ITEM_CODE));
  }

  @Test
  public void getItemCodesByItemSkuIn_nullStoreId() {
    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemCodesByItemSkuIn(null, itemSkus));
  }

  @Test
  public void getItemCodesByItemSkuIn_emptyItemSkus() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemCodesByItemSkuIn(STORE_ID, new HashSet<>()));
  }

  @Test
  public void getItemsByItemSkuIn() {
    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Mockito.when(this.itemRepository.findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID,
        itemSkus)).thenReturn(Collections.singletonList(item));
    Map<String, Item> results = this.itemServiceImpl.getItemsByItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    Mockito.verify(this.itemRepository).findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    assertNotNull(results);
    MatcherAssert.assertThat(results.get(ITEM_SKU), equalTo(item));
  }

  @Test
  public void getItemsByItemSkuIn_nullItemCode() {
    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    List<Item> items = Arrays.asList(item, new Item());
    Mockito.when(this.itemRepository.findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID,
        itemSkus)).thenReturn(items);
    Map<String, Item> results = this.itemServiceImpl.getItemsByItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    Mockito.verify(this.itemRepository).findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(ItemServiceImplTest.STORE_ID, itemSkus);
    assertNotNull(results);
    assertEquals(2, results.size());
    assertThat(results.get(ITEM_SKU), equalTo(item));
  }

  @Test
  public void getItemsByItemSkuIn_nullStoreId() {
    Set<String> itemSkus = Collections.singleton(ITEM_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByItemSkuIn(null, itemSkus));
  }

  @Test
  public void getItemsByItemSkuIn_emptyItemSkus() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByItemSkuIn(STORE_ID, new HashSet<>()));
  }

  @Test
  public void getItemAvailabilityTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    List<Item> response = new ArrayList<>();
    Item item = new Item();
    item.setProductSku(PRODUCT_SKU);
    response.add(item);

    Mockito.when(itemRepository.getItemAvailability(STORE_ID, productSkus)).thenReturn(response);
    Map<String, List<Item>> itemAvailability = itemServiceImpl.getItemAvailability(STORE_ID, productSkus);
    assertEquals(item, itemAvailability.get(PRODUCT_SKU).get(0));
    Mockito.verify(itemRepository).getItemAvailability(STORE_ID, productSkus);

  }

  @Test
  public void getItemTest() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false, false)).thenReturn(this.item);

    this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            true, false, false, null, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
  }

  @Test
  public void getItemsByProductSkuTest() {
    Mockito.when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU, false, false, false)).thenReturn(this.listOfItems);

    List<Item> result = this.itemServiceImpl.getItemsByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU);

    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, false, false,
            false);

    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsByProductSkuTestWithBlankProductSku() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByProductSku(ItemServiceImplTest.STORE_ID, null));
  }

  @Test
  public void getItemsByProductSkuTestWithBlankStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByProductSku(null, ItemServiceImplTest.PRODUCT_SKU));
  }

  @Test
  public void getItemsByProductSkuAndCncActivated_blankStoreId_throwsException() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByProductSkuAndCncActivated(null, USERNAME, REQUEST_ID, PRODUCT_SKU, true));
  }

  @Test
  public void getItemsByProductSkuAndCncActivated_invalidProductSkuFormat_throwsException() {
    try {
      when(this.skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(false);

      this.itemServiceImpl.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU, true);
    } catch (ApplicationRuntimeException e) {
      verify(this.skuValidator).isProductSku(PRODUCT_SKU);
    }
  }

  @Test
  public void getItemsByProductSkuAndCncActivated_cncActivatedTrue_returnsItems() {
    List<Item> items = Collections.singletonList(item);
    Map<String, Set<Price>> priceMap = new HashMap<>();

    when(this.skuValidator.isProductSku(PRODUCT_SKU)).thenReturn(true);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, true)).thenReturn(items);

    this.itemServiceImpl.getItemsByProductSkuAndCncActivated(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU, true);

    verify(this.skuValidator).isProductSku(PRODUCT_SKU);
    verify(this.itemCacheableService).findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(STORE_ID,
        PRODUCT_SKU, true);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
  }

  @Test
  public void getItemsForViewByProductSkuAndPickUpPointTest() throws Exception {
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSkuAndShowDeletedFlag(STORE_ID, PRODUCT_SKU, true, false, false, itemPickupPointMap)).thenReturn(listOfItems);
    List<Item> result =
        this.itemServiceImpl.getItemsForViewByProductSkuAndPickUpPoint(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU,
            false, true, false, itemPickupPointMap);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndShowDeletedFlag(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, true, false, false, itemPickupPointMap);
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewByProductSkuAndPickUpPointShowDeletedTest() throws Exception {
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSkuAndShowDeletedFlag(STORE_ID, PRODUCT_SKU, true, false, true, itemPickupPointMap)).thenReturn(listOfItems);
    itemPickupPoints.get(0).setItemSku(ITEM_SKU);
    List<Item> result =
        this.itemServiceImpl.getItemsForViewByProductSkuAndPickUpPoint(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU,
            true, true, false, itemPickupPointMap);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndShowDeletedFlag(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, true, false, true, itemPickupPointMap);
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewByProductSkuAndPickUpPointFlagFalseTest() throws Exception {
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSkuAndShowDeletedFlag(STORE_ID, PRODUCT_SKU, true, false, false, itemPickupPointMap)).thenReturn(listOfItems);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED, "false", ""));
    itemPickupPoints.get(0).setItemSku(ITEM_SKU);
    List<Item> result =
        this.itemServiceImpl.getItemsForViewByProductSkuAndPickUpPoint(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU,
            false, true, false, itemPickupPointMap);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndShowDeletedFlag(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, true, false, false, itemPickupPointMap);
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsWithDiscountAndPickUpPointDetailsTest() throws Exception {
    Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
    itemPickupPoints.get(0).setItemSku(ITEM_SKU);
    itemPickupPointMap.put(itemPickupPoints.get(0).getItemSku(), itemPickupPoints.get(0));
    this.itemServiceImpl.getItemsWithDiscountAndPickUpPointDetails(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.PICKUP_POINT, listOfItems, itemPickupPoints, itemPickupPointMap, false);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
  }

  @Test
  public void getItemsForViewByProductSkuSync() throws Exception {
    List<Item> result =
        this.itemServiceImpl.getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.IS_SYNCHRONIZED,
            ItemServiceImplTest.LEVEL2_MERCHANT_CODE, false, true, false, false);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, true, false,
            false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewByProductSkuSyncWithItemNotFound() throws Exception {
    try {
      this.itemServiceImpl.getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
          ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND, ItemServiceImplTest.IS_SYNCHRONIZED,
          ItemServiceImplTest.LEVEL2_MERCHANT_CODE, false, true, false, false);
    } catch (Exception e) {
      assertTrue(e instanceof ApplicationRuntimeException);
      Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
      Mockito.verify(this.itemCacheableService)
          .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND, true, false,
              false);

    }
  }

  @Test
  public void getItemsForViewByProductSkuSync_whenFetchAllItemsFlagFalseTest() throws Exception {
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED)).thenReturn(
        new SystemParameter(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED, "false", ""));
    List<Item> result =
        this.itemServiceImpl.getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.IS_SYNCHRONIZED,
            ItemServiceImplTest.LEVEL2_MERCHANT_CODE, true, true, false, false);
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, true, false,
            false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewByProductSkuUnsync() throws Exception {
    List<Item> result = this.itemServiceImpl
        .getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.IS_NOT_SYNCHRONIZED,
            ItemServiceImplTest.LEVEL2_MERCHANT_CODE, false, true, false, false);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU, true, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewByProductSkuUnsyncFetchByDeliveryTrue() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "fetchItemPickupPointWithoutDelivery", true);
    List<Item> result = this.itemServiceImpl
        .getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.IS_NOT_SYNCHRONIZED,
            ItemServiceImplTest.LEVEL2_MERCHANT_CODE, false, true, false, false);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU, true, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewByProductSkuUnsyncFetchByDeliveryTrueMigratedTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "fetchItemPickupPointWithoutDelivery", true);
    List<Item> result = this.itemServiceImpl
        .getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.IS_NOT_SYNCHRONIZED,
            ItemServiceImplTest.LEVEL2_MERCHANT_CODE, false, true, false, true);
    Mockito.verify(this.systemParameterService)
        .findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU, true, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findOneForEachItemSkuIn(anyString(), anyList());
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getItemsForViewTestWithBlankProductSku() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsForViewByProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, null, ItemServiceImplTest.IS_SYNCHRONIZED, ItemServiceImplTest.LEVEL2_MERCHANT_CODE, false, true, false,
        false));
  }

  @Test
  public void getItemsForViewTestWithBlankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsForViewByProductSku(null, ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.IS_SYNCHRONIZED, ItemServiceImplTest.LEVEL2_MERCHANT_CODE,
        false, true, false, false));
  }

  @Test
  public void getItemDetailsTest() {
    Mockito.when(this.itemCacheableService.findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE)).thenReturn(new ItemAndItemPickupPointVo());

    ItemAndItemPickupPointVo
        result = this.itemServiceImpl.getItemDetails(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);

    Mockito.verify(this.itemCacheableService)
        .findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    assertNotNull(result);
  }

  @Test
  public void getItemDetailsFromDBWithPristineDataItemNullTest() {
    item.setPristineDataItem(null);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    ItemAndItemPickupPointVo
        result= this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    assertNotNull(result);
  }

  @Test
  public void getItemDetailsFromDBNullItemPickupPointTest() {
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE));
    }finally {
      Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
      Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void getItemDetailsFromDBwithNullItemTest() {
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(null);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE));
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void getItemDetailsFromDBAndMfdTrueTest() {
    item.setMarkForDelete(true);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE));
  }

  @Test
  public void getItemDetailsFromDBAndForceReviewTrueTest() {
    item.setForceReview(true);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Assertions.assertThrows(ApiIncorrectInputDataException.class, () -> this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE));
  }

  @Test
  public void getItemDetailsFromDBAndmfdFalseAndForceReviewFalseTest() {
    item.setForceReview(false);
    item.setMarkForDelete(false);
    item.setSynchronized(false);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    ItemAndItemPickupPointVo
        result= this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    assertNotNull(result);
  }

  @Test
  public void getItemDetailsFromDBAndSynchronisedTrueAndPristineDataItemTest() {
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    item.setPristineDataItem(pristineDataItem);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    ItemAndItemPickupPointVo
        result= this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    assertNotNull(result);
  }

  @Test
  public void getItemDetailsFromDBAndSynchronisedTrueAndPristineDataItemNullTest() {
    item.setPristineDataItem(null);
    item.setItemCode(null);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    ItemAndItemPickupPointVo
        result= this.itemServiceImpl.getItemDetailsFromDB(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndPickupPointCodeFromDb(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    assertNotNull(result);
  }

  @Test
  public void getItemAndPickupPointDetailsTest() {
    Mockito.when(this.itemCacheableService.findItemAndItemPickPointByproductSkus(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE)).thenReturn(new ItemAndItemPickupPointVo());

    ItemAndItemPickupPointVo result = this.itemServiceImpl.getItemAndPickupPointDetails(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);

    Mockito.verify(this.itemCacheableService).findItemAndItemPickPointByproductSkus(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(ITEM_SKU), false, PAGE, PAGE_SIZE);
    assertNotNull(result);
  }

  @Test
  public void getItemTestSync() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false, false)).thenReturn(this.item);

    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true,
        false, false, null, false, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            true, false, false, null, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemMfdFalseItemTest() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);

    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true,
        false, false, null, false, true);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            true, false, false, null, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemSuspendedTest() throws Exception {
    this.item.setMarkForDelete(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);
    this.product.setMarkForDelete(true);
    this.product.setSuspended(true);
    Mockito.when(
            this.productCacheableService.findProductByStoreIdAndProductSku(item.getStoreId(), item.getProductSku()))
        .thenReturn(this.product);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
          ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true,
          false, false, null, false, true));
    } finally {
      Mockito.verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, true, false, false,
              null, false);
    }
  }

  @Test
  public void getItemProductMfdFalseTest() throws Exception {
    this.item.setMarkForDelete(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);
    this.product.setMarkForDelete(false);
    this.product.setSuspended(true);
    Mockito.when(
            this.productCacheableService.findProductByStoreIdAndProductSku(item.getStoreId(), item.getProductSku()))
        .thenReturn(this.product);
    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true,
        false, false, null, false, true);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            true, false, false, null, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
  }

  @Test
  public void getItemProductMfdTrueAndSunpendedFalseTest() throws Exception {
    this.item.setMarkForDelete(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);
    this.product.setMarkForDelete(true);
    this.product.setSuspended(false);
    Mockito.when(
            this.productCacheableService.findProductByStoreIdAndProductSku(item.getStoreId(), item.getProductSku()))
        .thenReturn(this.product);
    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true,
        false, false, null, false, true);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            true, false, false, null, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
  }

  @Test
  public void getDetailsForActiveOrSuspendedItemTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "skipOverrideL5Data", true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);

    Item result = this.itemServiceImpl.getDetailsForActiveOrSuspendedItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, false, false, null, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, true, false, false,
            null, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getDetailsForActiveOrSuspendedItemSwitchOffTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "skipOverrideL5Data", false);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);

    Item result = this.itemServiceImpl.getDetailsForActiveOrSuspendedItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, false, false, null, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, true, false, false,
            null, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getDetailsForActiveOrSuspendedItemNullObjectTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "skipOverrideL5Data", true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false)).thenReturn(this.item);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(null);
    Item result = this.itemServiceImpl.getDetailsForActiveOrSuspendedItem(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, false, false, null, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, true, false, false,
            null, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemTestUnsync() throws Exception {
    this.item.setSynchronized(false);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false, false)).thenReturn(this.item);
    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true,
        false, false, null, false, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            true, false, false, null, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemFullFetchFalseTest() throws Exception {
    this.item.setSynchronized(false);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false, false, false, null, false, false)).thenReturn(this.item);
    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, false,
        false, false, null, false, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            false, false, false, null, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemFullFetchFalseItemPickupPointNullTest() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false, false, false, null, false, false)).thenReturn(this.item);
    Mockito.when(this.itemPickupPointService
        .findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(null);
    Item result = this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, false,
        false, false, null, false, false);

    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
            false, false, false, null, false, false);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkuAndDelivery(anyString(), anyString());
    assertNotNull(result);
    assertEquals(result, this.item);
  }

  @Test
  public void getItemTestWithItemSkuBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
        ItemServiceImplTest.USERNAME, null, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, true, false, null, false, false
    ));
  }

  @Test
  public void getItemTestWithItemSkuNotFound() throws Exception {
    try {
      Mockito.when(itemCacheableService.findItemByStoreIdAndItemSku(anyString(),
          anyString(), Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), anyString(),
          Mockito.anyBoolean())).thenReturn(null);
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
          ItemServiceImplTest.USERNAME, ItemServiceImplTest.ITEM_SKU_NOT_FOUND,
          ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, true, false, null, false, true));
    } finally {
      Mockito.verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSku(any(), any(),
              Mockito.anyBoolean(), Mockito.anyBoolean(), Mockito.anyBoolean(), any(),
              Mockito.anyBoolean());
    }
  }

  @Test
  public void getItemTestWithRequestIdBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItem(ItemServiceImplTest.STORE_ID, null, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, true, false, null, false, false));
  }

  @Test
  public void getItemTestWithStoreIdBlank() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItem(null, ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.NEED_MASTER_DATA_DETAIL, true, true, false, null, false, false));
  }

  @Test
  public void getListOfItemsByItemSkus() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(
        eq(ItemServiceImplTest.STORE_ID), eq(productSkus))).thenReturn(this.listOfItems);
    Mockito.when(this.itemCacheableService.getCacheableItemsByProductSkus(eq(STORE_ID), eq(productSkus)))
        .thenReturn(Collections.emptyList());
    List<Item> result =
        this.itemServiceImpl.getItemsWithDiscountPriceByProductSkus(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, productSkus, true, false);
    Mockito.verify(this.itemCacheableService)
        .getCacheableItemsByProductSkus(eq(STORE_ID), eq(productSkus));
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(
        eq(ItemServiceImplTest.STORE_ID), eq(productSkus));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.itemCacheableService).setActivePromoBundlingsByPristineOrItemCode(STORE_ID, this.item);
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getListOfItemsByItemSkus_MultiGetTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    Mockito.when(this.itemCacheableService.getCacheableItemsByProductSkus(eq(STORE_ID), eq(productSkus)))
        .thenReturn(Arrays.asList(item));
    List<Item> result =
        this.itemServiceImpl.getItemsWithDiscountPriceByProductSkus(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, productSkus, true, false);
    productSkus.add(PRODUCT_SKU);
    Mockito.verify(this.itemCacheableService)
        .getCacheableItemsByProductSkus(eq(STORE_ID), eq(productSkus));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(this.itemCacheableService).setActivePromoBundlingsByPristineOrItemCode(STORE_ID, this.item);
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void getListOfItemsByItemSkusWithBlankStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsWithDiscountPriceByProductSkus(null, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.REQUEST_ID, this.itemSkus, false, false));
  }

  @Test
  public void getListOfItemsByItemSkusWithNullListOfItemSkus() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsWithDiscountPriceByProductSkus(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, null, false, false));
  }

  @Test
  public void getListOfItemsByProductSkusWithOff2OnTrue() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    item.setOff2OnChannelActive(false);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalseAndOff2OnChannelActiveTrue(
        eq(ItemServiceImplTest.STORE_ID), eq(productSkus))).thenReturn(Collections.emptyList());
    Mockito.when(this.itemCacheableService.getCacheableItemsByProductSkus(eq(STORE_ID), eq(productSkus)))
        .thenReturn(Arrays.asList(item));
    List<Item> result = this.itemServiceImpl.getItemsWithDiscountPriceByProductSkus(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, productSkus, true, true);
    productSkus.add(PRODUCT_SKU);
    Mockito.verify(this.itemCacheableService)
        .getCacheableItemsByProductSkus(eq(STORE_ID), eq(productSkus));
    assertEquals(result.size(), 0);
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(itemServiceImpl, "size", 20);
    ReflectionTestUtils.setField(itemServiceImpl, "attributeMap",
        new ObjectMapper().readValue(ATTRIBUTE_MAP, new TypeReference<Map<String, String>>() {
        }));
    ReflectionTestUtils.setField(itemServiceImpl, "isProductVisibilityEnabled", true);
    Mockito.when(this.channelService.getDefaultChannel()).thenReturn(ItemServiceImplTest.CHANNEL_DEFAULT);
    ReflectionTestUtils.setField(itemServiceImpl, "storeId", STORE_ID);
    ReflectionTestUtils.setField(itemServiceImpl, "requestId", REQUEST_ID);
    ReflectionTestUtils.setField(itemServiceImpl, "username", USERNAME);
    ReflectionTestUtils.setField(itemServiceImpl, "skipOverrideL5Data", false);
    when(this.channelService.getDefaultChannel()).thenReturn(ItemServiceImplTest.CHANNEL_DEFAULT);
    when(itemSummaryUtil.getProductCodeFromItemCode(anyString())).thenReturn(PRODUCT_CODE);

    itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);

    cncItemViewConfig = new ItemViewConfig();
    cncItemViewConfig.setChannel(CNC);

    this.discountPrice = new DiscountPrice();
    this.discountPrice.setDiscountPrice(5000.0);

    this.discountPrices = new ArrayList<>();
    this.discountPrices.add(discountPrice);

    this.price = new Price();
    this.price.setChannel(ItemServiceImplTest.CHANNEL_DEFAULT);
    this.price.setOfferPrice(100000.0);
    this.price.setListPrice(10000.0);
    this.price.setListOfDiscountPrices(discountPrices);
    this.price.setCurrency(CURRENCY);

    this.price1 = new Price();
    this.price1.setChannel(ItemServiceImplTest.CHANNEL_DEFAULT);
    this.price1.setOfferPrice(90000.0);
    this.price1.setListPrice(10000.0);
    this.price1.setCurrency(CURRENCY);

    this.price2 = new Price();
    this.price2.setChannel(ItemServiceImplTest.CHANNEL_WEB);

    this.prices = new HashSet<Price>();
    this.prices.add(this.price);
    this.prices.add(this.price2);

    this.masterDataAttribute = new MasterDataAttribute();

    this.itemAttributeValue = new MasterDataItemAttributeValue();
    this.itemAttributeValue.setAttributeValue(ItemServiceImplTest.VALUE);
    this.itemAttributeValue.setMasterDataAttribute(this.masterDataAttribute);

    this.itemAttributeValues = new ArrayList<MasterDataItemAttributeValue>();
    this.itemAttributeValues.add(this.itemAttributeValue);

    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setMasterDataItemAttributeValues(this.itemAttributeValues);
    this.masterDataItem.setProductCode(PRODUCT_CODE);

    this.item = new Item();
    item.setStoreId(STORE_ID);
    this.item.setItemSku(ItemServiceImplTest.ITEM_SKU);
    this.item.setProductSku(ItemServiceImplTest.PRODUCT_SKU);
    this.item.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.item.setSynchronized(ItemServiceImplTest.IS_SYNCHRONIZED);
    this.item.setPrice(this.prices);
    this.item.setMasterDataItem(this.masterDataItem);
    this.pristineDataItem = new PristineDataItem();
    this.pristineDataItem.setPristineId(PRISTINE_ID);
    this.pristineDataItem.setId("ID");
    this.item.setPristineDataItem(pristineDataItem);
    this.item.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.item.setActivePromoBundlings(Collections.singleton(ItemServiceImplTest.PROMO_BUNDLING_TYPE));

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);

    itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setStoreId(STORE_ID);
    itemPickupPoint.setItemSku(ItemServiceImplTest.ITEM_SKU);
    itemPickupPoint.setProductSku(ItemServiceImplTest.PRODUCT_SKU);
    itemPickupPoint.setOfflineItemId(OFFLINE_ITEM_ID);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setPrice(prices);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.setDelivery(true);
    this.itemPickupPoint.setItemViewConfig(new HashSet<>(Collections.singletonList(itemViewConfig)));

    this.newItemPickupPoint = new ItemPickupPoint();
    newItemPickupPoint.setStoreId(STORE_ID);
    this.newItemPickupPoint.setItemSku(ItemServiceImplTest.ITEM_SKU);
    this.newItemPickupPoint.setProductSku(ItemServiceImplTest.PRODUCT_SKU);
    this.newItemPickupPoint.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.newItemPickupPoint.setItemViewConfig(new HashSet<>(Collections.singletonList(itemViewConfig)));

    this.itemSkus = new HashSet<String>();
    this.itemSkusList = new ArrayList<String>();
    this.listOfItems = new ArrayList<Item>();
    this.listOfItems.add(this.item);
    itemsChanged.put(ITEM_CODE, true);

    itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);

    this.pageOfItems = new PageImpl<>(Arrays.asList(item), PageRequest.of(0, 10), 1);

    this.mapOfItems = new HashMap<String, Item>();
    this.mapOfItems.put(ItemServiceImplTest.ITEM_SKU, this.item);

    this.level2Ids = new ArrayList<String>();
    this.level2Ids.add(ItemServiceImplTest.ITEM_SKU);

    this.productAttribute = new ProductAttribute();
    this.productAttribute.setItemSku(ItemServiceImplTest.ITEM_SKU);

    this.productAttributes = new ArrayList<ProductAttribute>();
    this.productAttributes.add(this.productAttribute);

    this.product = new Product();
    this.masterCatalog.setCategory(category);
    this.masterDataProduct.setMasterCatalog(masterCatalog);
    this.masterDataProduct.setLength(ITEM_LENGTH);
    this.masterDataProduct.setHeight(HEIGHT);
    this.masterDataProduct.setWidth(ITEM_WIDTH);
    this.masterDataProduct.setWeight(ITEM_WEIGHT);
    this.masterDataProduct.setShippingWeight(ITEM_DELIVERY_WEIGHT);
    this.product.setMasterDataProduct(masterDataProduct);
    this.product.setStoreId(STORE_ID);
    this.product.setProductSku(ItemServiceImplTest.PRODUCT_SKU);
    this.product.setMerchantCode(ItemServiceImplTest.MERCHANT_CODE);
    this.product.setDefiningAttributes(this.productAttributes);
    this.product.setProductCode(ItemServiceImplTest.PRODUCT_CODE);

    this.itemUpdated = new Item();
    this.itemUpdated.setStoreId(ItemServiceImplTest.STORE_ID);
    this.itemUpdated.setItemSku(ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED);
    this.itemUpdated.setSynchronized(ItemServiceImplTest.IS_SYNCHRONIZED);

    this.itemSyncTobeUpdated = new Item();
    this.itemSyncTobeUpdated.setStoreId(ItemServiceImplTest.STORE_ID);
    this.itemSyncTobeUpdated.setItemSku(ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED);
    this.itemSyncTobeUpdated.setSynchronized(ItemServiceImplTest.IS_SYNCHRONIZED);
    this.itemSyncTobeUpdated.setItemViewConfigs(new HashSet<>(Collections.singletonList(itemViewConfig)));


    this.masterDataItemUpdated = new MasterDataItem();
    this.masterDataItemUpdated.setItemHeight(ItemServiceImplTest.HEIGHT_UPDATED);
    this.masterDataItemUpdated.setItemDeliveryWeight(ItemServiceImplTest.ITEM_DELIVERY_WEIGHT);
    this.masterDataItemUpdated.setItemLength(ItemServiceImplTest.ITEM_LENGTH);
    this.masterDataItemUpdated.setItemWeight(ItemServiceImplTest.ITEM_WEIGHT);
    this.masterDataItemUpdated.setItemWidth(ItemServiceImplTest.ITEM_WIDTH);
    this.masterDataItemUpdated.setProductCode(PRODUCT_CODE);

    this.itemUnsyncUpdated = new Item();
    this.itemUnsyncUpdated.setStoreId(ItemServiceImplTest.STORE_ID);
    this.itemUnsyncUpdated.setItemSku(ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED);
    this.itemUnsyncUpdated.setSynchronized(!ItemServiceImplTest.IS_SYNCHRONIZED);
    this.itemUnsyncUpdated.setMasterDataItem(this.masterDataItemUpdated);
    this.itemUnsyncUpdated.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.itemUnsyncUpdated.setItemCode(ITEM_CODE);
    this.itemUnsyncUpdated.setItemViewConfigs(new HashSet<>(Collections.singletonList(new ItemViewConfig())));


    this.masterDataItemToBeUpdated = new MasterDataItem();
    this.masterDataItemToBeUpdated.setItemHeight(ItemServiceImplTest.HEIGHT);
    this.masterDataItemToBeUpdated.setItemDeliveryWeight(ItemServiceImplTest.ITEM_DELIVERY_WEIGHT);
    this.masterDataItemToBeUpdated.setItemLength(ItemServiceImplTest.ITEM_LENGTH);
    this.masterDataItemToBeUpdated.setItemWeight(ItemServiceImplTest.ITEM_WEIGHT);
    this.masterDataItemToBeUpdated.setItemWidth(ItemServiceImplTest.ITEM_WIDTH);

    this.itemUnSyncTobeUpdated = new Item();
    this.itemUnSyncTobeUpdated.setStoreId(ItemServiceImplTest.STORE_ID);
    this.itemUnSyncTobeUpdated.setItemSku(ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED);
    this.itemUnSyncTobeUpdated.setSynchronized(!ItemServiceImplTest.IS_SYNCHRONIZED);
    this.itemUnSyncTobeUpdated.setMasterDataItem(this.masterDataItemToBeUpdated);
    this.itemUnSyncTobeUpdated.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.itemUnSyncTobeUpdated.setMerchantCode(ItemServiceImplTest.MERCHANT_CODE);
    this.itemUnSyncTobeUpdated.setItemViewConfigs(new HashSet<>(Collections.singletonList(new ItemViewConfig())));

    this.productDetailResponse = new ProductDetailResponse();
    ProductItemResponse productItemResponse = new ProductItemResponse();
    productItemResponse.setSkuCode(item.getItemCode());
    productItemResponse.setSourceItemCode(SOURCE_ITEM_CODE);
    productItemResponse.setContentChanged(true);
    productItemResponse.setProductItemAttributeValueResponses(new ArrayList<>());
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponse.setCategoryCode(PRODUCT_CATEGORY_CODE);
    ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
    productCategoryResponse.setCategory(categoryResponse);
    Set set = new HashSet();
    set.add(productItemResponse);

    this.productDetailResponse.setProductItemResponses(set);
    this.productDetailResponse.setProductCategoryResponses(Collections.singletonList(productCategoryResponse));
    this.productDetailResponse.setProductCode(this.product.getProductCode());
    productDetailResponse.setLength(10.0);
    productDetailResponse.setWeight(10.0);
    productDetailResponse.setWidth(10.0);
    productDetailResponse.setHeight(10.0);
    productDetailResponse.setShippingWeight(10.0);

    itemCategories = new ArrayList<>();
    itemCategories.add(new ItemCategoryVO(1, CATEGORY_ID, CATEGORY_NAME, PRODUCT_CATEGORY_CODE));
    itemCategories.add(new ItemCategoryVO(2, CATEGORY_ID, CATEGORY_NAME, PRODUCT_CATEGORY_CODE));
    itemCategories.add(new ItemCategoryVO(3, CATEGORY_ID_TWO, CATEGORY_NAME, PRODUCT_CATEGORY_CODE_TWO));
    itemCategories.add(new ItemCategoryVO(3, CATEGORY_ID_TWO, CATEGORY_NAME, PRODUCT_CATEGORY_CODE_TWO));
    itemCategories.add(new ItemCategoryVO(4, CATEGORY_ID, CATEGORY_NAME, PRODUCT_CATEGORY_CODE));

    this.mapOfMasterDataItems = new HashMap<>();
    mapOfMasterDataItems.put(item.getItemCode(), new MasterDataItem());
    mapOfMasterDataItems.put(ITEM_CODE_NOT_ADD, new MasterDataItem());

    this.itemSkuList = new ArrayList<>();
    this.itemSkuList.add(ITEM_SKU);
    this.itemSkuList.add(ITEM_SKU2);

    this.offlineItem = new OfflineItem();
    this.offlineItemList = new ArrayList<>();
    this.offlineItemList.add(offlineItem);

    masterDataProductAndItemsVO = new MasterDataProductAndItemsVO();
    masterDataProductAndItemsVO.setMasterDataProduct(masterDataProduct);

    when(this.itemRepository.insert(this.item)).thenReturn(this.item);
    Mockito.when(this.itemRepository.insert(this.item)).thenReturn(this.item);

    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false, false, null, false, false)).thenReturn(this.item);

    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_NOT_FOUND, true, false, false, null, false, false)).thenReturn(null);

    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false)).thenReturn(this.item);

    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_NOT_FOUND, false)).thenReturn(null);

    Mockito.when(this.objectConverterService.convertToPriceHistory(this.price2, ItemServiceImplTest.ITEM_SKU)).thenReturn(this.priceHistory);

    Mockito.when(this.itemRepository.save(this.item)).thenReturn(this.item);

    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(
        ItemServiceImplTest.STORE_ID, this.itemSkus)).thenReturn(this.listOfItems);

    Mockito.when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU, true, false, false)).thenReturn(this.listOfItems);

    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU)).thenReturn(this.product);

    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND))
        .thenReturn(null);

    Mockito.when(
        this.productHelperService.addItemAttributeToProductAttribute(this.product, ItemServiceImplTest.ITEM_SKU, this.itemAttributeValues)).thenReturn(this.product);

    Mockito.when(this.productHelperService.setMultipleMasterDataItemsFromMasterData(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, this.listOfItems)).thenReturn(this.listOfItems);

    Mockito.when(this.itemRepository.insert(this.itemWithMasterDataItem)).thenReturn(this.itemWithMasterDataItem);

    Mockito.when(this.productHelperService.deleteItemAttributeFromProductAttribute(this.product, ItemServiceImplTest.ITEM_SKU))
        .thenReturn(this.product);

    Mockito.when(this.saveOperationService.saveProduct(this.product)).thenReturn(this.product);

    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED, true, false, false, null, false, false)).thenReturn(this.itemSyncTobeUpdated);
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED, false)).thenReturn(this.itemSyncTobeUpdated);
    Mockito.when(this.itemRepository.save(this.itemSyncTobeUpdated)).thenReturn(this.itemUpdated);

    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, true, false, false, null, false, false)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.itemRepository.save(this.itemUnSyncTobeUpdated)).thenReturn(this.itemUnsyncUpdated);

    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
        ItemServiceImplTest.STORE_ID_NOT_FOUND, ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED, true, false, false, null,
        false, false)).thenReturn(null);

    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.itemRepository.save(this.itemUnSyncTobeUpdated)).thenReturn(this.itemUnsyncUpdated);

    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID_NOT_FOUND,
        ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED, false)).thenReturn(null);

    Mockito.when(this.itemRepository.saveAll(this.listOfItems)).thenReturn(this.listOfItems);

    Mockito.when(this.productHelperService
        .getCurrentBuyableStatusForItem(this.item.getItemViewConfigs(), ItemServiceImplTest.CHANNEL_DEFAULT,
            item.isArchived())).thenReturn(true);
    Mockito.when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED)).thenReturn(
        new com.gdn.x.product.model.entity.SystemParameter(STORE_ID,
            SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED, "true", ""));

    Mockito.when(this.masterDataCacheService.getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE))
        .thenReturn(masterDataProductAndItemsVO);

    this.itemRequestVO = new Item();
    this.itemRequestVO.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.itemRequestVO.setItemViewConfigs(this.setItemViewConfigDefault);
    this.itemRequestVO.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.itemRequestVO.setPrice(this.prices);

    this.listOfItemRequestVO = new ArrayList<Item>();
    this.listOfItemRequestVO.add(this.itemRequestVO);

    this.itemWithGeneratedItemSku = new Item();
    this.itemWithGeneratedItemSku.setItemSku(ItemServiceImplTest.ITEM_SKU);
    this.itemWithGeneratedItemSku.setStoreId(ItemServiceImplTest.STORE_ID);
    this.itemWithGeneratedItemSku.setSynchronized(ItemServiceImplTest.IS_SYNCHRONIZED);
    this.itemWithGeneratedItemSku.setItemCode(this.itemRequestVO.getItemCode());
    this.itemWithGeneratedItemSku.setMerchantSku(this.itemRequestVO.getMerchantSku());
    this.itemWithGeneratedItemSku.setItemViewConfigs(this.itemRequestVO.getItemViewConfigs());
    this.itemWithGeneratedItemSku.setPrice(this.itemRequestVO.getPrice());
    this.itemWithGeneratedItemSku.setMasterDataItem(masterDataItem);

    Mockito.when(this.productHelperService.containDefaultChannelPrice(this.itemRequestVO)).thenReturn(true);

    this.productWithProductAttribute = new Product();
    this.productWithProductAttribute.setProductSku(ItemServiceImplTest.PRODUCT_SKU);
    this.productWithProductAttribute.setProductCode(ItemServiceImplTest.PRODUCT_CODE);
    this.productWithProductAttribute.setMerchantCode(ItemServiceImplTest.MERCHANT_CODE);
    this.productWithProductAttribute.getDefiningAttributes().add(this.productAttribute);

    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU)).thenReturn(this.productWithProductAttribute);

    Mockito.when(this.productHelperService.setItemDetail(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU,
        ItemServiceImplTest.MERCHANT_CODE, ItemServiceImplTest.SIZE_OF_PRODUCT_ATTRIBUTES, this.itemRequestVO)).thenReturn(this.itemWithGeneratedItemSku);

    itemPickupPoints = Arrays.asList(new ItemPickupPoint());
    ItemVo itemVo = gdnMapper.deepCopy(itemWithGeneratedItemSku, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);

    Mockito.when(this.itemRepository.insert(this.itemWithGeneratedItemSku)).thenReturn(this.itemWithGeneratedItemSku);

    this.itemWithGeneratedItemSku.setMasterDataItem(this.masterDataItem);
    Mockito.when(this.productHelperService.setMasterDataItemFromMasterData(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, this.itemWithGeneratedItemSku)).thenReturn(this.itemWithGeneratedItemSku);

    Mockito.when(this.productHelperService.addItemAttributeToProductAttribute(this.productWithProductAttribute, this.itemWithGeneratedItemSku.getItemSku(),
            this.itemWithGeneratedItemSku.getMasterDataItem().getMasterDataItemAttributeValues()))
        .thenReturn(this.productWithProductAttribute);

    Mockito.when(this.saveOperationService.saveProduct(this.productWithProductAttribute))
        .thenReturn(this.productWithProductAttribute);

    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND))
        .thenReturn(null);

    this.itemRequestVOForAddItems_1 = new Item();
    this.itemRequestVOForAddItems_1.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.itemRequestVOForAddItems_1.setItemViewConfigs(this.itemViewConfigSetForAddItems);
    this.itemRequestVOForAddItems_1.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.itemRequestVOForAddItems_1.setPrice(this.prices);

    this.itemRequestVOForAddItems_2 = new Item();
    this.itemRequestVOForAddItems_2.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.itemRequestVOForAddItems_2.setItemViewConfigs(this.itemViewConfigSetForAddItems);
    this.itemRequestVOForAddItems_2.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.itemRequestVOForAddItems_2.setPrice(this.prices);

    this.listOfItemRequestVOForAddItems = new ArrayList<Item>();
    this.listOfItemRequestVOForAddItems.add(this.itemRequestVOForAddItems_1);
    this.listOfItemRequestVOForAddItems.add(this.itemRequestVOForAddItems_2);

    this.productAttributeForAddItems = new ProductAttribute();

    this.productForAddItems = new Product();
    this.productForAddItems.setProductSku(ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS);
    this.productForAddItems.getDefiningAttributes().add(this.productAttributeForAddItems);

    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS))
        .thenReturn(this.productForAddItems);

    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID_NOT_FOUND,
        ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS)).thenReturn(null);

    this.convertToItemForAddItems_1 = new Item();
    this.convertToItemForAddItems_1.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.convertToItemForAddItems_1.setItemViewConfigs(this.itemViewConfigSetForAddItems);
    this.convertToItemForAddItems_1.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.convertToItemForAddItems_1.setPrice(this.prices);

    Mockito.when(this.productHelperService.setMasterDataItemFromMasterData(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, this.item)).thenReturn(this.item);

    this.convertToItemForAddItems_2 = new Item();
    this.convertToItemForAddItems_2.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.convertToItemForAddItems_2.setItemViewConfigs(this.itemViewConfigSetForAddItems);
    this.convertToItemForAddItems_2.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.convertToItemForAddItems_2.setPrice(this.prices);

    this.convertTolistOfItemsForAddItems = new ArrayList<Item>();
    this.convertTolistOfItemsForAddItems.add(this.convertToItemForAddItems_1);
    this.convertTolistOfItemsForAddItems.add(this.convertToItemForAddItems_2);

    Mockito.when(this.productHelperService.setItemDetail(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS, ItemServiceImplTest.MERCHANT_CODE, ItemServiceImplTest.SIZE_OF_PRODUCT_ATTRIBUTES, this.itemRequestVOForAddItems_1))
        .thenReturn(this.convertToItemForAddItems_1);

    Mockito.when(this.productHelperService.setItemDetail(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS, ItemServiceImplTest.MERCHANT_CODE, ItemServiceImplTest.SIZE_OF_PRODUCT_ATTRIBUTES_2, this.itemRequestVOForAddItems_2))
        .thenReturn(this.convertToItemForAddItems_2);

    Mockito.when(this.itemRepository.insert(this.convertTolistOfItemsForAddItems))
        .thenReturn(this.convertTolistOfItemsForAddItems);

    this.convertToItemForAddItems_1_WithMasterDataItem = new Item();
    this.convertToItemForAddItems_1_WithMasterDataItem.setItemSku(ItemServiceImplTest.ITEM_SKU);
    this.convertToItemForAddItems_1_WithMasterDataItem.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.convertToItemForAddItems_1_WithMasterDataItem.setItemViewConfigs(this.itemViewConfigSetForAddItems);
    this.convertToItemForAddItems_1_WithMasterDataItem.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.convertToItemForAddItems_1_WithMasterDataItem.setPrice(this.prices);
    this.convertToItemForAddItems_1_WithMasterDataItem.setMasterDataItem(this.masterDataItem);

    this.convertToItemForAddItems_2_WithMasterDataItem = new Item();
    this.convertToItemForAddItems_2_WithMasterDataItem.setItemSku(ItemServiceImplTest.ITEM_SKU2);
    this.convertToItemForAddItems_2_WithMasterDataItem.setItemCode(ItemServiceImplTest.ITEM_CODE);
    this.convertToItemForAddItems_2_WithMasterDataItem.setItemViewConfigs(this.itemViewConfigSetForAddItems);
    this.convertToItemForAddItems_2_WithMasterDataItem.setMerchantSku(ItemServiceImplTest.MERCHANT_SKU);
    this.convertToItemForAddItems_2_WithMasterDataItem.setPrice(this.prices);
    this.convertToItemForAddItems_2_WithMasterDataItem.setMasterDataItem(this.masterDataItem);

    this.convertTolistOfItemsForAddItems_WithMasterDataItem = new ArrayList<Item>();
    this.convertTolistOfItemsForAddItems_WithMasterDataItem.add(this.convertToItemForAddItems_1_WithMasterDataItem);
    this.convertTolistOfItemsForAddItems_WithMasterDataItem.add(this.convertToItemForAddItems_2_WithMasterDataItem);

    Mockito.when(this.productHelperService.setMultipleMasterDataItemsFromMasterData(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, this.convertTolistOfItemsForAddItems))
        .thenReturn(this.convertTolistOfItemsForAddItems_WithMasterDataItem);

    // masterDataItem
    // when(this.productHelperService.getMasterDataItemsByItems(REQUEST_ID, USERNAME,
    // this.convertTolistOfItemsForAddItems)).thenReturn();

    this.productAttributeForAddItems = new ProductAttribute();
    this.productAttributeForAddItems.setItemSku(this.convertToItemForAddItems_1_WithMasterDataItem.getItemSku());

    this.productForAddItems_WithProductAttribute_1 = new Product();
    this.productForAddItems_WithProductAttribute_1.setProductSku(ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS);
    this.productForAddItems_WithProductAttribute_1.getDefiningAttributes().add(this.productAttributeForAddItems);

    this.productAttributeForAddItems_2 = new ProductAttribute();
    this.productAttributeForAddItems_2.setItemSku(this.convertToItemForAddItems_2_WithMasterDataItem.getItemSku());

    this.productForAddItems_WithProductAttribute_2 = new Product();
    this.productForAddItems_WithProductAttribute_2.setProductSku(ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS);
    this.productForAddItems_WithProductAttribute_2.getDefiningAttributes().add(this.productAttributeForAddItems_2);

    Mockito.when(this.productHelperService.addItemAttributeToProductAttribute(this.productForAddItems, this.convertToItemForAddItems_1_WithMasterDataItem.getItemSku(),
            this.convertToItemForAddItems_1_WithMasterDataItem.getMasterDataItem().getMasterDataItemAttributeValues()))
        .thenReturn(this.productForAddItems_WithProductAttribute_1);

    Mockito.when(this.productHelperService.addItemAttributeToProductAttribute(this.productForAddItems, this.convertToItemForAddItems_2_WithMasterDataItem.getItemSku(),
            this.convertToItemForAddItems_2_WithMasterDataItem.getMasterDataItem().getMasterDataItemAttributeValues()))
        .thenReturn(this.productForAddItems_WithProductAttribute_1);

    this.productAttributeForAddItems_1 = new ProductAttribute();
    this.productAttributeForAddItems_1.setItemSku(this.convertToItemForAddItems_1_WithMasterDataItem.getItemSku());
    this.productAttributeForAddItems_1.setItemSku(this.convertToItemForAddItems_2_WithMasterDataItem.getItemSku());

    this.productForAddItems_WithProductAttributes = new Product();
    this.productForAddItems_WithProductAttributes.setProductSku(ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS);
    this.productForAddItems_WithProductAttributes.getDefiningAttributes().add(this.productAttributeForAddItems);

    Mockito.when(this.saveOperationService.saveProduct(this.productForAddItems_WithProductAttributes))
        .thenReturn(this.productForAddItems_WithProductAttributes);

    Mockito.when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND, true, false, false)).thenReturn(new ArrayList<Item>());
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU_WITH_EMPTY_PRICE, true, false, false, null, false, false))
        .thenReturn(this.itemWithEmptyPrice);
    List<String> skus = new ArrayList<String>();
    skus.add(ItemServiceImplTest.ITEM_SKU);
    Mockito.when(this.itemRepository.assignTicketTemplate(ItemServiceImplTest.STORE_ID, skus,
        ItemServiceImplTest.TICKET_TEMPLATE_ID)).thenReturn(this.listOfItemRequestVO);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU)).thenReturn(this.product);

    Map<String, Set<Price>> mapOfPrices = new HashMap<String, Set<Price>>();
    mapOfPrices.put(ItemServiceImplTest.ITEM_SKU, new HashSet<Price>());
    Mockito.when(this.skuValidator.isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU2)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU_NOT_FOUND)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSkuL4OrL5(ItemServiceImplTest.ITEM_SKU_WITH_EMPTY_PRICE)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ItemServiceImplTest.ITEM_SKU)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ItemServiceImplTest.ITEM_SKU2)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ItemServiceImplTest.ITEM_SKU_NOT_FOUND)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ItemServiceImplTest.ITEM_SKU_WITH_EMPTY_PRICE)).thenReturn(true);
    Mockito.when(this.skuValidator.isProductSku(ItemServiceImplTest.PRODUCT_SKU)).thenReturn(true);
    Mockito.when(this.skuValidator.isProductSku(ItemServiceImplTest.PRODUCT_SKU_FOR_ADD_ITEMS)).thenReturn(true);
    Mockito.when(this.skuValidator.isProductSku(ItemServiceImplTest.PRODUCT_SKU_NOT_FOUND)).thenReturn(true);

    this.activeComboRequestVO = new ActiveComboRequestVO();
    this.itemCodes = new HashSet<>();
    this.itemSkus = new HashSet<>();
    this.pristineIdSet = new HashSet<>();

    mandatoryRequestParam = MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID, USERNAME, null);

    this.comboRule = new ComboRule();
    this.comboRule.setItemSku(ITEM_SKU);
    this.comboRule.setMainSku(true);
    this.comboRule.setDiscountPercentage(DISCOUNT_PERCENTAGE);

    this.comboRule2 = new ComboRule();
    this.comboRule2.setItemSku(ITEM_SKU2);
    this.comboRule2.setMainSku(false);
    this.comboRule2.setDiscountPercentage(DISCOUNT_PERCENTAGE);

    this.comboRuleList = new ArrayList<>();
    this.comboRuleList.add(comboRule);
    this.comboRuleList.add(comboRule2);

    this.promoBundlingDetailResponse = new PromoBundlingDetailResponse();
    this.promoBundlingDetailResponse.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.promoBundlingDetailResponse.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.promoBundlingDetailResponse.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.promoBundlingDetailResponse.setComboRules(comboRuleList);
    this.promoBundlingDetailResponse.setItemSku(ITEM_SKU);

    this.promoBundlingDetailResponses = new ArrayList<>();
    this.promoBundlingDetailResponses.add(promoBundlingDetailResponse);

    this.masterDataAttribute = new MasterDataAttribute();
    this.masterDataAttribute.setAttributeCode(ATTRIBUTE_CODE);
    this.masterDataAttribute.setAttributeName(ATTRIBUTE_NAME);

    this.masterDataItemAttributeValue = new MasterDataItemAttributeValue();
    this.masterDataItemAttributeValue.setAttributeValue(ATTRIBUTE_VALUE);
    this.masterDataItemAttributeValue.setMasterDataAttribute(this.masterDataAttribute);

    this.masterDataItemAttributeValueList = new ArrayList<>();
    this.masterDataItemAttributeValueList.add(this.masterDataItemAttributeValue);

    this.masterDataItem = new MasterDataItem();
    this.masterDataItem.setMasterDataItemAttributeValues(this.masterDataItemAttributeValueList);

    this.pristineDataItem = new PristineDataItem();
    this.pristineDataItem.setPristineId(PRISTINE_ID);

    this.comboItemVO = new ComboItemVO();
    this.comboItemVO.setProductName(PRODUCT_NAME);
    this.comboItemVO.setItemCode(ITEM_CODE);
    this.comboItemVO.setItemSku(ITEM_SKU);
    this.comboItemVO.setQuantity(QUANTITY);
    this.comboItemVO.setDiscountPercentage(10.0);
    this.comboItemVO.setProductCode(PRODUCT_CODE);
    this.comboItemVO.setProductSku(PRODUCT_SKU);

    this.comboItemVOList = new ArrayList<>();
    this.comboItemVOList.add(this.comboItemVO);

    this.comboVO = new ComboVO();
    this.comboVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    this.comboVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.comboVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.comboVO.setComboItems(comboItemVOList);

    this.comboVOList = new ArrayList<>();
    this.comboVOList.add(comboVO);

    ItemPriceVO itemPriceVO = new ItemPriceVO();
    itemPriceVO.setListPrice(item.getPrice().stream().collect(Collectors.toList()).get(0).getListPrice());
    itemPriceVO.setOfferPrice(item.getPrice().stream().collect(Collectors.toList()).get(0).getOfferPrice());
    this.itemPriceVOS = new ArrayList<>();
    this.itemPriceVOS.add(itemPriceVO);
    Mockito.when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(listOfItems);

    this.productDomainEventModel = new ProductDomainEventModel();
    this.productDomainEventModel.setProductCode(PRODUCT_CODE);

    systemParameter.setValue(CATEGORY_CODE_FOR_UNSYNC_CHECK);
    systemParameter.setVariable(Constants.CATEGORY_CODE_VARIABLE);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE)).thenReturn(systemParameter);

    category.setCategoryCode(PRODUCT_CATEGORY_CODE);

    itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel("DEFAULT");
    itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);

    itemViewConfigWithBuyableFalse = new ItemViewConfig();
    itemViewConfigWithBuyableFalse.setBuyable(false);
    itemViewConfigWithBuyableFalse.setChannel("DEFAULT");

    itemViewConfigsWithBuyableFalse = new HashSet<>();
    itemViewConfigsWithBuyableFalse.add(itemViewConfigWithBuyableFalse);

    pickupPointUpdateRequest = new PickupPointUpdateRequest();
    pickupPointUpdateRequest.setProductSku(PRODUCT_SKU);
    PickupPointUpdateItemRequest pickupPointUpdateItemRequest = new PickupPointUpdateItemRequest();
    pickupPointUpdateItemRequest.setItemSku(ITEM_SKU);
    pickupPointUpdateItemRequest.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointUpdateRequest.setPickupPointUpdateItemRequestList(Arrays.asList(pickupPointUpdateItemRequest));
    itemActivationRequest =
        NeedCorrectionItemActivationRequest.builder().itemSku(ITEM_SKU).isBuyable(true).isDiscoverable(true)
            .cncDiscoverable(true).cncBuyable(true)
            .listPrice(10.0).offerPrice(11.0).merchantSku(MERCHANT_SKU).pickupPointCode(PICKUP_POINT_CODE).build();

    Mockito.when(saveOperationService.saveProductAndItemsAndPickupPoint(any(Product.class),
        anyList())).thenReturn(new ProductAndItemsVO());

    promoBundlingActivatedDeactivatedEventModel =
        PromoBundlingActivatedDeactivatedEventModel.builder().merchantCode(MERCHANT_CODE).storeId(STORE_ID)
            .itemSkus(new HashSet<>(Arrays.asList(ITEM_SKU))).build();

    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);

    Mockito.when(
        this.itemPickupPointService.findByItemSkusAndDelivery(anyString(), anyList(),
            eq(true))).thenReturn(Arrays.asList(
        ItemPickupPoint.builder().itemSku(item.getItemSku()).pickupPointCode(item.getPickupPointCode())
            .itemViewConfig(item.getItemViewConfigs()).price(item.getPrice()).build()));
    Mockito.when(
            this.itemPickupPointService.findOneForEachItemSkuIn(anyString(), anyList()))
        .thenReturn(Arrays.asList(
            ItemPickupPoint.builder().itemSku(item.getItemSku()).pickupPointCode(item.getPickupPointCode())
                .itemViewConfig(item.getItemViewConfigs()).price(item.getPrice()).build()));
    Mockito.when(itemPriceService.getDiscountItemPickupPoint(anyList())).thenReturn(
        ImmutableMap.of(item.getItemSku() + Constants.COLON + item.getPickupPointCode(), item.getPrice()));

    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE))
        .thenReturn(systemParameter);

    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT))
        .thenReturn(new SystemParameter(STORE_ID,
            SystemParameterNames.PRODUCT_SKU_LIST_LIMIT, "2", ""));

    ItemPickupPoint itemPickupPointNeedrevison1 = new ItemPickupPoint();
    itemPickupPointNeedrevison1.setItemSku(ITEM_SKU);
    itemPickupPointNeedrevison1.setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointNeedrevison1.setItemViewConfig(itemViewConfigs);
    ItemPickupPoint itemPickupPointNeedrevison2 = new ItemPickupPoint();
    itemPickupPointNeedrevison2.setItemSku(ITEM_SKU);
    itemPickupPointNeedrevison2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemPickupPointNeedrevison2.setItemViewConfig(itemViewConfigs);
    itemPickupPointNeedrevisonList = new ArrayList<>();
    itemPickupPointNeedrevisonList.add(itemPickupPointNeedrevison1);
    itemPickupPointNeedrevisonList.add(itemPickupPointNeedrevison2);

    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);

    profileResponse.setCompany(new CompanyDTO());
    b2bItemViewConfig.setChannel(Constants.B2B);
    businessPartner = new BusinessPartner();
    List<String> salesChannel = new ArrayList<>();
    salesChannel.add(B2C_SELLER_CHANNEL);
    salesChannel.add(B2B_SELLER_CHANNEL);
    businessPartner.setSalesChannel(salesChannel);
    itemPickupPointMap.put(ITEM_SKU + Constants.HYPHEN + PICKUP_POINT_CODE, itemPickupPoint);
    itemMap = new HashMap<>();
    itemMap.put(ITEM_SKU, item);
    editProductDetailDTO = new EditProductDetailDTO();
    editProductDetailDTO.setProduct(product);
    editProductDetailDTO.setProductDetailResponse(productDetailResponse);
    editProductDetailDTO.setAllItemPickupPointMap(itemPickupPointMap);
    editProductDetailDTO.setAllItemMap(itemMap);

    masterSkuMapping = new MasterSkuMappingEventModel();
    masterSkuMapping.setItemSku(ITEM_SKU);
    masterSkuMapping.setMasterItemSku(MASTER_SKU);
    masterSkuMapping.setStoreId(STORE_ID);

    businessPartnerPickupPoint = new BusinessPartnerPickupPoint();
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE);
    businessPartnerPickupPoint.setDelivery(true);
    businessPartnerPickupPoint.setCncActivated(true);

    Mockito.when(this.channelService.getCncChannel()).thenReturn(CNC);
    Mockito.when(this.channelService.getDefaultChannel()).thenReturn(Constants.DEFAULT);

    productSkuAndMerchantCodeMap.put(PRODUCT_SKU, MERCHANT_CODE);
  }

  @Test
  public void getAllItemSkuByItemCodeTest() throws Exception {
    listOfItems.get(0).setItemViewConfigs(itemViewConfigs);
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigs);
    Mockito.when(
        itemCacheableService.findItemsByPristineId(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
            anyString(), eq(false))).thenReturn(listOfItems);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPointList.get(0));
    List<ItemPriceVO> itemPriceVOS =
        itemServiceImpl.getAllItemSkuByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, false);
    Mockito.verify(itemCacheableService)
        .findItemsByPristineId(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), anyString(),
            eq(false));
    Mockito.verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertEquals(itemPriceVOS.size(), 1);
    assertEquals(itemPriceVOS.get(0).getMerchantCode(), listOfItems.get(0).getMerchantCode());
  }

  @Test
  public void getAllItemSkuByItemCodeNoPristineMapping() throws Exception {
    listOfItems.get(0).setItemViewConfigs(itemViewConfigs);
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigs);
    listOfItems.get(0).setPristineDataItem(null);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPointList.get(0));
    List<ItemPriceVO> itemPriceVOS =
        itemServiceImpl.getAllItemSkuByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, false);
    Mockito.verify(itemCacheableService, times(1))
        .findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertEquals(itemPriceVOS.size(), 1);
    assertEquals(itemPriceVOS.get(0).getMerchantCode(), listOfItems.get(0).getMerchantCode());
  }


  @Test
  public void getAllItemSkuByItemCodeNoPristineMappingBuyableFalse() {
    listOfItems.get(0).setItemViewConfigs(itemViewConfigsWithBuyableFalse);
    listOfItems.get(0).setPristineDataItem(null);
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigsWithBuyableFalse);
    Mockito.when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(listOfItems);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPointList.get(0));
    List<ItemPriceVO> itemPriceVOS =
        itemServiceImpl.getAllItemSkuByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, false);
    Mockito.verify(itemCacheableService, times(1))
        .findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertEquals(0, itemPriceVOS.size());
  }

  @Test
  public void getAllItemSkuByItemCodeNoPristineMappingFetchAllItems() {
    listOfItems.get(0).setItemViewConfigs(itemViewConfigs);
    listOfItems.get(0).setPristineDataItem(null);
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigs);
    Mockito.when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(listOfItems);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPointList.get(0));
    List<ItemPriceVO> itemPriceVOS =
        itemServiceImpl.getAllItemSkuByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, true);
    Mockito.verify(itemCacheableService, times(1))
        .findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertEquals(1, itemPriceVOS.size());
    assertEquals(itemPriceVOS.get(0).getMerchantCode(), listOfItems.get(0).getMerchantCode());
    assertTrue(itemPriceVOS.get(0).isBuyable());
  }

  @Test
  public void getAllItemSkuByItemCodeBuyableFalseTest() {
    listOfItems.get(0).setItemViewConfigs(itemViewConfigsWithBuyableFalse);
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigsWithBuyableFalse);
    Mockito.when(itemCacheableService.findItemsByPristineId(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
        anyString(), eq(false))).thenReturn(listOfItems);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPointList.get(0));
    List<ItemPriceVO> itemPriceVOS =
        itemServiceImpl.getAllItemSkuByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, false);
    Mockito.verify(itemCacheableService)
        .findItemsByPristineId(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), anyString(),
            eq(false));
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    assertEquals(0, itemPriceVOS.size());
  }

  @Test
  public void getAllItemSkuByItemCodeBuyableFalseFullFetchTest() {
    listOfItems.get(0).setItemViewConfigs(itemViewConfigsWithBuyableFalse);
    itemPickupPointList.get(0).setItemViewConfig(itemViewConfigsWithBuyableFalse);
    Mockito.when(itemCacheableService.findItemsByPristineId(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID),
        anyString(), eq(false))).thenReturn(listOfItems);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPointList.get(0));
    List<ItemPriceVO> itemPriceVOS =
        itemServiceImpl.getAllItemSkuByItemCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_CODE, true);
    Mockito.verify(itemCacheableService)
        .findItemsByPristineId(eq(STORE_ID), eq(USERNAME), eq(REQUEST_ID), anyString(),
            eq(false));
    Mockito.verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    assertEquals(1, itemPriceVOS.size());
    assertEquals(itemPriceVOS.get(0).getMerchantCode(), listOfItems.get(0).getMerchantCode());
    assertFalse(itemPriceVOS.get(0).isBuyable());
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(this.objectConverterService);
    Mockito.verifyNoMoreInteractions(this.itemCacheableService);
    Mockito.verifyNoMoreInteractions(this.productHelperService);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.channelService);
    Mockito.verifyNoMoreInteractions(this.itemPriceService);
    Mockito.verifyNoMoreInteractions(this.executorService);
    Mockito.verifyNoMoreInteractions(this.cacheEvictItemService);
    Mockito.verifyNoMoreInteractions(this.objectConverterService);
    Mockito.verifyNoMoreInteractions(this.itemCacheableService);
    Mockito.verifyNoMoreInteractions(this.productHelperService);
    Mockito.verifyNoMoreInteractions(this.productService);
    Mockito.verifyNoMoreInteractions(this.channelService);
    Mockito.verifyNoMoreInteractions(this.itemPriceService);
    Mockito.verifyNoMoreInteractions(this.executorService);
    Mockito.verifyNoMoreInteractions(this.promoBundlingService);
    Mockito.verifyNoMoreInteractions(this.offlineItemService);
    Mockito.verifyNoMoreInteractions(this.cacheEvictHelperService);
    Mockito.verifyNoMoreInteractions(this.pristineItemRepository);
    Mockito.verifyNoMoreInteractions(this.saveAndPublishService);
    Mockito.verifyNoMoreInteractions(this.systemParameterService);
    Mockito.verifyNoMoreInteractions(this.masterDataCacheService);
    Mockito.verifyNoMoreInteractions(this.productAndItemSolrIndexerService);
    Mockito.verifyNoMoreInteractions(this.productL3SolrService);
    Mockito.verifyNoMoreInteractions(this.itemPickupPointService);
    Mockito.verifyNoMoreInteractions(this.productL3SolrReindexStatusService);
    Mockito.verifyNoMoreInteractions(this.xCampaignOutbound);
    Mockito.verifyNoMoreInteractions(cachedService);
  }

  @Test
  public void testAddItems() throws Exception {
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(items);
    assertNotNull(items.get(0).getPristineDataItem());
    assertTrue(items.get(0).isContentChanged());
    assertTrue(items.get(0).isInitialContentChanged());
    assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void testAddItemsForceReview() throws Exception {
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      item.setForceReview(true);
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS),
              eq(this.MERCHANT_CODE), eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).setForceReview(true);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage = new PageImpl<>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(eq(this.STORE_ID),
        anyString(), any(Pageable.class))).thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(
        productHelperService.addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList())).thenReturn(this.product);
    List<ItemVo> items =
        this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME, this.PRODUCT_SKU_FOR_ADD_ITEMS,
            productItemsVo.getItemVoList(), this.product, this.productDetailResponse, businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .setItemDetail(anyString(), anyString(), anyString(), Mockito.anyInt(),
            any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(), anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(), anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(), anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(items);
    assertNotNull(items.get(0).getPristineDataItem());
    assertTrue(items.get(0).isContentChanged());
    assertTrue(items.get(0).isInitialContentChanged());
    assertEquals(SOURCE_ITEM_CODE, items.get(0).getSourceItemCode());
    assertEquals(false, itemVo.isCncActive());
    assertTrue(itemVo.getItemPickupPointVoList().get(0).isForceReview());
  }

  @Test
  public void testAddItemsWithB2bConfig() throws Exception {
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    Set<ItemViewConfig> itemViewConfigs = itemPickupPoints.get(0).getAllItemViewConfigs().stream()
        .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class)).collect(Collectors.toSet());
    itemVo.getItemPickupPointVoList().get(0).setItemViewConfig(itemViewConfigs);
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(items);
    assertNotNull(items.get(0).getPristineDataItem());
    assertTrue(items.get(0).isContentChanged());
    assertTrue(items.get(0).isInitialContentChanged());
    assertEquals(SOURCE_ITEM_CODE, items.get(0).getSourceItemCode());
    assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void testAddItemsWithB2bConfigWithDiscovableTrue() throws Exception {
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    b2bItemViewConfig.setDiscoverable(true);
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    Set<ItemViewConfig> itemViewConfigs = itemPickupPoints.get(0).getAllItemViewConfigs().stream()
        .map(itemViewConfig -> gdnMapper.deepCopy(itemViewConfig, ItemViewConfig.class)).collect(Collectors.toSet());
    itemVo.getItemPickupPointVoList().get(0).setItemViewConfig(itemViewConfigs);
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertNotNull(items);
    assertNotNull(items.get(0).getPristineDataItem());
    assertTrue(items.get(0).isContentChanged());
    assertTrue(items.get(0).isInitialContentChanged());
    assertEquals(SOURCE_ITEM_CODE, items.get(0).getSourceItemCode());
    assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void testAddItems_whenExistingItemIsNull() throws Exception {
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS),
          eq(this.MERCHANT_CODE), eq(this.product.getDefiningAttributes().size()), any(Item.class))).thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(eq(this.STORE_ID),
        anyString(), any(Pageable.class))).thenReturn(new PageImpl<Item>(new ArrayList<>()));
    Mockito.when(objectConverterService.convertToMasterDataItems(this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class), anyString(),
        anyList())).thenReturn(this.product);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>());
    item.setItemViewConfigs(new HashSet<>());
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    b2bItemViewConfig.setBuyable(true);
    itemVo.getItemPickupPointVoList().get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig)));
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    List<ItemVo> items =
        this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME, this.PRODUCT_SKU_FOR_ADD_ITEMS,
            productItemsVo.getItemVoList(), this.product, this.productDetailResponse, businessPartner);

    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .setItemDetail(anyString(), anyString(), anyString(),
            Mockito.anyInt(), any(ItemVo.class));
    Mockito.verify(objectConverterService)
        .convertToMasterDataItems(anySet(), anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(), anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
  }

  @Test
  public void testAddItems_whenExistingItemIsNull_no_ItemPickupPoints() throws Exception {
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS),
          eq(this.MERCHANT_CODE), eq(this.product.getDefiningAttributes().size()), any(Item.class))).thenReturn(item);
    }
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(eq(this.STORE_ID),
        anyString(), any(Pageable.class))).thenReturn(new PageImpl<Item>(new ArrayList<>()));
    Mockito.when(objectConverterService.convertToMasterDataItems(this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class), anyString(),
        anyList())).thenReturn(this.product);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(new ArrayList<>());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>());
    item.setItemViewConfigs(new HashSet<>());
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(new ArrayList<>());
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    List<ItemVo> items =
        this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME, this.PRODUCT_SKU_FOR_ADD_ITEMS,
            productItemsVo.getItemVoList(), this.product, this.productDetailResponse, businessPartner);

    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .setItemDetail(anyString(), anyString(), anyString(),
            Mockito.anyInt(), any(ItemVo.class));
    Mockito.verify(objectConverterService)
        .convertToMasterDataItems(anySet(), anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(), anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    assertTrue(items.get(0).isArchived());
  }

  @Test
  public void testAddItems_WithEmptyItemRequests() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME, this.PRODUCT_SKU_FOR_ADD_ITEMS,
        new ArrayList<>(), this.product, this.productDetailResponse, businessPartner));
  }

  @Test
  public void testAddItems_WithNullItemRequests() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME, this.PRODUCT_SKU_FOR_ADD_ITEMS, null,
        this.product, this.productDetailResponse, businessPartner));
  }

  @Test
  public void testAddItems_WithNullProduct() throws Exception {
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME, this.PRODUCT_SKU_FOR_ADD_ITEMS,
        productItemsVo.getItemVoList(), null, this.productDetailResponse, businessPartner));
  }

  @Test
  public void updateItemSyncTestWithBlankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.updateItem(null, this.itemUpdated, ItemServiceImplTest.USERNAME, false, false, false));
  }

  @Test
  public void updateItemSyncTestWithNullItem() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, null, ItemServiceImplTest.USERNAME, false, false,
        false));
  }

  private static Item initializeItem(String itemSku) {
    Item item = new Item();
    item.setItemSku(itemSku);
    return item;
  }

  @Test
  public void publishAllItemsTest() throws Exception {
    when(itemRepository.streamAllByStoreId(STORE_ID)).thenReturn(ITEM_STREAM);

    itemServiceImpl.publishAllItems(STORE_ID);

    verify(itemRepository, times(1)).streamAllByStoreId(STORE_ID);
    verify(saveAndPublishService, times(1)).publishStreamOfItems(ITEM_STREAM);
  }

  @Test
  public void publishAllItemsTestThrowException() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(itemRepository).streamAllByStoreId(BLANK);

    itemServiceImpl.publishAllItems(BLANK);

    verify(itemRepository, times(1)).streamAllByStoreId(BLANK);
  }

  @Test
  public void republishItemsToAgpTestThrowException() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(itemRepository).streamAllByStoreIdAndItemSkuIn(BLANK, Collections.emptyList());

    itemServiceImpl.republishItemsToAgp(BLANK, Collections.emptyList());

    verify(itemRepository, times(1)).streamAllByStoreIdAndItemSkuIn(BLANK, Collections.emptyList());
  }

  @Test
  public void updateItemFreeSampleTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", false);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, PRODUCT_SKU, false, false,
            false))
        .thenReturn(Arrays.asList(itemUnsyncUpdated));
    itemUnsyncUpdated.setFreeSample(true);
    Mockito.when(
            productHelperService.updateItemViewConfigForExistingChannel(any(), any(ItemViewConfig.class)))
        .thenReturn(item);
    Item result = this.itemServiceImpl
        .updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated, ItemServiceImplTest.USERNAME, false, false,
            false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(any(),
        itemViewConfigArgumentCaptor.capture());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.saveOperationService).saveItem(this.itemUnSyncTobeUpdated, new ArrayList<>(), new ArrayList<>());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated), Arrays.asList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL5DetailsFromL4(listArgumentCaptor.capture(), eq(Arrays.asList(itemPickupPoint)));
  }

  @Test
  public void updateItemFreeSampleTestcnc1pswitchon() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, PRODUCT_SKU, false, false,
            false))
        .thenReturn(Arrays.asList(itemUnsyncUpdated));
    itemUnsyncUpdated.setFreeSample(true);
    Mockito.when(
            productHelperService.updateItemViewConfigForExistingChannel(Mockito.any(), Mockito.any(ItemViewConfig.class)))
        .thenReturn(item);
    Item result = this.itemServiceImpl
        .updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated, ItemServiceImplTest.USERNAME, false, false,
            false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
    verify(this.productHelperService,times(2)).updateItemViewConfigForExistingChannel(Mockito.any(),
        itemViewConfigArgumentCaptor.capture());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.saveOperationService).saveItem(this.itemUnSyncTobeUpdated, new ArrayList<>(), new ArrayList<>());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService)
        .overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated), Arrays.asList(itemPickupPoint));
    verify(objectConverterService)
        .overrideL5DetailsFromL4(listArgumentCaptor.capture(), eq(Arrays.asList(itemPickupPoint)));
  }

  @Test
  public void updatePickupPointInItemPickupPointsTest() throws Exception {
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    Mockito
        .when(productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint, item, new HashMap<>(),
        false, null, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    verify(productService).findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku());
  }

  @Test
  public void updatePickupPointInItemPickupPointsCombinedEditTest() throws Exception {
    item.setStoreId(STORE_ID);
    editProductDetailDTO.setAllItemPickupPointMap(new HashMap<>());
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint, item, new HashMap<>(),
        true, editProductDetailDTO, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
  }

  @Test
  public void updatePickupPointInItemPickupPointsMppOnTest() throws Exception {
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    Mockito
        .when(productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint, item, new HashMap<>(),
        false, null, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    verify(productService).findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku());
  }

  @Test
  public void updatePickupPointInItemPickupPointsMfdTureMppOnTest() throws Exception {
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.setMarkForDelete(true);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    Mockito
        .when(productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint, item, new HashMap<>(),
        false, null, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    verify(productService).findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku());
  }

  @Test
  public void updatePickupPointInItemPickupPointsMfdTureCombinedEditTest() throws Exception {
    item.setStoreId(STORE_ID);
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.setMarkForDelete(true);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    Map<String, ItemPickupPoint> evictItemPickupPointCacheMap =
        itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint, item, new HashMap<>(),
            true, editProductDetailDTO, false);
    assertFalse(evictItemPickupPointCacheMap.values().iterator().next().isWholesalePriceExists());
  }

  @Test
  public void updatePickupPointInItemPickupPointsMfdTrueWithCNCTrue() throws Exception {
    item.setStoreId(STORE_ID);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(false);
    itemViewConfig.setChannel(Constants.CNC);
    itemViewConfigs.add(itemViewConfig);
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.getPrice().stream().findFirst().get()
        .setListOfDiscountPrices(List.of(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(
        itemPickupPoint);
    Mockito.when(productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(),
        itemPickupPoint.getProductSku())).thenReturn(product);
    Map<String, ItemPickupPoint> evictItemPickupPointCacheMap =
        itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint,
            item, new HashMap<>(), false, editProductDetailDTO, false);
    assertFalse(evictItemPickupPointCacheMap.values().iterator().next().isWholesalePriceExists());
    Mockito.verify(productService)
        .findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku());
  }

  @Test
  public void updatePickupPointInItemPickupPointsMfdTrueWithCNCFalse() throws Exception {
    item.setStoreId(STORE_ID);
    item.setItemViewConfigs(itemViewConfigs);
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.CNC);
    itemViewConfigs.add(itemViewConfig);
    itemPickupPoint.setMarkForDelete(true);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.getPrice().stream().findFirst().get()
        .setListOfDiscountPrices(List.of(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(
        itemPickupPoint);
    Mockito.when(productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(),
        itemPickupPoint.getProductSku())).thenReturn(product);
    Map<String, ItemPickupPoint> evictItemPickupPointCacheMap =
        itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint,
            item, new HashMap<>(), false, editProductDetailDTO, false);
    assertFalse(evictItemPickupPointCacheMap.values().iterator().next().isWholesalePriceExists());
    Mockito.verify(productService)
        .findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku());
  }

  @Test
  public void updatePickupPointInItemPickupPointsNullMppOnTest() throws Exception {
    itemViewConfigs.add(new ItemViewConfig(true, true, "CNC", null, null));
    item.setItemViewConfigs(itemViewConfigs);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    itemPickupPoint.setMarkForDelete(true);
    itemPickupPoint.getPrice().stream().findFirst().get().setListOfDiscountPrices(Arrays.asList(new DiscountPrice()));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(null);
    Mockito
        .when(productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku()))
        .thenReturn(product);
    itemServiceImpl.updatePickupPointInItemPickupPoints(PICKUP_POINT_CODE, itemPickupPoint, item, new HashMap<>(),
        false, null, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
  }

  @Test
  public void updateItemWithWholesaleAlreadyExists() throws Exception {
    item.setWholesalePriceActivated(true);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE);
    item.setItemSku(ITEM_SKU_UNSYNC_TOBE_UPDATED);
    item.setProductSku(itemUnsyncUpdated.getProductSku());
    itemUnsyncUpdated.setWholesalePriceActivated(true);
    item.setItemViewConfigs(itemViewConfigs);
    Set<String> updatedFields = new HashSet<>();
    updatedFields.add(UpdatedFields.NAME_UPDATE.name());
    updatedFields.add(UpdatedFields.DESCRIPTION_UPDATE.name());
    Mockito.when(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.item,
        this.itemUnsyncUpdated.getItemViewConfigs())).thenReturn(true);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated.getProductSku());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), anySet());
    verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), any(), eq(new ArrayList<>()));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertEquals(updatedFields,result.getUpdatedFields());
  }

  @Test
  public void updateItem1pCncdiscoverablechange() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    item.setWholesalePriceActivated(true);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE);
    item.setItemSku(ITEM_SKU_UNSYNC_TOBE_UPDATED);
    item.setProductSku(itemUnsyncUpdated.getProductSku());
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    itemUnsyncUpdated.setWholesalePriceActivated(true);
    itemUnsyncUpdated.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setItemViewConfigs(itemViewConfigs);
    item.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    itemUnsyncUpdated.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    Set<String> updatedFields = new HashSet<>();
    updatedFields.add(UpdatedFields.NAME_UPDATE.name());
    updatedFields.add(UpdatedFields.DESCRIPTION_UPDATE.name());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setItemSku(item.getItemSku());
    itemPickupPointDataChangeEventModel.setPickupPointCode(item.getPickupPointCode());
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.any())).thenReturn(itemPickupPointDataChangeEventModel);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    when(itemPickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.item,
        this.itemUnsyncUpdated.getItemViewConfigs())).thenReturn(true);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated.getProductSku());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), Mockito.anySet());
    verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), Mockito.any(),
        eq(Collections.singletonList(itemPickupPointDataChangeEventModel)));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.anyList());
    verify(objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.any());
    verify(objectConverterService).overrideL4DetailsFromL5(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertEquals(updatedFields,result.getUpdatedFields());
  }

  @Test
  public void updateItem1pCncdiscoverablenochangeswitchoff() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", false);
    item.setWholesalePriceActivated(true);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE);
    item.setItemSku(ITEM_SKU_UNSYNC_TOBE_UPDATED);
    item.setProductSku(itemUnsyncUpdated.getProductSku());
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    itemUnsyncUpdated.setWholesalePriceActivated(true);
    itemUnsyncUpdated.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setItemViewConfigs(itemViewConfigs);
//    item.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.DEFAULT).isDiscoverable(false).isBuyable(false).build());
    itemUnsyncUpdated.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.DEFAULT).isDiscoverable(false).build());
    item.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    itemUnsyncUpdated.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    Set<String> updatedFields = new HashSet<>();
    updatedFields.add(UpdatedFields.NAME_UPDATE.name());
    updatedFields.add(UpdatedFields.DESCRIPTION_UPDATE.name());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setItemSku(item.getItemSku());
    itemPickupPointDataChangeEventModel.setPickupPointCode(item.getPickupPointCode());
    when(objectConverterService.convertToItemPickupPointChangeEventModel(Mockito.any(),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(itemPickupPoint);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(Mockito.any(Item.class), Mockito.any(), Mockito.anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    when(itemPickupPointService.saveItemPickupPoint(Mockito.anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.item,
        this.itemUnsyncUpdated.getItemViewConfigs())).thenReturn(true);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated.getProductSku());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), Mockito.anySet());
    verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), Mockito.any(),
        eq(Collections.singletonList(itemPickupPointDataChangeEventModel)));
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(Mockito.anyList(), Mockito.anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.anyList());
    verify(objectConverterService).convertToItemPickupPointChangeEventModel(Mockito.any(ItemPickupPoint.class),
        Mockito.anyBoolean());
    verify(objectConverterService).overrideL4DetailsFromL5(listArgumentCaptor.capture(),
        Mockito.eq(Arrays.asList(itemPickupPoint)));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        Mockito.eq(Arrays.asList(itemPickupPoint)));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    assertTrue(itemArgumentCaptor.getValue().getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    assertEquals(updatedFields,result.getUpdatedFields());
  }

  @Test
  public void updateItemWithWholesalePriceAlreadyExists() throws Exception {
    item.setWholesalePriceActivated(true);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setItemSku(ITEM_SKU_UNSYNC_TOBE_UPDATED);
    item.setProductSku(itemUnsyncUpdated.getProductSku());
    itemUnsyncUpdated.setWholesalePriceActivated(true);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(this.itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.item,
        this.itemUnsyncUpdated.getItemViewConfigs())).thenReturn(false);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated.getProductSku());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    verify(objectConverterService).overrideL4DetailsFromL5(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
  }

  @Test
  public void updateItemWithWholesaleDeactivation() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    item.setWholesalePriceActivated(true);
    item.setActivePromoBundlings(new HashSet<>());
    item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setItemSku(ITEM_SKU_UNSYNC_TOBE_UPDATED);
    item.setProductSku(itemUnsyncUpdated.getProductSku());
    itemUnsyncUpdated.setWholesalePriceActivated(false);
    itemUnsyncUpdated.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(true).build());
    itemUnsyncUpdated.getItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    Mockito.when(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, item.getItemSku(), false))
        .thenReturn(item);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    itemPickupPointDataChangeEventModel.setItemSku(item.getItemSku());
    itemPickupPointDataChangeEventModel.setPickupPointCode(item.getPickupPointCode());
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(),
        Mockito.any())).thenReturn(itemPickupPointDataChangeEventModel);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    item.setItemViewConfigs(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, itemUnsyncUpdated.getItemSku()))
        .thenReturn(itemPickupPoint);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    when(itemPickupPointService.saveItemPickupPoint(anyList())).thenReturn(
        Collections.singletonList(itemPickupPoint));
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.item,
        this.itemUnsyncUpdated.getItemViewConfigs())).thenReturn(true);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated.getProductSku());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(itemArgumentCaptor.capture(), anySet());
    verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), any(),
        eq(Collections.singletonList(itemPickupPointDataChangeEventModel)));
    verify(objectConverterService).convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.any());
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    assertEquals(0, itemArgumentCaptor.getValue().getActivePromoBundlings().size());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(item),
        Arrays.asList(itemPickupPoint));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
  }

  @Test
  public void updateItemTest_noShippingDetailChange() throws Exception {
    itemUnsyncUpdated.getMasterDataItem().setItemHeight(HEIGHT);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(true);
    Item result =
        this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, itemUnsyncUpdated, ItemServiceImplTest.USERNAME,
            false, false, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
    verify(this.productHelperService).updateItemViewConfigForExistingChannel(this.itemUnSyncTobeUpdated, this.itemUnSyncTobeUpdated.getItemViewConfigs());
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    verify(this.saveOperationService).saveItem(this.itemUnSyncTobeUpdated, new ArrayList<>(), new ArrayList<>());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
        Arrays.asList(itemPickupPoint));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
  }

  @Test
  public void updateItem_whenItemPriceChangeTrueAndMerchantPromoTrueTest() throws Exception {
    itemUnSyncTobeUpdated.setPrice(Sets.newHashSet(price1));
    itemUnSyncTobeUpdated.setMerchantPromoDiscount(true);
    itemUnsyncUpdated.setPrice(Sets.newHashSet(price));
    itemUnSyncTobeUpdated.setPickupPointCode(PICKUP_POINT_CODE);
    itemUnsyncUpdated.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(true);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
          ItemServiceImplTest.USERNAME, false, false, false));
    } finally {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
          ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
      Mockito.verify(this.productService)
          .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
      verify(this.productHelperService).updateItemViewConfigForExistingChannel(this.itemUnSyncTobeUpdated, this.itemUnSyncTobeUpdated.getItemViewConfigs());
      verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
      Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
      verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
          Arrays.asList(itemPickupPoint));
    }
  }

  @Test
  public void updateItemTest_WithNoChangeUnsyncItem() throws Exception {
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(false);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
    Mockito.verify(this.itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
            this.itemUnSyncTobeUpdated.getItemViewConfigs());
    verify(this.saveOperationService).saveItem(this.itemUnSyncTobeUpdated, new ArrayList<>(), new ArrayList<>());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
        Arrays.asList(itemPickupPoint));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
  }

  @Test
  public void updateItemTest_WithNoChangeSyncItem() throws Exception {
    this.itemSyncTobeUpdated.setMasterDataItem(masterDataItemUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, this.itemSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemSyncTobeUpdated,
        this.itemSyncTobeUpdated.getItemViewConfigs())).thenReturn(false);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemSyncTobeUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemSyncTobeUpdated.getProductSku());
    Mockito.verify(this.itemViewConfigService).isItemViewConfigChangeForExistingChannelChange(this.itemSyncTobeUpdated,
        this.itemSyncTobeUpdated.getItemViewConfigs());
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_SYNC_TOBE_UPDATED);
    verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemSyncTobeUpdated),
        Arrays.asList(itemPickupPoint));
  }

  @Test
  public void updateItemTestWithItemNotFound() {
    try {
      this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID_NOT_FOUND, this.itemUpdated,
          ItemServiceImplTest.USERNAME, false, false, false);

    } catch (Exception e) {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(
          ItemServiceImplTest.STORE_ID_NOT_FOUND, ItemServiceImplTest.ITEM_SKU_SYNC_TOBE_UPDATED, false);
      Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID_NOT_FOUND, ITEM_SKU_SYNC_TOBE_UPDATED);
      assertTrue(e instanceof Exception);
    }
  }

  @Test
  public void toggleArchiveItemWithExceptionTest() {
    item.setItemSku(ITEM_SKU);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndItemSku(STORE_ID,ITEM_SKU)).thenReturn(null);
    try {
      this.itemServiceImpl.toggleArchiveItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
          ItemServiceImplTest.USERNAME, !ItemServiceImplTest.DO_ARCHIVE_TRUE);
    } catch (Exception e) {
      Mockito.verify(this.cacheItemHelperService)
          .findCacheableByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU);
      assertTrue(e.getMessage().contains(ITEM_NOT_FOUND + ITEM_SKU));
    }
  }

  @Test
  public void publishItemSkusTest_whenIdIsProductCode() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "masterDataChangeSolrReindexEnabled", true);
    productDomainEventModel.setPristineCategory(true);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    Mockito.verify(this.saveOperationService).saveItems(anyList(), any());
  }

  @Test
  public void publishItemSkusTest_whenPristineCategoryNull() throws Exception {
    productDomainEventModel.setPristineCategory(null);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    Mockito.verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture(), anyList(),
        anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH),anyBoolean(), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.cacheEvictHelperService).evictItemData(anyString(), any(Item.class));
  }

  @Test
  public void publishItemSkusTestWithNullPristineDataItem() throws Exception {
    productDomainEventModel.setPristineCategory(true);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    listOfItems.get(0).setPristineDataItem(null);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    Mockito.verify(this.saveOperationService).saveItemsWithoutUpdatingSolr(anyList());
  }

  @Test
  public void publishItemSkusTestWithNullPristineDataItemAndPristineCategoryFalse() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    listOfItems.get(0).setPristineDataItem(null);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    Mockito.verify(cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(anyList(), anyList(), anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH),anyBoolean(), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void archiveAndDeleteActiveProductTest() {
    product.setSuspended(true);
    product.setForceReview(true);
    item.setForceReview(true);
    this.item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
        PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.doNothing().when(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class))).thenReturn(new ProductAndItemEventModel());
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(),
        eq(Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED)), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
Mockito.verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
            PageRequest.of(0, 500));
    Mockito.verify(itemPickupPointService).updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,Boolean.TRUE, item, Arrays.asList(itemPickupPoint), true);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(itemPickupPointService).deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
    assertTrue(itemArgumentCaptor.getValue().isArchived());
    assertTrue(itemArgumentCaptor.getValue().isMarkForDelete());
    assertFalse(productArgumentCaptor.getValue().isForceReview());
    assertFalse(productArgumentCaptor.getValue().isSuspended());
    assertFalse(itemArgumentCaptor.getValue().isForceReview());
  }

  @Test
  public void archiveAndDeleteActiveProductMppONTest() {
    product.setSuspended(true);
    product.setForceReview(true);
    item.setForceReview(true);
    this.item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
        PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.doNothing().when(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class))).thenReturn(new ProductAndItemEventModel());
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.saveOperationService).saveProductWithoutUpdatingSolr(productArgumentCaptor.capture(),
        eq(Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED)), eq(StringUtils.EMPTY), eq(Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
Mockito.verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
            PageRequest.of(0, 500));
    Mockito.verify(itemPickupPointService).updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME,Boolean.TRUE, item, Arrays.asList(itemPickupPoint), true);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(itemPickupPointService).deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
    assertTrue(itemArgumentCaptor.getValue().isArchived());
    assertTrue(itemArgumentCaptor.getValue().isMarkForDelete());
    assertFalse(productArgumentCaptor.getValue().isForceReview());
    assertFalse(productArgumentCaptor.getValue().isSuspended());
    assertFalse(itemArgumentCaptor.getValue().isForceReview());
  }


  @Test
  public void archiveAndDeleteActiveProductAlreadyArchivedProductTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "isDeleteL3AndL4Enabled", true);
    this.item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setArchived(true);
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
        PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.doNothing().when(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class))).thenReturn(new ProductAndItemEventModel());
    Mockito.doNothing().when(itemPickupPointService).deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.saveOperationService).saveProductWithoutUpdatingSolr(product,
        Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED), StringUtils.EMPTY, Collections.EMPTY_MAP);
Mockito.verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
            PageRequest.of(0, 500));
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(itemPickupPointService).deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
    assertTrue(itemArgumentCaptor.getValue().isArchived());
    assertTrue(itemArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void archiveAndDeleteActiveProductItemsEmptyTest() {
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
        PageRequest.of(0, 500))).thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0,
        500), 0));
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.saveOperationService)
        .saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED),
            StringUtils.EMPTY, Collections.EMPTY_MAP);
  }

  @Test
  public void archiveAndDeleteActiveProductEmptyTest() {
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(new ArrayList<>());
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void archiveAndDeleteActiveProductorArchivedItemTest() {
    item.setArchived(true);
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
        PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.doNothing().when(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.when(objectConverterService.convertToProductAndItemEventModel(any(ProductAndItemsVO.class))).thenReturn(new ProductAndItemEventModel());
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.saveOperationService)
        .saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED),
            StringUtils.EMPTY, Collections.EMPTY_MAP);
Mockito.verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
            PageRequest.of(0, 500));
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(objectConverterService).convertToProductAndItemEventModel(any(ProductAndItemsVO.class));
    Mockito.verify(itemPickupPointService).deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
    assertTrue(itemArgumentCaptor.getValue().isArchived());
    assertTrue(itemArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void archiveAndDeleteActiveProductorArchivedItemDifferentProductSkuTest() {
    item.setArchived(true);
    item.setProductSku(PRODUCT_SKU_FOR_ADD_ITEMS);
    Mockito.when(this.productService.findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
        PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    Mockito.doNothing().when(this.productAndItemSolrIndexerService)
        .deleteProductsFromSolrAfterPostLiveRejection(Collections.singleton(PRODUCT_SKU));
    Mockito.doNothing().when(saveAndPublishService).publishSolrUpdateEvent(anyList());
    itemServiceImpl.archiveAndDeleteActiveProduct(productDomainEventModel, Collections.EMPTY_MAP);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(this.saveOperationService)
        .saveProductWithoutUpdatingSolr(product, Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED),
            StringUtils.EMPTY, Collections.EMPTY_MAP);
Mockito.verify(this.saveOperationService).saveItemWithoutUpdatingSolr(itemArgumentCaptor.capture(), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU),
            PageRequest.of(0, 500));
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveAndPublishService).publishSolrUpdateEvent(anyList());
    Mockito.verify(itemPickupPointService).deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
    assertTrue(itemArgumentCaptor.getValue().isArchived());
    assertTrue(itemArgumentCaptor.getValue().isMarkForDelete());
  }

  @Test
  public void publishItemSkusTest_whenIdIsProductSku() throws Exception {
    productDomainEventModel.setProductCode(PRODUCT_SKU);
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    Mockito.verify(cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(saveAndPublishService)
        .publishListOfItems(anyList(), anyList(), anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH),anyBoolean(), eq(Collections.EMPTY_MAP));
  }

  private PristineDataItem createPristineItem(List<Item> currentItems, List<Product> productList, String pristineCategory) {
    PristineDataItem pristineItem = new PristineDataItem();
    pristineItem.setId("id");
    pristineItem.setPristineId(PRISTINE_ID);
    pristineItem.setPcbProductItemId(ITEM_CODE);
    pristineItem.setPristineBrand("BRAND");
    pristineItem.setPristineCategory(pristineCategory);
    pristineItem.setPristineProductName(PRISTINE_PRODUCT_NAME);
    pristineItem.setPristineMasterId("PRISTINE_MASTER_ID");
    pristineItem.setDefaultProductCode("DEFAULT_PRODUCT_CODE");
    List<ItemCatalogVO> itemCatalogVOS = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    List<ItemCategoryVO> itemCategories = new ArrayList<>();
    ItemCategoryVO itemCategoryVO = new ItemCategoryVO();
    itemCategoryVO.setCategoryId("AN-1000001");
    itemCategories.add(itemCategoryVO);
    ItemCategoryVO itemCategoryVO1 = new ItemCategoryVO();
    itemCategoryVO1.setCategoryId("HA-1000002");
    itemCategories.add(itemCategoryVO1);
    ItemCategoryVO itemCategoryVO2 = new ItemCategoryVO();
    itemCategoryVO2.setCategoryId("AN-1000001");
    itemCategories.add(itemCategoryVO2);
    itemCatalogVO.setItemCategories(itemCategories);
    itemCatalogVOS.add(itemCatalogVO);
    pristineItem.setPristineCategoriesHierarchy(itemCatalogVOS);
    List<SalesCategorySequence> salesCategorySequences = new ArrayList<>();
    pristineItem.setSalesCategorySequences(salesCategorySequences);
    pristineItem.setPristineListingAttributes(new HashMap<String, String>());
    ReflectionTestUtils.setField(itemServiceImpl, "storeId", "10001");
    ReflectionTestUtils.setField(itemServiceImpl, "username", "username");
    ReflectionTestUtils.setField(itemServiceImpl, "requestId", "requestId");
    Item item = new Item();
    item.setItemCode("PRODUCT_ITEMCODE");
    item.setProductSku(PRODUCT_SKU);
    item.setPristineDataItem(pristineItem);
    item.setStoreId(STORE_ID);
    currentItems.add(item);

    Product product = new Product();
    product.setProductSku(PRODUCT_SKU);
    SalesCatalog salesCatalog = new SalesCatalog();
    Category category = new Category("CATEGORY_CODE", "CATEGORY_GRP_ID");
    List<SalesCatalog> salesCatalogList = new ArrayList<>();
    List<Category> categoryList = new ArrayList<>();
    categoryList.add(category);
    salesCatalog.setListOfCategories(categoryList);
    salesCatalogList.add(salesCatalog);
    product.setSalesCatalogs(salesCatalogList);
    productList.add(product);
    return pristineItem;
  }

  private List<ItemCatalogVO> createItemCategoryVos() {
    List<ItemCatalogVO> itemCatalogVOS = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    List<ItemCategoryVO> itemCategories = new ArrayList<>();
    ItemCategoryVO itemCategoryVO = new ItemCategoryVO();
    itemCategoryVO.setCategoryId("AN-1000001");
    itemCategories.add(itemCategoryVO);
    ItemCategoryVO itemCategoryVO1 = new ItemCategoryVO();
    itemCategoryVO1.setCategoryId("HA-1000002");
    itemCategories.add(itemCategoryVO1);
    ItemCategoryVO itemCategoryVO2 = new ItemCategoryVO();
    itemCategoryVO2.setCategoryId("AN-1000002");
    itemCategories.add(itemCategoryVO2);
    itemCatalogVO.setItemCategories(itemCategories);
    itemCatalogVOS.add(itemCatalogVO);
    return itemCatalogVOS;
  }

  @Test
  public void updateItemsPristineDataByItemCodeTest() throws Exception {

    List<Item> currentItems = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    List<PristineDataItem> pristineItems = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, "HANDPHONE");
    pristineItems.add(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString())).thenReturn(currentItems);
    Mockito.when(this.pristineItemRepository.findByPristineId(anyString())).thenReturn(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItem)).thenReturn(currentItems);
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItems)).thenReturn(currentItems);
    Mockito.when(this.productService.getProducts(eq("10001"), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(createItemCatalogList());
    Mockito.when(this.saveAndPublishService.saveItems(eq(currentItems))).thenReturn(currentItems);
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString());
    Mockito.verify(this.pristineItemRepository, Mockito.times(1)).findByPristineId(anyString());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItem);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(itemRepository).findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItems);
    Mockito.verify(this.productService).getProducts(eq("10001"), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    for (Item currentItem : currentItems) {
      Mockito.verify(this.saveAndPublishService).publishPristineItem(eq(currentItem.getStoreId()), eq(currentItem));
      Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), eq(currentItem));
    }
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(cacheEvictHelperService).evictItemData(currentItems.get(0).getStoreId(), currentItems.get(0));
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    assertEquals(currentItems.get(0).getPristineDataItem(), pristineItem);
  }

  @Test
  public void updateItemsPristineDataByItemCodeTest_2() throws Exception {
    List<Item> currentItems = new ArrayList<>();
    List<Item> changed = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    List<PristineDataItem> pristineItems = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, "HANDPHONE");
    createPristineItem(changed, productList, "HANDPHONE");
    changed.get(0).setItemSku(ITEM_SKU);
    pristineItems.add(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString())).thenReturn(currentItems);
    Mockito.when(this.pristineItemRepository.findByPristineId(anyString())).thenReturn(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItem)).thenReturn(changed);
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItems)).thenReturn(currentItems);
    Mockito.when(this.productService.getProducts(eq("10001"), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(createItemCatalogList());
    Mockito.when(this.saveAndPublishService.saveItems(eq(currentItems))).thenReturn(currentItems);
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString());
    Mockito.verify(this.pristineItemRepository, Mockito.times(1)).findByPristineId(anyString());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItem);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(itemRepository)
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItems);
    Mockito.verify(this.productService).getProducts(eq("10001"), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    for (Item currentItem : changed) {
      Mockito.verify(this.saveAndPublishService).publishPristineItem(eq(currentItem.getStoreId()), eq(currentItem));
      Mockito.verify(cacheEvictHelperService).evictItemData(currentItems.get(0).getStoreId(), currentItem);
    }
    for (Item currentItem : currentItems) {
      Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), eq(currentItem));
    }
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(changed);
    assertEquals(currentItems.get(0).getPristineDataItem(), pristineItem);
  }

  @Test
  public void updateItemsPristineDataByItemCodeForCameraCategoryTest() throws Exception {

    List<Item> currentItems = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    List<PristineDataItem> pristineItems = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, PristineCategory.CAMERA.name());
    pristineItems.add(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString())).thenReturn(currentItems);
    Mockito.when(this.pristineItemRepository.findByPristineId(anyString())).thenReturn(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItem)).thenReturn(currentItems);
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItems)).thenReturn(currentItems);
    Mockito.when(this.productService.getProducts(eq("10001"), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(new ArrayList());
    Mockito.when(this.saveAndPublishService.saveItems(eq(currentItems))).thenReturn(currentItems);
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString());
    Mockito.verify(this.pristineItemRepository, Mockito.times(1)).findByPristineId(anyString());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItem);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(itemRepository)
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItems);
    Mockito.verify(this.productService).getProducts(eq("10001"), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    for (Item currentItem : currentItems) {
      Mockito.verify(this.saveAndPublishService).publishPristineItem(eq(currentItem.getStoreId()), eq(currentItem));
      Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), eq(currentItem));
    }
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(cacheEvictHelperService).evictItemData(currentItems.get(0).getStoreId(), currentItems.get(0));
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    assertEquals(currentItems.get(0).getPristineDataItem(), pristineItem);
  }

  @Test
  public void updateItemsPristineDataByItemSkuTest() throws Exception {
    List<Product> productList = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, "HANDPHONE");
    List<PristineDataItem> pristineItems = new ArrayList<>();
    pristineItem.setPcbProductItemId(ITEM_SKU);
    pristineItems.add(pristineItem);
    Map<String, String> listineAttributes = new HashMap<>();
    when(this.skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq("10001"), anyString(), eq(false)))
        .thenReturn(currentItems.get(0));
    Mockito.when(this.pristineItemRepository.findByPristineId(anyString())).thenReturn(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItem)).thenReturn(currentItems);
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItems)).thenReturn(currentItems);
    Mockito.when(this.productService.getProducts(eq("10001"), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(new ArrayList());
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    Mockito.when(this.saveAndPublishService.saveItems(eq(currentItems))).thenReturn(currentItems);
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq("10001"), anyString(), eq(false));
    Mockito.verify(this.pristineItemRepository, Mockito.times(1)).findByPristineId(anyString());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItem);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(itemRepository)
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItems);
    Mockito.verify(this.productService).getProducts(eq("10001"), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    for (Item currentItem : currentItems) {
      Mockito.verify(this.saveAndPublishService).publishPristineItem(eq(currentItem.getStoreId()), eq(currentItem));
      Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), eq(currentItem));
    }
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(skuValidator).isItemSku(eq(ITEM_SKU));
    Mockito.verify(cacheEvictHelperService).evictItemData(currentItems.get(0).getStoreId(), currentItems.get(0));
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    assertEquals(currentItems.get(0).getPristineDataItem(), pristineItem);
  }

  @Test
  public void updateItemsPristineDataByItemCode_WhenPristineIdNullTest() throws Exception {

    List<Item> currentItems = new ArrayList<>();
    List<Item> existingPristineItems = new ArrayList<>();
    List<Item> remainingPristineItems = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    List<PristineDataItem> pristineItems = new ArrayList<>();

    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, PristineCategory.HANDPHONE.name());
    PristineDataItem pristineDataItem =
        createPristineItem(existingPristineItems, productList, PristineCategory.HANDPHONE.name());
    PristineDataItem pristineDataItem1 =
        createPristineItem(remainingPristineItems, productList, PristineCategory.HANDPHONE.name());
    remainingPristineItems.get(0).setItemSku(ITEM_SKU);
    pristineItems.add(pristineDataItem1);
    pristineItem.setPristineId(null);
    pristineItem.setPcbProductItemId(ITEM_SKU);
    pristineDataItem1.setPcbProductItemId(ITEM_SKU);
    Mockito.when(this.skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq("10001"), anyString(),  eq(false)))
        .thenReturn(existingPristineItems.get(0));
    Mockito.when(this.pristineItemRepository.findByPristineId(pristineDataItem.getPristineId()))
        .thenReturn(pristineDataItem);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineDataItem)).thenReturn(remainingPristineItems);
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItems)).thenReturn(remainingPristineItems);
    Mockito.when(this.productService.getProducts(eq("10001"), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(createItemCatalogList());
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq("10001"), anyString(),  eq(false));
    Mockito.verify(this.pristineItemRepository).findByPristineId(pristineItem.getPristineId());
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItem);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(itemRepository)
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItems);
    Mockito.verify(this.productService).getProducts(eq("10001"), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), any(Item.class));
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(skuValidator, times(2)).isItemSku(eq(ITEM_SKU));
    Mockito.verify(this.saveAndPublishService, times(2)).publishPristineItem(eq("10001"), any(Item.class));
    Mockito.verify(cacheEvictHelperService, times(2)).evictItemData(eq("10001"), any(Item.class));
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(anyList());
    assertEquals(existingPristineItems.get(0).getPristineDataItem(), null);
  }

  @Test
  public void updateItemsPristineDataByItemCode_WhenPristineIdNullAndDefaultProductCodeNotEqualTest() throws Exception {
    List<Item> currentItems = new ArrayList<>();
    List<Item> existingPristineItems = new ArrayList<>();
    List<Item> remainingPristineItems = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    List<PristineDataItem> pristineItems = new ArrayList<>();
    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, PristineCategory.HANDPHONE.name());
    PristineDataItem pristineDataItem =
        createPristineItem(existingPristineItems, productList, PristineCategory.HANDPHONE.name());
    pristineDataItem.setDefaultProductCode("DEFAULT_PRODUCT_CODE1");
    PristineDataItem pristineDataItem1 =
        createPristineItem(remainingPristineItems, productList, PristineCategory.HANDPHONE.name());
    remainingPristineItems.get(0).setItemSku(ITEM_SKU);
    pristineItems.add(pristineDataItem1);
    pristineItem.setPristineId(null);
    pristineItem.setPcbProductItemId(ITEM_SKU);
    pristineDataItem1.setPcbProductItemId(ITEM_SKU);
    Mockito.when(this.skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq("10001"), anyString(),  eq(false)))
        .thenReturn(existingPristineItems.get(0));
    Mockito.when(this.pristineItemRepository.findByPristineMasterId(pristineDataItem.getPristineMasterId()))
        .thenReturn(Arrays.asList(pristineDataItem));
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineDataItem)).thenReturn(remainingPristineItems);
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001",
        pristineItems)).thenReturn(remainingPristineItems);
    Mockito.when(this.productService.getProducts(eq("10001"), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(createItemCategoryVos());
    Mockito.when(this.saveAndPublishService.saveItems(eq(currentItems))).thenReturn(currentItems);
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq("10001"), anyString(), eq(false));
    Mockito.verify(this.pristineItemRepository).findByPristineMasterId(pristineDataItem.getPristineMasterId());
    Mockito.verify(this.itemRepository, Mockito.times(2))
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse("10001", pristineItems);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(this.productService).getProducts(eq("10001"), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), any(Item.class));
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(skuValidator, times(2)).isItemSku(eq(ITEM_SKU));
    Mockito.verify(this.saveAndPublishService, times(2)).publishPristineItem(eq("10001"), any(Item.class));
    Mockito.verify(cacheEvictHelperService, times(2)).evictItemData(eq("10001"), any(Item.class));
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(anyList());
    assertEquals(existingPristineItems.get(0).getPristineDataItem(), null);
  }

  @Test
  public void updateItemsPristineDataByItemCode_WhenPristineIdNullAndDefaultProductCodeNullTest() throws Exception {
    List<Item> currentItems = new ArrayList<>();
    List<Item> existingPristineItems = new ArrayList<>();
    List<Item> remainingPristineItems = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    List<PristineDataItem> pristineItems = new ArrayList<>();

    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, PristineCategory.HANDPHONE.name());
    pristineItem.setDefaultProductCode(null);
    PristineDataItem pristineDataItem =
        createPristineItem(existingPristineItems, productList, PristineCategory.HANDPHONE.name());
    pristineDataItem.setDefaultProductCode(StringUtils.EMPTY);
    PristineDataItem pristineDataItem1 =
        createPristineItem(remainingPristineItems, productList, PristineCategory.HANDPHONE.name());
    remainingPristineItems.get(0).setItemSku(ITEM_SKU);
    pristineItems.add(pristineDataItem1);
    pristineItem.setPristineId(null);
    pristineItem.setPcbProductItemId(ITEM_SKU);
    pristineDataItem1.setPcbProductItemId(ITEM_SKU);
    Mockito.when(this.skuValidator.isItemSku(ITEM_SKU)).thenReturn(true);
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID), anyString(),  eq(false)))
        .thenReturn(existingPristineItems.get(0));
    Mockito.when(this.pristineItemRepository.findByPristineMasterId(pristineDataItem.getPristineMasterId()))
        .thenReturn(Arrays.asList(pristineDataItem));
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(pristineItems);
    Mockito.when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        pristineItems)).thenReturn(remainingPristineItems);
    Mockito.when(this.productService.getProducts(eq(STORE_ID), anySet())).thenReturn(productList);
    Mockito.when(this.catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(anyString(), anyString(),
        anyList())).thenReturn(new ArrayList());
    Mockito.when(this.saveAndPublishService.saveItems(eq(currentItems))).thenReturn(currentItems);
    Mockito.doNothing().when(cacheEvictItemService).evictFindItemSkusByPristineId(anyString(), eq(PRISTINE_ID));
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID), anyString(),  eq(false));
    Mockito.verify(this.pristineItemRepository).findByPristineMasterId(pristineDataItem.getPristineMasterId());
    Mockito.verify(this.itemRepository, Mockito.times(2))
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, pristineItems);
    Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
    Mockito.verify(this.productService).getProducts(eq(STORE_ID), anySet());
    Mockito.verify(this.catalogService)
        .getItemCatalogsWithCategoryHierarchyExistsInPCB(eq(USERNAME), anyString(), anyList());
    Mockito.verify(this.saveAndPublishService, times(2)).publishPristineItem(eq("10001"), any(Item.class));
    Mockito.verify(itemRepository).updatePristineDataItem(eq("10001"), any(Item.class));
    Mockito.verify(pristineItemRepository).updateSalesCategorySequencesAndDPC(anySet());
    Mockito.verify(cacheEvictHelperService, times(2)).evictItemData(eq("10001"), any(Item.class));
    Mockito.verify(skuValidator, times(2)).isItemSku(eq(ITEM_SKU));
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(anyList());
    assertEquals(existingPristineItems.get(0).getPristineDataItem(), null);
  }

  @Test
  public void updateItemsPristineDataByItemCode_WhenMasterIdNullAndCurrentItemsNullTest() throws Exception {

    List<Item> currentItems = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Set<String> pristineIdSet = new HashSet<>();
    pristineIdSet.add(PRISTINE_ID);
    PristineDataItem pristineItem = createPristineItem(currentItems, productList, PristineCategory.HANDPHONE.name());
    pristineItem.setPristineMasterId(null);
    pristineItem.setPcbProductItemId(ITEM_SKU);
    Mockito.when(this.skuValidator.isItemSku(ITEM_SKU)).thenReturn(false);
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq(STORE_ID), anyString()))
        .thenReturn(new ArrayList<Item>());
    this.itemServiceImpl.updateItemsPristineDataByItemCode(pristineItem);
    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(eq("10001"), anyString());
    Mockito.verify(skuValidator).isItemSku(eq(ITEM_SKU));
  }

  @Test
  public void getItemsByPristineIds_WhenStoreIdNullTest() throws Exception {

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByPristineIds(null, new HashSet<>()));
  }

  @Test
  public void getItemsByPristineIds_WhenPristineIdsNullTest() throws Exception {

    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByPristineIds(STORE_ID, null));
  }

  @Test
  public void getItemsByPristineIdsTest() throws Exception {

    Set<String> pristineIds = new HashSet();
    pristineIds.add(PRISTINE_ID);
    PristineDataItem pristineItem = new PristineDataItem();
    List<PristineDataItem> pristineItemList = new ArrayList<>();
    pristineItemList.add(pristineItem);
    Mockito.when(pristineItemRepository.findByPristineIdIn(pristineIds)).thenReturn(pristineItemList);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        eq(pristineItemList))).thenReturn(listOfItems);
    List<Item> result = this.itemServiceImpl.getItemsByPristineIds(STORE_ID, pristineIds);
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), eq(pristineItemList));
    verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    assertEquals(result, listOfItems);

  }

  @Test
  public void getAllItemsByPristineIdsTest() throws Exception {
    Set<String> pristineIds = new HashSet();
    pristineIds.add(PRISTINE_ID);
    PristineDataItem pristineItem = new PristineDataItem();
    List<PristineDataItem> pristineItemList = new ArrayList<>();
    pristineItemList.add(pristineItem);
    Mockito.when(pristineItemRepository.findByPristineIdIn(pristineIds)).thenReturn(pristineItemList);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemIn(eq(STORE_ID), eq(pristineItemList)))
        .thenReturn(listOfItems);
    List<Item> result = this.itemServiceImpl.getAllItemsByPristineIds(STORE_ID, pristineIds);
    Mockito.verify(this.itemRepository).findByStoreIdAndPristineDataItemIn(eq(STORE_ID), eq(pristineItemList));
    verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    assertEquals(result, listOfItems);
  }

  @Test
  public void getAllItemsByPristineIds_WhenPristineIdsNullTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getAllItemsByPristineIds(STORE_ID, null));
  }

  @Test
  public void getItemsByProductSkusTest() throws Exception {
    Set<String> productSkus = new HashSet();
    productSkus.add(PRODUCT_SKU);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        eq(productSkus))).thenReturn(listOfItems);
    List<Item> result = this.itemServiceImpl.getItemsByProductSkus(STORE_ID, productSkus);
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), eq(productSkus));
    assertEquals(result, listOfItems);
  }

  @Test
  public void getItemsByProductSkus_WhenStoreIdNullTest() throws Exception {
    Set<String> productSkus = new HashSet();
    productSkus.add(PRODUCT_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByProductSkus(null, productSkus));
  }

  @Test
  public void getItemsByProductSkus_WhenProductSkusNullTest() throws Exception {
    Set<String> productSkus = new HashSet();
    productSkus.add(PRODUCT_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByProductSkus(STORE_ID, null));

  }

  @Test
  public void updateSalesCatalogForAllPristineProduct_whenExpectedExceptionTest() throws Exception {
    Mockito.when(pristineItemRepository.getAllPristineIds()).thenThrow(new ApplicationRuntimeException());
    boolean result = itemServiceImpl.updateSalesCatalogForPristineProducts(Collections.emptyList());
    Mockito.verify(pristineItemRepository).getAllPristineIds();
    assertFalse(result);
  }

  @Test
  public void getItemsWithDiscountPriceByPristineMasterIdTest() {
    this.pristineMasterIds = new HashSet<>();
    this.pristineMasterIds.add("PRISTINE_MASTER_ID");
    Mockito.when(pristineItemRepository.findByPristineMasterIdIn(this.pristineMasterIds))
        .thenReturn(new ArrayList<PristineDataItem>());
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(eq(ItemServiceImplTest.STORE_ID), anyList())).thenReturn(this.listOfItems);
    List<Item> result = this.itemServiceImpl.getItemsWithDiscountPriceByPristineMasterIds(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, this.pristineMasterIds);
    Mockito.verify(pristineItemRepository).findByPristineMasterIdIn(this.pristineMasterIds);
    Mockito.verify(this.itemRepository).findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(eq(ItemServiceImplTest.STORE_ID),
        anyList());
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    assertNotNull(result);
  }

  @Test
  public void getItemsWithDiscountPriceByPristineMasterIdWithBlankStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsWithDiscountPriceByPristineMasterIds(null, ItemServiceImplTest.USERNAME,
        ItemServiceImplTest.REQUEST_ID, this.pristineMasterIds));
  }

  @Test
  public void getItemsWithDiscountPriceByPristineMasterIdWithNullListOfPristineMasterId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsWithDiscountPriceByPristineMasterIds(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, null));
  }

  private List<ItemCatalogVO> createItemCatalogList() {
    List<ItemCatalogVO> itemCatalogVOs = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    List<ItemCategoryVO> itemCategories = new ArrayList<>();
    ItemCategoryVO itemCategoryVO = new ItemCategoryVO();
    itemCategoryVO.setCategoryId(CATEGORY_ID);
    itemCategoryVO.setProductCategoryCode(PRODUCT_CATEGORY_CODE);
    itemCategoryVO.setCategory(CATEGORY_NAME);
    itemCategoryVO.setLevel(LEVEL);
    itemCategories.add(itemCategoryVO);
    itemCatalogVO.setCatalogId(CATALOG_ID);
    itemCatalogVO.setItemCategories(itemCategories);
    itemCatalogVOs.add(itemCatalogVO);
    return itemCatalogVOs;
  }

  @Test
  public void getSalesCategorySequenceDtoListFromCategoryHierarchyTest_SuccessOK() throws Exception {
    List<SalesCategorySequence> salesCategorySequences;
    List<ItemCatalogVO> itemCatalogVOs = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    itemCatalogVO.setItemCategories(itemCategories);
    itemCatalogVOs.add(itemCatalogVO);
    salesCategorySequences = this.itemServiceImpl.getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogVOs);
    assertNotNull(salesCategorySequences);
    assertEquals(salesCategorySequences.get(0).getCategoryCode(), PRODUCT_CATEGORY_CODE);
    assertEquals(salesCategorySequences.get(0).getSequence(), 4);
    assertEquals(salesCategorySequences.get(1).getCategoryCode(), PRODUCT_CATEGORY_CODE_TWO);
    assertEquals(salesCategorySequences.get(1).getSequence(), 3);
  }

  @Test
  public void getSalesCategorySequenceDtoListFromCategoryHierarchy_WhenItemCatalogVOsEmptyTest() throws Exception {
    List<SalesCategorySequence> salesCategorySequences;
    List<ItemCatalogVO> itemCatalogVOs = new ArrayList<>();
    ItemCatalogVO itemCatalogVO = new ItemCatalogVO();
    List<ItemCategoryVO> itemCategories = new ArrayList<>();
    itemCategories.add(new ItemCategoryVO());
    itemCatalogVO.setItemCategories(itemCategories);
    itemCatalogVOs.add(itemCatalogVO);
    salesCategorySequences = this.itemServiceImpl.getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogVOs);
    assertNotNull(salesCategorySequences);
    assertEquals(salesCategorySequences.get(0).getCategoryCode(), null);
  }

  @Test
  public void updateItemsPristineDataWithHandPhoneTest() throws Exception {
    Map<String, PristineDataItem> itemIdToPristineDataMap = new HashMap();
    PristineDataItem pristineItem = new PristineDataItem();
    pristineItem.setPristineId("PRISTINE_ID");
    pristineItem.setPristineBrand("BRAND");
    pristineItem.setPristineCategory("HANDPHONE");
    pristineItem.setPristineProductName("PRODUCT_NAME");
    pristineItem.setProductCondition("Refurbish");
    HashMap<String, String> map = new HashMap<>();
    map.put("HANDPHONE", "color,rom");
    map.put("COMPUTER", "color,screenSize,processorName,ram,ssd");
    pristineItem.setPristineListingAttributes(map);
    itemIdToPristineDataMap.put("PRODUCT_ITEMCODE", pristineItem);
    List<Item> currentItems = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("PRODUCT_ITEMCODE");
    currentItems.add(item);
    Set<String> pristineIds =
        itemIdToPristineDataMap.keySet().stream().map(key -> itemIdToPristineDataMap.get(key).getPristineId()).collect(Collectors.toSet());
    Mockito.when(skuValidator.isItemSku(anyString())).thenReturn(Boolean.FALSE);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID), anySet())).thenReturn(currentItems);
    when(pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(Collections.EMPTY_LIST);
    when(pristineItemRepository.save(any(PristineDataItem.class))).thenReturn(pristineItem);
    when(pristineItemRepository.findByPristineProductNameIgnoreCase(anyString())).thenReturn(null);
    when(itemRepository.saveAll(currentItems)).thenReturn(currentItems);
    this.itemServiceImpl.updateItemsPristineData(STORE_ID, itemIdToPristineDataMap);
    verify(skuValidator).isItemSku(anyString());
    verify(itemRepository).findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID),
        eq(itemIdToPristineDataMap.keySet()));
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    verify(pristineItemRepository).save(pristineItem);
    verify(pristineItemRepository).findByPristineProductNameIgnoreCase(pristineItem.getPristineProductName());
    Mockito.verify(itemRepository).saveAll(currentItems);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(STORE_ID), any(Item.class));
  }


  @Test
  public void updateItemsPristineDataWithHandPhoneWithItemSkuTest() throws Exception {
    Map<String, PristineDataItem> itemIdToPristineDataMap = new HashMap<>();
    PristineDataItem pristineItem = new PristineDataItem();
    pristineItem.setPristineId("PRISTINE_ID");
    pristineItem.setPristineBrand("BRAND");
    pristineItem.setPristineCategory("HANDPHONE");
    pristineItem.setPristineProductName("PRODUCT_NAME");
    pristineItem.setProductCondition("Refurbish");
    HashMap<String, String> map = new HashMap<>();
    map.put("HANDPHONE", "color,rom");
    map.put("COMPUTER", "color,screenSize,processorName,ram,ssd");
    pristineItem.setPristineListingAttributes(map);
    itemIdToPristineDataMap.put("PRODUCT-23255-213425-232555", pristineItem);
    List<Item> currentItems = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("PRODUCT_ITEM_CODE");
    item.setItemSku("PRODUCT-23255-213425-232555");
    currentItems.add(item);
    Set<String> pristineIds =
        itemIdToPristineDataMap.keySet().stream().map(key -> itemIdToPristineDataMap.get(key).getPristineId()).collect(Collectors.toSet());
    Mockito.when(skuValidator.isItemSku(anyString())).thenReturn(Boolean.TRUE);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), anySet())).thenReturn(currentItems);
    when(pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(Collections.EMPTY_LIST);
    when(pristineItemRepository.save(any(PristineDataItem.class))).thenReturn(pristineItem);
    when(pristineItemRepository.findByPristineProductNameIgnoreCase(anyString())).thenReturn(null);
    when(itemRepository.saveAll(currentItems)).thenReturn(currentItems);
    this.itemServiceImpl.updateItemsPristineData(STORE_ID, itemIdToPristineDataMap);
    verify(skuValidator).isItemSku(anyString());
    verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(itemIdToPristineDataMap.keySet()));
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    verify(pristineItemRepository).save(pristineItem);
    verify(pristineItemRepository).findByPristineProductNameIgnoreCase(pristineItem.getPristineProductName());
    Mockito.verify(itemRepository).saveAll(currentItems);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(STORE_ID), any(Item.class));
  }

  @Test
  public void updateItemsPristineDataWithComputerTest() throws Exception {
    Map<String, PristineDataItem> itemIdToPristineDataMap = new HashMap();
    List<PristineDataItem> existingPristineList = new ArrayList();
    PristineDataItem pristineItem = new PristineDataItem();
    pristineItem.setPristineId("PRISTINE_ID");
    pristineItem.setPristineBrand("BRAND");
    pristineItem.setPristineCategory("COMPUTER");
    pristineItem.setPristineProductName("PRODUCT_NAME");
    pristineItem.setProductCondition("Refurbish");
    PristineDataItem pristineItem2 = new PristineDataItem();
    pristineItem2.setPristineId("PRISTINE_ID2");
    pristineItem2.setPristineBrand("BRAND");
    pristineItem2.setPristineCategory("COMPUTER");
    pristineItem2.setPristineProductName("PRODUCT_NAME");
    pristineItem2.setProductCondition("Refurbish");
    HashMap<String, String> map = new HashMap<>();
    map.put("HANDPHONE", "color,rom");
    map.put("COMPUTER", "color,screenSize,processorName,ram,ssd");
    pristineItem.setPristineListingAttributes(map);
    pristineItem2.setPristineListingAttributes(map);
    itemIdToPristineDataMap.put("PRODUCT_ITEMCODE", pristineItem);
    itemIdToPristineDataMap.put("PRODUCT_ITEMCODE2", pristineItem2);
    List<Item> currentItems = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("PRODUCT_ITEMCODE");
    Item item2 = new Item();
    item2.setItemCode("PRODUCT_ITEMCODE2");
    currentItems.add(item);
    currentItems.add(item2);
    existingPristineList.add(pristineItem);
    Set<String> pristineIds =
        itemIdToPristineDataMap.keySet().stream().map(key -> itemIdToPristineDataMap.get(key).getPristineId()).collect(Collectors.toSet());
    Mockito.when(skuValidator.isItemSku(anyString())).thenReturn(Boolean.FALSE);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID), anySet())).thenReturn(currentItems);
    when(pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(existingPristineList);
    when(pristineItemRepository.save(any(PristineDataItem.class))).thenReturn(pristineItem2);
    when(pristineItemRepository.findByPristineProductNameIgnoreCase(anyString())).thenReturn(null);
    when(itemRepository.saveAll(currentItems)).thenReturn(currentItems);
    this.itemServiceImpl.updateItemsPristineData(STORE_ID, itemIdToPristineDataMap);
    verify(skuValidator).isItemSku(anyString());
    verify(itemRepository).findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID),
        eq(itemIdToPristineDataMap.keySet()));
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    verify(pristineItemRepository).save(pristineItem2);
    verify(pristineItemRepository).findByPristineProductNameIgnoreCase(pristineItem2.getPristineProductName());
    Mockito.verify(itemRepository).saveAll(currentItems);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    Mockito.verify(this.cacheEvictHelperService, Mockito.times(2))
        .evictItemData(eq(STORE_ID), any(Item.class));
  }

  @Test
  public void updateItemsPristineDataWithComputer_whenPristineProductNameMatchTest() throws Exception {

    Map<String, PristineDataItem> itemIdToPristineDataMap = new HashMap();
    List<PristineDataItem> existingPristineList = new ArrayList();
    PristineDataItem pristineItem = new PristineDataItem();
    pristineItem.setPristineId("PRISTINE_ID");
    pristineItem.setPristineBrand("BRAND");
    pristineItem.setPristineCategory("COMPUTER");
    pristineItem.setPristineProductName("PRODUCT_NAME");
    pristineItem.setProductCondition("Refurbish");
    PristineDataItem pristineItem2 = new PristineDataItem();
    pristineItem2.setPristineId("PRISTINE_ID2");
    pristineItem2.setPristineBrand("BRAND");
    pristineItem2.setPristineCategory("COMPUTER");
    pristineItem2.setPristineProductName("PRODUCT_NAME");
    pristineItem2.setProductCondition("Refurbish");
    HashMap<String, String> map = new HashMap<>();
    map.put("HANDPHONE", "color,rom");
    map.put("COMPUTER", "color,screenSize,processorName,ram,ssd");
    pristineItem.setPristineListingAttributes(map);
    pristineItem2.setPristineListingAttributes(map);
    itemIdToPristineDataMap.put("PRODUCT_ITEMCODE", pristineItem);
    itemIdToPristineDataMap.put("PRODUCT_ITEMCODE2", pristineItem2);
    List<Item> currentItems = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("PRODUCT_ITEMCODE");
    Item item2 = new Item();
    item2.setItemCode("PRODUCT_ITEMCODE2");
    currentItems.add(item);
    currentItems.add(item2);
    existingPristineList.add(pristineItem);
    Set<String> pristineIds =
        itemIdToPristineDataMap.keySet().stream().map(key -> itemIdToPristineDataMap.get(key).getPristineId()).collect(Collectors.toSet());
    Mockito.when(skuValidator.isItemSku(anyString())).thenReturn(Boolean.FALSE);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID), anySet())).thenReturn(currentItems);
    when(pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(existingPristineList);
    when(pristineItemRepository.save(any(PristineDataItem.class))).thenReturn(pristineItem2);
    when(pristineItemRepository.findByPristineProductNameIgnoreCase(anyString())).thenReturn(null);
    when(itemRepository.saveAll(currentItems)).thenReturn(currentItems);
    this.itemServiceImpl.updateItemsPristineData(STORE_ID, itemIdToPristineDataMap);
    verify(skuValidator).isItemSku(anyString());
    verify(itemRepository).findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID),
        eq(itemIdToPristineDataMap.keySet()));
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    verify(pristineItemRepository).save(pristineItem2);
    verify(pristineItemRepository).findByPristineProductNameIgnoreCase(pristineItem2.getPristineProductName());
    Mockito.verify(itemRepository).saveAll(currentItems);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    Mockito.verify(this.cacheEvictHelperService, Mockito.times(2))
        .evictItemData(eq(STORE_ID), any(Item.class));
  }

  @Test
  public void updateItemsPristineDataExceptionTest() {

    Map<String, PristineDataItem> itemIdToPristineDataMap = new HashMap();
    Mockito.when(this.pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(new ArrayList());

    Mockito.doThrow(RuntimeException.class).when(this.itemRepository)
        .findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID), anySet());
    try {
      this.itemServiceImpl.updateItemsPristineData(STORE_ID, itemIdToPristineDataMap);
    } catch (Exception e) {
      Mockito.verify(this.pristineItemRepository).findByPristineIdIn(anySet());
      Mockito.verify(itemRepository)
          .findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID), anySet());
      assertTrue(e instanceof RuntimeException);

    }
  }

  @Test
  public void updateItemsPristineDataWithBlankNameTest() throws Exception {
    Map<String, PristineDataItem> itemIdToPristineDataMap = new HashMap();
    PristineDataItem pristineItem = new PristineDataItem();
    pristineItem.setPristineId("PRISTINE_ID");
    itemIdToPristineDataMap.put("PRODUCT_ITEMCODE", pristineItem);
    List<Item> currentItems = new ArrayList<>();
    Item item = new Item();
    item.setItemCode("PRODUCT_ITEMCODE");
    currentItems.add(item);
    Set<String> pristineIds =
        itemIdToPristineDataMap.keySet().stream().map(key -> itemIdToPristineDataMap.get(key).getPristineId()).collect(Collectors.toSet());
    Mockito.when(skuValidator.isItemSku(anyString())).thenReturn(Boolean.FALSE);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID), anySet())).thenReturn(currentItems);
    when(pristineItemRepository.findByPristineIdIn(anySet())).thenReturn(Collections.EMPTY_LIST);
    when(pristineItemRepository.save(any(PristineDataItem.class))).thenReturn(pristineItem);
    when(itemRepository.saveAll(currentItems)).thenReturn(currentItems);
    this.itemServiceImpl.updateItemsPristineData(STORE_ID, itemIdToPristineDataMap);
    verify(skuValidator).isItemSku(anyString());
    verify(itemRepository).findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(eq(STORE_ID),
        eq(itemIdToPristineDataMap.keySet()));
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    verify(pristineItemRepository).save(pristineItem);
    Mockito.verify(itemRepository).saveAll(currentItems);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPristineChanges(currentItems);
    Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(STORE_ID), any(Item.class));
  }

  @Test
  public void getMapForPristineCategoryAttributeTest() throws Exception {
    Map<String, String> map = itemServiceImpl.getMapForPristineCategoryAttribute();
    assertNotNull(map);
    assertEquals(2, map.size());
    assertTrue(map.containsKey("HANDPHONE"));
    assertTrue(map.containsKey("COMPUTER"));
  }

  @Test
  public void updateResignMerchantItemsByMerchantCodeTest() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    pageOfItems.getContent().get(0).setItemViewConfigs(itemViewConfigs);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        MERCHANT_CODE, PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    this.itemServiceImpl.updateResignMerchantItemsByMerchantCode(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE);
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse((STORE_ID), (MERCHANT_CODE),
            PageRequest.of(0, 500));
    Mockito.verify(saveOperationService).saveItems(itemListCaptor.capture(), any());
    assertEquals(listOfItems.stream().map(Item::getItemSku).collect(Collectors.toList()),
        itemListCaptor.getAllValues().get(0).stream().map(Item::getItemSku).collect(Collectors.toList()));
  }

  @Test
  public void updateResignMerchantItemsByMerchantCodeExceptionTest() throws Exception {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);
    pageOfItems.getContent().get(0).setItemViewConfigs(itemViewConfigs);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        MERCHANT_CODE, PageRequest.of(0, 500))).thenReturn(this.pageOfItems);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.saveOperationService)
        .saveItems(any(), any());
    this.itemServiceImpl.updateResignMerchantItemsByMerchantCode(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE);
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse((STORE_ID), (MERCHANT_CODE),
            PageRequest.of(0, 500));
  }

  @Test
  public void getItemsByMerchantCodeAndMerchantSkusTest() throws Exception {
    List<String> merchantSkus = new ArrayList<>();
    merchantSkus.add(MERCHANT_SKU);

    Mockito.when(
        this.itemRepository.findByStoreIdAndMerchantCodeAndMerchantSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, MERCHANT_CODE, merchantSkus)).thenReturn(this.listOfItems);

    List<Item> actual =
        itemServiceImpl.getItemsByMerchantCodeAndMerchantSkus(STORE_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
            merchantSkus);

    Mockito.verify(this.itemRepository)
        .findByStoreIdAndMerchantCodeAndMerchantSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, MERCHANT_CODE,
            merchantSkus);
    assertEquals(1, actual.size());
  }

  @Test
  public void updatePromoBundlingByItemSku_SuccessTest() throws Exception {
    Item item1 = new Item();
    item1.setItemSku("sku-1");
    item1.setPromoBundling(true);

    Item item2 = new Item();
    item2.setItemSku("sku-2");
    item2.setPromoBundling(false);

    List<Item> items = Arrays.asList(item1, item2);

    Set<String> itemSkus = new HashSet<>(Arrays.asList("sku-1", "sku-2"));

    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus)).thenReturn(items);

    itemServiceImpl.updatePromoBundlingByItemSkus(STORE_ID, itemSkus, true);
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnPromoBundlingFlagChange(anyList(), Mockito.anyBoolean());
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus);
    Mockito.verify(saveOperationService)
        .updateItemFieldByItemSkus(STORE_ID, Collections.singleton("sku-2"), ProductFieldNames.PROMO_BUNDLING, true);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void updatePromoBundlingByItemSku_WithoutChange() throws Exception {
    List<Item> items = new ArrayList<>();
    Item item = new Item();
    item.setPromoBundling(true);
    items.add(item);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add("sku");
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus)).thenReturn(items);
    itemServiceImpl.updatePromoBundlingByItemSkus(STORE_ID, itemSkus, true);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus);
  }

  @Test
  public void updatePromoBundlingByItemSku_EmptyTest() throws Exception {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add("sku");
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus)).thenReturn(new ArrayList<Item>());
    itemServiceImpl.updatePromoBundlingByItemSkus(STORE_ID, itemSkus, true);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus);
  }

  @Test
  public void updatePromoBundlingByItemSkusInItemPickupPoint_SuccessTest() throws Exception {
    Item item1 = new Item();
    item1.setItemSku("sku-1");
    item1.setPromoBundling(true);

    Item item2 = new Item();
    item2.setItemSku("sku-2");
    item2.setPromoBundling(false);

    List<Item> items = Arrays.asList(item1, item2);

    Set<String> itemSkus = new HashSet<>(Arrays.asList("sku-2"));

    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(
        itemPickupPointService.updateFieldByItemSkusAndDelivery(STORE_ID, itemSkus, ProductFieldNames.PROMO_BUNDLING,
            true, true)).thenReturn(Arrays.asList(itemPickupPoint));
    itemServiceImpl.updatePromoBundlingByItemSkusInItemPickupPoint(STORE_ID, itemSkus, true);
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnPromoBundlingFlagChange(anyList(), Mockito.anyBoolean());
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus);
    Mockito.verify(itemPickupPointService)
        .updateFieldByItemSkusAndDelivery(STORE_ID, Collections.singleton("sku-2"), ProductFieldNames.PROMO_BUNDLING,
            true, true);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void updatePromoBundlingByItemSkusInItemPickupPoint_WithoutChange() throws Exception {
    List<Item> items = new ArrayList<>();
    Item item = new Item();
    item.setPromoBundling(true);
    item.setItemSku("item-sku");
    items.add(item);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add("item-sku");
    itemPickupPoint.setItemSku("item-sku");
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus)).thenReturn(items);
    Mockito.when(
        itemPickupPointService.updateFieldByItemSkusAndDelivery(STORE_ID, itemSkus, ProductFieldNames.PROMO_BUNDLING,
            true, true)).thenReturn(Arrays.asList(itemPickupPoint));
    itemServiceImpl.updatePromoBundlingByItemSkusInItemPickupPoint(STORE_ID, itemSkus, true);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus);
    Mockito.verify(this.productAndItemSolrIndexerService).updateSolrOnPromoBundlingFlagChange(anyList(), Mockito.anyBoolean());
    Mockito.verify(itemPickupPointService)
        .updateFieldByItemSkusAndDelivery(eq(STORE_ID), any(), eq(ProductFieldNames.PROMO_BUNDLING),
            eq(true), eq(true));
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void updatePromoBundlingByItemSkusInItemPickupPoint_EmptyTest() throws Exception {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add("sku");
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkus)).thenReturn(new ArrayList<Item>());
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.updatePromoBundlingByItemSkus(STORE_ID, new HashSet<>(new ArrayList()), true));
  }

  @Test
  public void updateSalesCatalogForAllPristineProducts_whenExpectedExceptionTest() throws InterruptedException {
    Mockito.when(executorService.invokeAll(anyList()))
        .thenThrow(new ApplicationRuntimeException());
    boolean result = itemServiceImpl.updateSalesCatalogForAllPristineProducts(anyList());
    Mockito.verify(executorService).invokeAll(anyList());
    assertFalse(result);
  }

  @Test
  public void getPristineIdsByProductSku_SuccessTest() {
    List<Item> items = new ArrayList<>();
    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    Item item = new Item();
    item.setPristineDataItem(pristineDataItem);
    items.add(item);
    Mockito.when(itemRepository.getByProductSkuAndPristineDataItemExist(eq(STORE_ID), eq(PRODUCT_SKU)))
        .thenReturn(items);
    this.itemServiceImpl.getPristineIdsByProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemRepository).getByProductSkuAndPristineDataItemExist(eq(STORE_ID), eq(PRODUCT_SKU));
  }

  @Test
  public void addActivePromoBundling_Success() {
    Mockito.when(this.saveOperationService.addActivePromoBundling(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PROMO_BUNDLING_TYPE)).thenReturn(this.item);

    Item item = this.itemServiceImpl.addActivePromoBundling(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PROMO_BUNDLING_TYPE);

    assertEquals(this.item, item);

    Mockito.verify(this.saveOperationService)
        .addActivePromoBundling(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PROMO_BUNDLING_TYPE);
  }

  @Test
  public void removeActivePromoBundling_Success() {
    Mockito.when(this.saveOperationService.removeActivePromoBundling(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PROMO_BUNDLING_TYPE)).thenReturn(this.item);

    Item item = this.itemServiceImpl.removeActivePromoBundling(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PROMO_BUNDLING_TYPE);

    assertEquals(this.item, item);

    Mockito.verify(this.saveOperationService)
        .removeActivePromoBundling(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PROMO_BUNDLING_TYPE);
  }

  @Test
  public void getItemByItemCodeTest_success() {
    Set<String> itemSkus = new HashSet<String>();
    itemSkus.add(item.getItemSku());
    Mockito.when(itemCacheableService.findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(itemSkus);
    Mockito.when(itemCacheableService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID), eq(item.getItemSku()), eq(false), eq(false),
        eq(false))).thenReturn(item);
    Item response = itemServiceImpl.getItemByItemCode(STORE_ID, ITEM_CODE);
    Mockito.verify(itemCacheableService).findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(itemCacheableService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(eq(STORE_ID), eq(item.getItemSku()), eq(false), eq(false),
            eq(false));
    assertNotNull(response);
  }

  @Test
  public void getItemByItemCodeTest_nullStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemByItemCode(null, ITEM_CODE));
  }

  @Test
  public void getItemByItemCodeTest_nullItemCode() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemByItemCode(STORE_ID, null));
  }


  @Test
  public void getItemCodesByPristineTest_success() {
    when(itemCacheableService.findItemCodesByPristine(STORE_ID, pristineDataItem)).thenReturn(itemCodes);
    Set<String> response = itemServiceImpl.getItemCodesByPristine(STORE_ID, pristineDataItem);
    verify(itemCacheableService).findItemCodesByPristine(STORE_ID, pristineDataItem);
    assertEquals(response, itemCodes);
  }

  @Test
  public void getItemCodesByPristineTest_nullStoreId() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemCodesByPristine(null, pristineDataItem));
  }

  @Test
  public void getItemCodesByPristineTest_nullPristineDataItem() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemCodesByPristine(STORE_ID, null));
  }

  @Test
  public void getItemCodesByPristineIdTest_success() throws Exception {
    when(itemCacheableService.findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID)).thenReturn(itemCodes);
    Set<String> response = itemServiceImpl.getItemCodesByPristineId(STORE_ID, PRISTINE_ID);
    verify(itemCacheableService).findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(STORE_ID, PRISTINE_ID);
    assertEquals(response, itemCodes);
  }

  @Test
  public void getItemCodesByPristineIdTest_nullStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemCodesByPristineId(null, PRISTINE_ID));
  }

  @Test
  public void getItemCodesByPristineIdTest_nullPristineId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemCodesByPristineId(STORE_ID, null));
  }

  @Test
  public void getItemsAndBundlingInfo_success() throws Exception {
    Set<String> promoBundlingIds = new HashSet<>();
    promoBundlingIds.add(PROMO_BUNDLING_ID);
    this.itemSkus.add(ITEM_SKU);
    ItemInfoVO itemInfoVO = new ItemInfoVO();

    List<ItemInfoVO> itemInfoVOList = new ArrayList<>();
    itemInfoVOList.add(itemInfoVO);

    PromoBundlingVO promoBundlingVO = new PromoBundlingVO();
    promoBundlingVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);

    List<PromoBundlingVO> promoBundlingVOList = new ArrayList<>();
    promoBundlingVOList.add(promoBundlingVO);

    ItemAndBundlingInfoVO itemAndBundlingInfoVO = new ItemAndBundlingInfoVO();
    itemAndBundlingInfoVO.setItems(itemInfoVOList);
    itemAndBundlingInfoVO.setPromoBundlings(promoBundlingVOList);

    List<ItemAndBundlingInfoVO> itemAndBundlingInfoVOList = new ArrayList<>();
    itemAndBundlingInfoVOList.add(itemAndBundlingInfoVO);

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setMasterDataItemAttributeValues(masterDataItemAttributeValueList);

    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(true);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    itemViewConfigs.add(itemViewConfig);

    this.item.setMasterDataItem(masterDataItem);
    this.item.setItemViewConfigs(itemViewConfigs);
    Set<Price> priceSet = new HashSet<>();
    priceSet.add(price2);
    this.item.setPrice(priceSet);

    List<Item> items = new ArrayList<>();
    items.add(item);

    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
    productAndItemsVO.setItems(items);
    productAndItemsVO.setProduct(product);

    List<ProductAndItemsVO> productAndItemsVOList = new ArrayList<>();
    productAndItemsVOList.add(productAndItemsVO);

    ComboRuleVO comboRuleVO = new ComboRuleVO();
    comboRuleVO.setQuantity(10);
    comboRuleVO.setMainSku(true);
    comboRuleVO.setItemSku(ITEM_SKU);

    List<ComboRuleVO> comboRules = new ArrayList<>();
    comboRules.add(comboRuleVO);

    PromoBundlingDetailResponseVO promoBundlingDetailResponseVO = new PromoBundlingDetailResponseVO();
    promoBundlingDetailResponseVO.setPromoBundlingId(PROMO_BUNDLING_ID);
    promoBundlingDetailResponseVO.setPromoBundlingName(PROMO_BUNDLING_NAME);
    promoBundlingDetailResponseVO.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingDetailResponseVO.setItemSku(ITEM_SKU);
    promoBundlingDetailResponseVO.setComboRules(comboRules);

    List<PromoBundlingDetailResponseVO> promoBundlingDetailResponseVOList = new ArrayList<>();
    promoBundlingDetailResponseVOList.add(promoBundlingDetailResponseVO);

    Map<String, Price> priceByItemSku = new HashMap<>();
    priceByItemSku.put(ITEM_SKU, price2);

    when(this.productSearchService.getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkus, false, true, true,
        false, false)).thenReturn(productAndItemsVOList);
    when(this.promoBundlingService.getByPromoBundlingIds(mandatoryRequestParam, promoBundlingIds, itemSkus)).thenReturn(
        promoBundlingDetailResponseVOList);
    when(this.objectConverterService.convertPromoBundlingDetailResponseVOToPromoBundlingVO(promoBundlingDetailResponseVO,
        priceByItemSku)).thenReturn(promoBundlingVO);
    systemParameter.setValue(String.valueOf(10));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MAX_ITEM_COUNT_TRANSACTION_API_SWITCH)).thenReturn(systemParameter);

    ItemAndBundlingInfoVO result = this.itemServiceImpl.getItemsAndBundlingInfo(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID, itemSkus,
        promoBundlingIds, USERNAME);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MAX_ITEM_COUNT_TRANSACTION_API_SWITCH);
    verify(this.productSearchService).getProductAndItemsInfoForAllItems(mandatoryRequestParam, itemSkus, false, true,
        true, false, false);
    verify(this.objectConverterService).convertItemAndProductToItemInfoVO(itemInfoVO, item, product);
    verify(this.objectConverterService).convertProductToItemInfoVO(itemInfoVO, product);
    verify(this.promoBundlingService).getByPromoBundlingIds(mandatoryRequestParam, promoBundlingIds, itemSkus);
    verify(this.objectConverterService).convertPromoBundlingDetailResponseVOToPromoBundlingVO(
        promoBundlingDetailResponseVO, priceByItemSku);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);

    assertNotNull(result);
    assertEquals(itemAndBundlingInfoVO, result);
  }

  @Test
  public void getItemsAndBundlingInfoTest() throws Exception {
    systemParameter.setValue(String.valueOf(0));
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MAX_ITEM_COUNT_TRANSACTION_API_SWITCH)).thenReturn(systemParameter);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.getItemsAndBundlingInfo(STORE_ID, CHANNEL_WEB, CLIENT_ID, REQUEST_ID, itemSkus, Collections.EMPTY_SET, USERNAME));
    } finally {
      verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, SystemParameterNames.MAX_ITEM_COUNT_TRANSACTION_API_SWITCH);
    }
  }

  @Test
  public void updateCategoryCodeByItemSkuListTest() {
    when(this.itemRepository.updateCategoryCodeByItemSkus(anyString(), anyList(),
        anyString())).thenReturn(Arrays.asList(item));
    this.itemServiceImpl.updateCategoryCodeByItemSkuList(STORE_ID, Arrays.asList(ITEM_SKU), PRODUCT_CATEGORY_CODE);
    verify(this.itemRepository).updateCategoryCodeByItemSkus(anyString(), anyList(),
        anyString());
    verify(this.cacheEvictHelperService).evictItemCache(STORE_ID, item);
  }

  @Test
  public void updateCncActivated_SuccessTest() {
    when(this.itemRepository.updateCncActivated(STORE_ID, ITEM_SKU, false, USERNAME)).thenReturn(item);
    this.itemServiceImpl.updateCncActivated(STORE_ID, ITEM_SKU, false, USERNAME);
    verify(this.itemRepository).updateCncActivated(STORE_ID, ITEM_SKU, false, USERNAME);
    verify(this.cacheEvictHelperService).evictItemCache(STORE_ID, item);
  }

  @Test
  public void updateCncActivated_ExceptionTest() {
    doThrow(new ApplicationRuntimeException()).when(this.itemRepository).updateCncActivated(STORE_ID, null, false, USERNAME);
    try {
      this.itemServiceImpl.updateCncActivated(STORE_ID, null, false, USERNAME);
    } catch (Exception e) {
      verify(this.itemRepository).updateCncActivated(STORE_ID, null, false, USERNAME);
    }
  }

  @Test
  public void updateCncActivatedByMerchantCode_valid_success() {
    when(this.itemRepository.findSpecificFieldsByStoreIdAndMarkForDeleteFalseAndIsArchivedFalseAndMerchantCode(STORE_ID,
        MERCHANT_CODE)).thenReturn(Arrays.asList(item));
    this.itemServiceImpl.updateCncActivatedByMerchantCode(STORE_ID, MERCHANT_CODE, false, USERNAME);
    verify(this.itemRepository).findSpecificFieldsByStoreIdAndMarkForDeleteFalseAndIsArchivedFalseAndMerchantCode(
        STORE_ID, MERCHANT_CODE);
    verify(this.itemRepository).updateCncActivatedByMerchantCode(STORE_ID, MERCHANT_CODE, false, USERNAME);
    verify(this.cacheEvictHelperService).evictItemCache(STORE_ID, item);
  }

  @Test
  public void updateCncActivatedByMerchantCode_exceptionTest() {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.itemRepository)
        .updateCncActivatedByMerchantCode(STORE_ID, null, false, USERNAME);
    try {
      this.itemServiceImpl.updateCncActivatedByMerchantCode(STORE_ID, null, false, USERNAME);
    } catch (Exception e) {
      Mockito.verify(this.itemRepository).updateCncActivatedByMerchantCode(STORE_ID, null, false, USERNAME);
    }
  }

  @Test
  public void updateCncActivatedByItemSkusAndPublishTest() throws Exception {
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ITEM_SKU);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkuSet))
        .thenReturn(Arrays.asList(item));
    this.itemServiceImpl.updateCncActivatedByItemSkusAndPublish(STORE_ID, itemSkuSet, false, USERNAME);
    Mockito.verify(itemRepository).updateCncActivatedByItemSkuInMarkForDeleteFalse(STORE_ID, itemSkuSet, false, USERNAME);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkuSet);
    Mockito.verify(saveAndPublishService).publishItemDataChangeEvent(Arrays.asList(item));
    Mockito.verify(this.cacheEvictHelperService).evictItemCache(STORE_ID, item);
  }

  @Test
  public void updateItemTestPickUpPointUpdated() throws Exception {
    Mockito.when(
        this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
            ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint1.setDelivery(true);
    itemPickupPoint1.setMarkForDelete(true);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_1);
    item.setPickupPointCode(PICKUP_POINT_CODE_1);
    product.setCategoryCode(CATEGORY_ID);
    Mockito.when(
            itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemUnsyncUpdated.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(),
        anyString(), eq(true), eq(false))).thenReturn(Arrays.asList(itemPickupPoint1));
    Mockito.when(itemPickupPointService
            .findByItemSkuAndPickupPointCode(anyString(), anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnsyncUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);

    itemUnsyncUpdated.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemUnsyncUpdated.setMerchantSku(ItemServiceImplTest.NEW_MERCHANT_SKU);
    Mockito.when(productService.findByStoreIdAndProductSku(anyString(), anyString()))
        .thenReturn(product);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);

    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(new ArrayList<>()));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService, Mockito.times(2)).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        anyList());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(), anyString(), eq(true),
            eq(false));
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(anyString(), anyString(),
            anyString());
    Mockito.verify(productService).findByStoreIdAndProductSku(anyString(), anyString());
  }

  @Test
  public void updateItemTestPickUpPointUpdatedMppTrue() throws Exception {
    Mockito.when(
        this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
            ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint1.setDelivery(true);
    itemPickupPoint1.setMarkForDelete(true);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_1);
    item.setPickupPointCode(PICKUP_POINT_CODE_1);
    product.setCategoryCode(CATEGORY_ID);
    Mockito.when(
            itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemUnsyncUpdated.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(),
        anyString(), eq(true), eq(false))).thenReturn(Arrays.asList(itemPickupPoint1));
    Mockito.when(itemPickupPointService
            .findByItemSkuAndPickupPointCode(anyString(), anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnsyncUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    itemUnsyncUpdated.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemUnsyncUpdated.setMerchantSku(ItemServiceImplTest.NEW_MERCHANT_SKU);
    Mockito.when(productService.findByStoreIdAndProductSku(anyString(), anyString()))
        .thenReturn(product);
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);

    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(new ArrayList<>()));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService, Mockito.times(2)).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        anyList());

    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(), anyString(), eq(true),
            eq(false));
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(anyString(), anyString(),
            anyString());
    Mockito.verify(productService).findByStoreIdAndProductSku(anyString(), anyString());
  }

  @Test
  public void updateItemTestPickUpPointUpdatedMppTrueProductTypeUpdated() throws Exception {
    Mockito.when(
        this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
            ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint1.setDelivery(true);
    itemPickupPoint1.setMarkForDelete(true);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_1);
    item.setPickupPointCode(PICKUP_POINT_CODE_1);
    product.setCategoryCode(CATEGORY_ID);
    Mockito.when(
            itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemUnsyncUpdated.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(),
        anyString(), eq(true), eq(false))).thenReturn(Arrays.asList(itemPickupPoint1));
    Mockito.when(itemPickupPointService
            .findByItemSkuAndPickupPointCode(anyString(), anyString(), anyString()))
        .thenReturn(itemPickupPoint);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnsyncUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED))
        .thenReturn(null);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    itemUnsyncUpdated.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemUnsyncUpdated.setMerchantSku(ItemServiceImplTest.NEW_MERCHANT_SKU);
    Mockito.when(productService.findByStoreIdAndProductSku(anyString(), anyString()))
        .thenReturn(product);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, true, false);

    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(new ArrayList<>()));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        anyList());

  }

  @Test
  public void updateItemTestPickUpPointUpdatedMFDFalse() throws Exception {
    Mockito.when(
        this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
            ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint1.setDelivery(true);
    itemPickupPoint1.setMarkForDelete(false);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE_1);
    item.setPickupPointCode(PICKUP_POINT_CODE_1);
    Mockito.when(
            itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, itemUnsyncUpdated.getItemSku(), false))
        .thenReturn(item);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(),
        anyString(), eq(true), eq(false))).thenReturn(Arrays.asList(itemPickupPoint1));
    Mockito.when(this.offlineItemService.findByMerchantCodeAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.MERCHANT_CODE, ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED))
        .thenReturn(this.offlineItemList);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnsyncUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU, false, false, false)).thenReturn(Arrays.asList(itemUnsyncUpdated));
    itemUnsyncUpdated.setPickupPointCode(PICKUP_POINT_CODE);
    this.itemUnsyncUpdated.setMerchantSku(ItemServiceImplTest.NEW_MERCHANT_SKU);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);

    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, PRODUCT_SKU);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.saveOperationService).saveItem(itemArgumentCaptor.capture(), eq(new ArrayList<>()),
        eq(new ArrayList<>()));
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService, Mockito.times(2)).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        anyList());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(), anyString(), eq(true),
            eq(false));
    Mockito.verify(itemPickupPointService)
        .findItemPickupPointsByProductSkuAndPPcode(anyString(), anyString(),
            anyString());
  }

  @Test
  public void getItemsByPristineMasterId_PristineIsNull() {
    when(pristineItemRepository.findByPristineMasterId(PRISTINE_MASTER_ID)).thenReturn(null);
    List<Item> result =
        this.itemServiceImpl.getItemsByPristineMasterId(STORE_ID, USERNAME, REQUEST_ID, PRISTINE_MASTER_ID);
    verify(pristineItemRepository).findByPristineMasterId(PRISTINE_MASTER_ID);
    assertTrue(CollectionUtils.isEmpty(result));
  }

  @Test
  public void findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse_blankStoreId() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
        null, ITEM_CODE));
  }

  @Test
  public void findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse_blankItemCode() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
        STORE_ID, null));
  }

  @Test
  public void findItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse_success() throws Exception {
    List<Item> items = new ArrayList<>();
    when(itemRepository.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
        STORE_ID, ITEM_CODE)).thenReturn(items);
    List<Item> result = this.itemServiceImpl.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
        STORE_ID, ITEM_CODE);
    assertEquals(items, result);
    verify(itemRepository).findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
        STORE_ID, ITEM_CODE);
  }

  @Test
  public void isItemBuyableAndDiscoverable_True() {
    Set<ItemViewConfig> itemViewConfigs = mockItemViewConfigs(true, true);
    item.setItemViewConfigs(itemViewConfigs);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(itemPickupPoint);
    assertTrue(itemServiceImpl.isItemBuyableAndDiscoverable(item));
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
  }

  @Test
  public void isItemBuyableAndDiscoverable_TrueWithSchedules() {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setBuyable(true);
    itemBuyableSchedule.setStartDateTime(new Date(Calendar.getInstance().getTimeInMillis() - 100000));
    itemBuyableSchedule.setEndDateTime(new Date(Calendar.getInstance().getTimeInMillis() + 100000));
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    itemViewConfig.setDiscoverable(true);
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfigs.add(itemViewConfig);
    item.setItemViewConfigs(itemViewConfigs);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(itemPickupPoint);
    assertTrue(itemServiceImpl.isItemBuyableAndDiscoverable(item));
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
  }

  @Test
  public void isItemBuyableAndDiscoverable_False() {
    Set<ItemViewConfig> itemViewConfigs = mockItemViewConfigs(true, false);
    item.setItemViewConfigs(itemViewConfigs);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(itemPickupPoint);
    assertFalse(itemServiceImpl.isItemBuyableAndDiscoverable(item));
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
  }

  @Test
  public void isItemBuyableAndDiscoverable_False_test() {
    Set<ItemViewConfig> itemViewConfigs = mockItemViewConfigs(true, false);
    item.setItemViewConfigs(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(null);
    assertFalse(itemServiceImpl.isItemBuyableAndDiscoverable(item));
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
  }

  @Test
  public void isItemBuyableAndDiscoverable_FalseWithSchedules() {
    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    ItemBuyableSchedule itemBuyableSchedule = new ItemBuyableSchedule();
    itemBuyableSchedule.setBuyable(false);
    itemBuyableSchedule.setStartDateTime(new Date(Calendar.getInstance().getTimeInMillis() - 100000));
    itemBuyableSchedule.setEndDateTime(new Date(Calendar.getInstance().getTimeInMillis() + 100000));
    itemViewConfig.setItemBuyableSchedules(itemBuyableSchedule);
    itemViewConfig.setDiscoverable(true);
    itemViewConfigs.add(itemViewConfig);
    item.setItemViewConfigs(itemViewConfigs);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(itemPickupPoint);
    assertFalse(itemServiceImpl.isItemBuyableAndDiscoverable(item));
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
  }

  @Test
  public void findBuyableDiscoverableItemsByPristineIdTest() {
    Set<ItemViewConfig> itemViewConfigs = mockItemViewConfigs(true, true);
    item.setItemViewConfigs(itemViewConfigs);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(itemPickupPoint);
    PristineDataItem pristineItem = new PristineDataItem();
    Mockito.when(pristineItemRepository.findByPristineId(PRISTINE_ID)).thenReturn(pristineItem);
    Mockito.when(this.itemRepository.findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        pristineItem)).thenReturn(listOfItems);
    List<Item> result = this.itemServiceImpl.findBuyableDiscoverableItemsByPristineId(STORE_ID, PRISTINE_ID);
    Mockito.verify(this.itemRepository)
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, pristineItem);
    verify(pristineItemRepository).findByPristineId(PRISTINE_ID);
    assertEquals(result, listOfItems);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
  }

  @Test
  public void findFirstBuyableDiscoverableItemSkuTest() {
    LinkedHashSet<String> itemSkuSet = new LinkedHashSet<>();
    itemSkuSet.add(ITEM_SKU);
    itemSkuSet.add(ITEM_SKU3);
    itemSkuSet.add(ITEM_SKU2);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    List<Item> itemList = new ArrayList<>();
    item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setItemViewConfigs(mockItemViewConfigs(false, true));
    itemList.add(item);
    item = new Item();
    item.setItemSku(ITEM_SKU2);
    item.setItemViewConfigs(mockItemViewConfigs(true, true));
    item.setPristineDataItem(pristineDataItem);
    itemList.add(item);
    item = new Item();
    item.setItemSku(ITEM_SKU3);
    item.setItemViewConfigs(mockItemViewConfigs(true, true));
    itemList.add(item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(mockItemViewConfigs(false, true));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), eq(ITEM_SKU)))
        .thenReturn(itemPickupPoint);
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemViewConfig(mockItemViewConfigs(true, true));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), eq(ITEM_SKU2)))
        .thenReturn(itemPickupPoint1);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), eq(ITEM_SKU3)))
        .thenReturn(itemPickupPoint1);
    Mockito.when(itemRepository.getItemViewConfigsByItemSkus(STORE_ID, itemSkuSet)).thenReturn(itemList);
    DefaultItemSkuVO response = this.itemServiceImpl.findFirstBuyableDiscoverableItemSkuByPristineId(STORE_ID, itemSkuSet, PRISTINE_ID);
    Mockito.verify(itemRepository).getItemViewConfigsByItemSkus(STORE_ID, itemSkuSet);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(any(), any());
    assertNotNull(response);
  }

  private Set<ItemViewConfig> mockItemViewConfigs(boolean isBuyable, boolean isDiscoverable) {
    Set<ItemViewConfig> itemViewConfigs1 = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(Constants.DEFAULT);
    itemViewConfig.setBuyable(isBuyable);
    itemViewConfig.setDiscoverable(isDiscoverable);
    itemViewConfigs1.add(itemViewConfig);
    return itemViewConfigs1;
  }

  @Test
  public void findFirstBuyableDiscoverableItemSkuTest_nullItemSku_success() {
    LinkedHashSet<String> itemSkuSet = new LinkedHashSet<>();
    itemSkuSet.add(null);
    itemSkuSet.add(ITEM_SKU3);
    itemSkuSet.add(ITEM_SKU2);

    PristineDataItem pristineDataItem = new PristineDataItem();
    pristineDataItem.setPristineId(PRISTINE_ID);
    List<Item> itemList = new ArrayList<>();
    item = new Item();
    item.setItemSku(ITEM_SKU2);
    item.setItemViewConfigs(mockItemViewConfigs(true, true));
    itemList.add(item);
    item = new Item();
    item.setItemSku(ITEM_SKU3);
    item.setItemViewConfigs(mockItemViewConfigs(true, true));
    item.setPristineDataItem(pristineDataItem);
    itemList.add(item);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemViewConfig(itemViewConfigs);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku()))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.getItemViewConfigsByItemSkus(STORE_ID, itemSkuSet)).thenReturn(itemList);
    DefaultItemSkuVO response = this.itemServiceImpl.findFirstBuyableDiscoverableItemSkuByPristineId(STORE_ID, itemSkuSet, PRISTINE_ID);
    Mockito.verify(itemRepository).getItemViewConfigsByItemSkus(STORE_ID, itemSkuSet);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
    assertNotNull(response);
    assertEquals(response.getDefaultItemSku(), ITEM_SKU3);
  }

  @Test
  public void findFirstBuyableDiscoverableItemSku_EmptyTest() {
    LinkedHashSet<String> itemSkus = new LinkedHashSet<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(itemRepository.getItemViewConfigsByItemSkus(STORE_ID, itemSkus)).thenReturn(new ArrayList<>());
    DefaultItemSkuVO response = this.itemServiceImpl.findFirstBuyableDiscoverableItemSkuByPristineId(STORE_ID, itemSkus, PRISTINE_ID);
    Mockito.verify(itemRepository).getItemViewConfigsByItemSkus(STORE_ID, itemSkus);
    assertNotNull(response);
    assertEquals(StringUtils.EMPTY, response.getDefaultItemSku());
  }

  @Test
  public void findFirstBuyableDiscoverableItemSku_ApplicationRuntimeException_EmptyItemSkuSet() {
    try {
      this.itemServiceImpl.findFirstBuyableDiscoverableItemSkuByPristineId(STORE_ID, new LinkedHashSet<>(), PRISTINE_ID);
    } catch (ApplicationRuntimeException e) {
      assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
      assertEquals(CommonConstants.APPLICATION_RUNTIME_EXCEPTION_MESSAGE + CommonConstants.ITEM_SKUS_MUST_NOT_BE_EMPTY,
          e.getErrorMessage());
    }
  }

  @Test
  public void findFirstBuyableDiscoverableItemSku_ApplicationRuntimeException_NullStoreId() {
    LinkedHashSet<String> itemSkus = new LinkedHashSet<>();
    itemSkus.add(ITEM_SKU);
    try {
      this.itemServiceImpl.findFirstBuyableDiscoverableItemSkuByPristineId(null, itemSkus, PRISTINE_ID);
    } catch (ApplicationRuntimeException e) {
      assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
      assertEquals(CommonConstants.APPLICATION_RUNTIME_EXCEPTION_MESSAGE + CommonConstants.STORE_ID_MUST_NOT_BE_BLANK,
          e.getErrorMessage());
    }
  }

  @Test
  public void findFirstBuyableDiscoverableItemSku_ApplicationRuntimeException_NullPristineId() {
    LinkedHashSet<String> itemSkus = new LinkedHashSet<>();
    itemSkus.add(ITEM_SKU);
    try {
      this.itemServiceImpl.findFirstBuyableDiscoverableItemSkuByPristineId(STORE_ID, itemSkus, null);
    } catch (ApplicationRuntimeException e) {
      assertEquals(ErrorCategory.VALIDATION, e.getErrorCodes());
      assertEquals(CommonConstants.APPLICATION_RUNTIME_EXCEPTION_MESSAGE + CommonConstants.PRISTINE_ID_MUST_NOT_BE_BLANK,
          e.getErrorMessage());
    }
  }

  @Test
  public void updatePromoPriceForItemSkuList() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, Boolean.TRUE, Boolean.FALSE,
        Boolean.FALSE, null, false)).thenReturn(listOfItems.get(0));
    this.itemServiceImpl.updatePromotionPriceForItemSkuList(STORE_ID, itemSkus);
    Mockito.verify(this.itemCacheableService)
        .findItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(eq(STORE_ID), eq(itemSkus));
    Mockito.verify(itemPriceService)
        .getAndSetPromotionPrice(eq(REQUEST_ID), eq(USERNAME), anyList());
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, Boolean.TRUE, Boolean.FALSE, Boolean.FALSE, null, false);

    for (Item updatedItem : listOfItems) {
      Mockito.verify(this.itemPickupPointService)
          .updateDiscountPriceInItemPickupPoint(any(ItemPickupPoint.class));
      Mockito.verify(this.cacheEvictHelperService).evictItemData(eq(STORE_ID), eq(updatedItem));
    }
  }

  @Test
  public void updateItemPromotionActiveFlagTest() {
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(new HashSet(Collections.singletonList(ITEM_SKU))))).thenReturn(listOfItems);
    this.itemServiceImpl.updateItemFlashSaleActiveFlag(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(new HashSet(Collections.singletonList(ITEM_SKU))));
    Mockito.verify(saveOperationService).saveItems(listOfItems, null);
    Mockito.verify(cacheEvictHelperService).evictItemData(eq(STORE_ID), any(Item.class));
  }

  @Test
  public void updateItemPromotionActiveFlag_WhenEmptyItemSkusTest() {
    this.itemServiceImpl.updateItemFlashSaleActiveFlag(STORE_ID, Collections.EMPTY_LIST, true);
  }

  @Test
  public void updateItemPromotionActiveFlag_WhenItemsNotPresentTest() {
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(new HashSet(Collections.singletonList(ITEM_SKU))))).thenReturn(Collections.EMPTY_LIST);
    this.itemServiceImpl.updateItemFlashSaleActiveFlag(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(new HashSet(Collections.singletonList(ITEM_SKU))));
  }

  @Test
  public void updateItemPromotionActiveFlag_WhenExceptionTest() {
    Mockito.doThrow(new RuntimeException()).when(itemRepository)
        .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID),
            eq(new HashSet(Collections.singletonList(ITEM_SKU))));
    boolean result =
        this.itemServiceImpl.updateItemFlashSaleActiveFlag(STORE_ID, Collections.singletonList(ITEM_SKU), true);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(eq(STORE_ID), eq(new HashSet(Collections.singletonList(ITEM_SKU))));
    assertFalse(result);
  }

  @Test
  public void findByStoreIdAndMarkForDeleteFalseAndProductSkusTest() {
    Set skus = new HashSet<>(Collections.singletonList(ItemServiceImplTest.PRODUCT_SKU));
    Mockito.when(this.itemRepository
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, skus,
            CommonConstants.itemFields)).thenReturn(this.listOfItems);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID,
        ITEM_SKU)).thenReturn(itemPickupPoint);
    List<Item> result = this.itemServiceImpl
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.USERNAME,
            ItemServiceImplTest.REQUEST_ID, skus, true);

    Mockito.verify(this.itemRepository)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, skus,
            CommonConstants.itemFields);
    Mockito.verify(this.itemCacheableService)
        .setActivePromoBundlingsByPristineOrItemCode(eq(STORE_ID), any(Item.class));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(listOfItems, Collections.singletonList(itemPickupPoint));
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void findByStoreIdAndMarkForDeleteFalseAndProductSkus1Test() {
    Set skus = new HashSet<>(Collections.singletonList(ItemServiceImplTest.PRODUCT_SKU));
    Mockito.when(this.itemRepository.findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, skus,
        CommonConstants.itemFields)).thenReturn(this.listOfItems);
    List<Item> result = this.itemServiceImpl.findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID, skus, true);

    Mockito.verify(this.itemRepository)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, skus,
            CommonConstants.itemFields);
    Mockito.verify(this.itemCacheableService)
        .setActivePromoBundlingsByPristineOrItemCode(eq(STORE_ID), any(Item.class));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(listOfItems, Collections.singletonList(itemPickupPoint));
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void findByStoreIdAndMarkForDeleteFalseAndProductSkus_WhenCombineOthersBundlingsFalseTest() throws Exception {
    Set skus = new HashSet<>(Arrays.asList(ItemServiceImplTest.PRODUCT_SKU));
    Mockito.when(this.itemRepository
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, skus,
            CommonConstants.itemFields)).thenReturn(this.listOfItems);
    List<Item> result = this.itemServiceImpl
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.USERNAME,
            ItemServiceImplTest.REQUEST_ID, skus, false);

    Mockito.verify(this.itemRepository)
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(ItemServiceImplTest.STORE_ID, skus,
            CommonConstants.itemFields);
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(listOfItems, Collections.singletonList(itemPickupPoint));
    assertNotNull(result);
    assertEquals(result, this.listOfItems);
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteFalseTest() {
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        PRODUCT_SKU)).thenReturn(listOfItems);
    this.itemServiceImpl.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void deleteItemByStoreIdAndProductSkusTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    Mockito.when(itemRepository.deleteByStoreIdAndProductSkuIn(STORE_ID, productSkuSet)).thenReturn(new ArrayList<>());
    this.itemServiceImpl.deleteItemByStoreIdAndProductSkus(STORE_ID, productSkuSet);
    Mockito.verify(itemRepository).deleteByStoreIdAndProductSkuIn(STORE_ID, productSkuSet);
  }

  @Test
  public void deleteItemByStoreIdAndProductSkusStoreIdEmptyTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    productSkuSet.add(PRODUCT_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->  this.itemServiceImpl.deleteItemByStoreIdAndProductSkus(StringUtils.EMPTY, productSkuSet));
  }

  @Test
  public void deleteItemByStoreIdAndProductSkusProductSkuSetEmptyTest() throws Exception {
    Set<String> productSkuSet = new HashSet<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.deleteItemByStoreIdAndProductSkus(STORE_ID, productSkuSet));
  }

  @Test
  public void findByStoreIdAndItemSkuTest() {
    when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Item response = itemServiceImpl.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    assertEquals(item.getItemSku(), response.getItemSku());
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse_whenStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(null, ItemServiceImplTest.PRODUCT_SKU));
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse_whenProductSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID, null));
  }

  @Test
  public void sendItemChangeEventByUpdatedByTest() {
    Slice<Item> items = new SliceImpl<Item>(Arrays.asList(this.item), PageRequest.of(0, 100),
        false);
    Mockito.when(this.itemRepository.findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY), any(Pageable.class))).thenReturn(items);
    this.itemServiceImpl.sendItemChangeEventByUpdatedBy(STORE_ID, UPDATED_BY);
    verify(this.itemRepository).findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY), any(Pageable.class));
    verify(this.masterDataCacheService).evictMasterDataItem(eq(ITEM_SKU), eq(null));
    verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture());
    Item item = itemListCaptor.getValue().get(0);
    assertEquals(ITEM_SKU, item.getItemSku());
  }

  @Test
  public void sendItemChangeEventByUpdatedByExceptionTest() {
    Mockito.doThrow(RuntimeException.class).when(this.itemRepository)
        .findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY), any(Pageable.class));
    this.itemServiceImpl.sendItemChangeEventByUpdatedBy(STORE_ID, UPDATED_BY);
    verify(this.itemRepository).findByStoreIdAndUpdatedByAndMarkForDeleteFalse(eq(STORE_ID), eq(UPDATED_BY), any(Pageable.class));
  }

  @Test
  public void updateItemMerchantDiscountPriceTest() throws Exception {
    when(this.itemRepository.updateItemMerchantDiscountPrice(ItemServiceImplTest.STORE_ID, item)).thenReturn(item);
    this.itemServiceImpl.updateItemMerchantDiscountPrice(ItemServiceImplTest.STORE_ID, item);
    verify(this.itemRepository).updateItemMerchantDiscountPrice(ItemServiceImplTest.STORE_ID, item);
    verify(this.saveAndPublishService).publishMerchantPromoDiscountEventChange(item);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, false);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
  }

  @Test
  public void updateItemMerchantDiscountPriceWithoutUpdatingL4DbTest() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    this.itemServiceImpl.updateItemMerchantDiscountPriceWithoutUpdatingL4Db(STORE_ID, ITEM_SKU, discountPrice);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, false);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    verify(this.saveAndPublishService).publishMerchantPromoDiscountEventChange(itemPickupPoint);
    verify(this.itemPickupPointService).updateDiscountPriceInItemPickupPoint(itemPickupPointArgumentCaptor.capture());
  }

  @Test
  public void updateItemMerchantDiscountPriceWithoutUpdatingL4DbTestItemNull() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(null);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    try {
      this.itemServiceImpl.updateItemMerchantDiscountPriceWithoutUpdatingL4Db(STORE_ID, ITEM_SKU, discountPrice);
    } catch (Exception e) {
      assertEquals(e.getClass(), ApplicationRuntimeException.class);
    } finally {
      Mockito.verify(this.itemCacheableService).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    }
  }

  @Test
  public void updateItemMerchantDiscountPriceWithoutUpdatingL4DbTestItemPPNull() throws Exception {
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(null);
    try {
      this.itemServiceImpl.updateItemMerchantDiscountPriceWithoutUpdatingL4Db(STORE_ID, ITEM_SKU, discountPrice);
    } catch (Exception e) {
      assertEquals(e.getClass(), ApplicationRuntimeException.class);
    } finally {
      verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
      Mockito.verify(this.itemCacheableService).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    }
  }

  @Test
  public void updateItemMerchantDiscountPriceByItemSkuAndPPCodeTest() throws Exception {
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(itemPickupPoint);
    this.itemServiceImpl.updateItemMerchantDiscountPriceByItemSkuAndPPCode(STORE_ID, ITEM_SKU, discountPrice,
        PICKUP_POINT_CODE, true);
    verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    verify(this.saveAndPublishService).publishMerchantPromoDiscountEventChange(itemPickupPoint);
    verify(this.itemPickupPointService).updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(
        itemPickupPointArgumentCaptor.capture());
    verify(productAndItemSolrIndexerService).indexMerchantPromoDiscountItemPickupPoint(itemPickupPoint, true);
    verify(productL3SolrService).updatePromoOrWholesaleItemSkusByItemPickupPoint(itemPickupPoint);
  }

  @Test
  public void updateItemMerchantDiscountPriceByItemSkuAndPPCodeItemNullTest() throws Exception {
    Mockito.when(
            itemPickupPointService.findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.updateItemMerchantDiscountPriceByItemSkuAndPPCode(STORE_ID, ITEM_SKU, discountPrice,
          PICKUP_POINT_CODE, true));
    } finally {
      verify(itemPickupPointService).findByItemSkuAndPickupPointCode(STORE_ID, ITEM_SKU, PICKUP_POINT_CODE);
    }
  }

  @Test
  public void updateMerchantPromoDiscountFlagTest() throws Exception {
    when(this.itemRepository.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true)).thenReturn(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    this.itemServiceImpl.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true);
    verify(this.itemRepository).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, true);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void updateMerchantPromoDiscountFlag_WhenUpdateFalseTest() throws Exception {
    when(this.itemRepository.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true)).thenReturn(false);
    this.itemServiceImpl.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true);
    verify(this.itemRepository).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true);
  }

  @Test
  public void updateMerchantPromoDiscountFlag_WhenIsPromoActiveFalseTest() throws Exception {
    when(this.itemRepository.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false)).thenReturn(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    this.itemServiceImpl.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false);
    verify(this.itemRepository).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, true);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void updateMerchantPromoDiscountFlagInItemPickupPointTest() throws Exception {
    when(this.itemPickupPointService.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true)).thenReturn(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    this.itemServiceImpl.updateMerchantPromoDiscountFlagInItemPickupPoint(STORE_ID, ITEM_SKU, true);
    verify(this.itemPickupPointService).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, true);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void updateMerchantPromoDiscountFlagInItemPickupPoint_WhenUpdateFalseTest() throws Exception {
    when(this.itemPickupPointService.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true)).thenReturn(false);
    this.itemServiceImpl.updateMerchantPromoDiscountFlagInItemPickupPoint(STORE_ID, ITEM_SKU, true);
    verify(this.itemPickupPointService).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, true);
  }

  @Test
  public void updateMerchantPromoDiscountFlagByItemSkuAndPPCodeTest() {
    List<ItemInfo> itemInfos = new ArrayList<>();
    ItemInfo itemInfo = new ItemInfo();
    itemInfo.setItemSku(ITEM_SKU);
    itemInfo.setPickupPointCode(PICKUP_POINT_CODE);
    itemInfos.add(itemInfo);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    when(itemPickupPointService.updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos,
        ProductFieldNames.MERCHANT_PROMO_DISCOUNT, true)).thenReturn(Arrays.asList(itemPickupPoint));
    itemServiceImpl.updateMerchantPromoDiscountFlagByItemSkuAndPPCode(STORE_ID, ITEM_SKU, true, PICKUP_POINT_CODE);
    verify(itemPickupPointService).updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos,
        ProductFieldNames.MERCHANT_PROMO_DISCOUNT, true);
    verify(productAndItemSolrIndexerService).updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), eq(true), eq(
        SolrFieldNames.MERCHANT_PROMO_DISCOUNT), eq(productSkuAndMerchantCodeMap));
  }

  @Test
  public void updateMerchantPromoDiscountFlagByItemSkuAndPPCodeEmptyPPsTest() {
    List<ItemInfo> itemInfos = new ArrayList<>();
    ItemInfo itemInfo = new ItemInfo();
    itemInfo.setItemSku(ITEM_SKU);
    itemInfo.setPickupPointCode(PICKUP_POINT_CODE);
    itemInfos.add(itemInfo);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    when(itemPickupPointService.updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos,
        ProductFieldNames.MERCHANT_PROMO_DISCOUNT, true)).thenReturn(new ArrayList<>());
    itemServiceImpl.updateMerchantPromoDiscountFlagByItemSkuAndPPCode(STORE_ID, ITEM_SKU, true, PICKUP_POINT_CODE);
    verify(itemPickupPointService).updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos,
        ProductFieldNames.MERCHANT_PROMO_DISCOUNT, true);
  }

  @Test
  public void updateMerchantPromoDiscountFlagInItemPickupPoint_WhenIsPromoActiveFalseTest() throws Exception {
    when(this.itemPickupPointService.updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false)).thenReturn(true);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    this.itemServiceImpl.updateMerchantPromoDiscountFlagInItemPickupPoint(STORE_ID, ITEM_SKU, false);
    verify(this.itemPickupPointService).updateMerchantPromoDiscountFlag(STORE_ID, ITEM_SKU, false);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, true);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void findByStoreIdAndUpdatedDateGreaterThanTest() {
    when(this.itemRepository.findByStoreIdAndUpdatedDateGreaterThan(anyString(), any(), any(Pageable.class))).thenReturn(new PageImpl<>(new ArrayList<>()));
    this.itemServiceImpl.findByStoreIdAndUpdatedDateGreaterThan(STORE_ID, new Date(),
        PageRequest.of(0, 10));
    verify(this.itemRepository).findByStoreIdAndUpdatedDateGreaterThan(anyString(), any(), any(Pageable.class));
  }

  @Test
  public void publishItemsByMerchantCodeToVoucherTest() {
    Date voucherCreatedDate = new Date();
    SystemParameter systemParameter = new SystemParameter();
    systemParameter.setValue("100");
    when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_MERCHANT_VOUCHER_ITEMS_PAGE_SIZE)).thenReturn(systemParameter);
    productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    when(productSearchService.getItemsByMerchantCode(eq(STORE_ID), eq(MERCHANT_CODE), eq(voucherCreatedDate), any(Pageable.class))).thenReturn(new PageImpl<>(Arrays.asList(productAndItemSolr)));
    VoucherItemSkusEventModel voucherItemSkusEventModel =
        VoucherItemSkusEventModel.builder().storeId(STORE_ID).merchantCode(MERCHANT_CODE).voucherCode(ITEM_CODE).voucherCreatedDate(voucherCreatedDate).build();
    this.itemServiceImpl.publishItemsByMerchantCodeToVoucher(voucherItemSkusEventModel);
    verify(productSearchService).getItemsByMerchantCode(eq(STORE_ID), eq(MERCHANT_CODE), eq(voucherCreatedDate), any(Pageable.class));
    verify(saveAndPublishService).publishItemSkuListForVoucher(voucherItemSkusEventModel);
    verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.SOLR_MERCHANT_VOUCHER_ITEMS_PAGE_SIZE);
  }

  @Test
  public void suspendItemsFalseArchivedTrueTest() throws Exception {
    item.setArchived(true);
    item.setArchivedBeforeSuspension(true);
    itemUpdated.setPermanentDelete(true);
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item, itemUpdated));
    Mockito.when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(anyList(), anyList(), anyString())).thenReturn(Arrays.asList(item));
    this.itemServiceImpl. suspendItems(STORE_ID, PRODUCT_SKU, Boolean.FALSE);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(listArgumentCaptor.capture(),
        anyList(), anyString());
    assertEquals(listArgumentCaptor.getValue().size(), 1);
  }

  @Test
  public void suspendItemsFalseArchivedFalseTest() throws Exception {
    item.setArchived(true);
    item.setArchivedBeforeSuspension(false);
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    Mockito.when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(anyList(), anyList(), anyString()))
        .thenReturn(Arrays.asList(item));
    this.itemServiceImpl.suspendItems(STORE_ID, PRODUCT_SKU, Boolean.FALSE);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        anyList(), anyString());
    Mockito.verify(this.saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID,item.getItemSku());
    Mockito.verify(itemPickupPointService).updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, Boolean.FALSE, item, Arrays.asList(itemPickupPoint), false);
  }

  @Test
  public void suspendItemsTrueArchivedTrueTest() throws Exception {
    item.setArchived(true);
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    Mockito.when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(anyList(), anyList(), anyString())).thenReturn(Arrays.asList(item));
    this.itemServiceImpl.suspendItems(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        anyList(), anyString());
  }

  @Test
  public void suspendItemsTrueArchivedTruePublishOnlyDeliveryTrueL5sTest() throws Exception {
    ReflectionTestUtils.setField(this.itemServiceImpl, "suspensionPublishOnlyDeliveryTrueL5", true);
    item.setArchived(true);
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    Mockito.when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        anyList(), anyString())).thenReturn(Arrays.asList(item));
    newItemPickupPoint.setDelivery(false);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint, newItemPickupPoint));
    this.itemServiceImpl.suspendItems(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        eq(Collections.singletonList(itemPickupPoint)), anyString());
  }

  @Test
  public void suspendItemsTrueArchivedTruePublishAllL5sTest() throws Exception {
    ReflectionTestUtils.setField(this.itemServiceImpl, "suspensionPublishOnlyDeliveryTrueL5", false);
    item.setArchived(true);
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    Mockito.when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        anyList(), anyString())).thenReturn(Arrays.asList(item));
    newItemPickupPoint.setDelivery(false);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString()))
        .thenReturn(Arrays.asList(itemPickupPoint, newItemPickupPoint));
    this.itemServiceImpl.suspendItems(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
    Mockito.verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        eq(Arrays.asList(itemPickupPoint, newItemPickupPoint)), anyString());
  }

  @Test
  public void suspendItemsTrueArchivedFalseTest() throws Exception {
    item.setArchived(false);
    this.item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    Mockito.when(itemPickupPointService.findByStoreIdAndItemSku(anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    Mockito.when(saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(anyList(), anyList(), anyString())).thenReturn(Arrays.asList(item));
    this.itemServiceImpl.suspendItems(STORE_ID, PRODUCT_SKU, Boolean.TRUE);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService).saveItemsAndClearCacheWithoutUpdatingSolr(anyList(),
        anyList(), anyString());
    Mockito.verify(this.saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService).updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, Boolean.TRUE, item, Arrays.asList(itemPickupPoint), false);
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, item.getItemSku());
  }

  @Test
  public void updateItemTest_categoryCodeCheck() throws Exception {
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(false);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
    Mockito.verify(this.itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
            this.itemUnSyncTobeUpdated.getItemViewConfigs());
    verify(this.saveOperationService).saveItem(this.itemUnSyncTobeUpdated, new ArrayList<>(), new ArrayList<>());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    assertNotNull(result);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
        Arrays.asList(itemPickupPoint));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
  }

  @Test
  public void updateItemTest_categoryCodeException() throws Exception {
    systemParameter.setValue(PRODUCT_CATEGORY_CODE);
    itemUnsyncUpdated.getMasterDataItem().setItemHeight(HEIGHT);
    itemUnsyncUpdated.getMasterDataItem().setItemLength(ITEM_UPDATED_LENGTH);
    product.getMasterDataProduct().getMasterCatalog().getCategory().setCategoryCode(PRODUCT_CATEGORY_CODE);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(true);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->
          this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, itemUnsyncUpdated, ItemServiceImplTest.USERNAME,
              true, false, false));
    } finally {
      Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
      verify(this.productHelperService).updateItemViewConfigForExistingChannel(this.itemUnSyncTobeUpdated, this.itemUnSyncTobeUpdated.getItemViewConfigs());
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
          ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
      verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
      Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
      verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
          Arrays.asList(itemPickupPoint));
    }
  }

  @Test
  public void updateItemTest_systemParameterException() throws Exception {
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE)).thenThrow(ApplicationRuntimeException.class);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    Mockito.when(this.itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(false);
    Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated,
        ItemServiceImplTest.USERNAME, false, false, false);
    verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
    Mockito.verify(this.productService)
        .getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
    Mockito.verify(this.itemViewConfigService)
        .isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
            this.itemUnSyncTobeUpdated.getItemViewConfigs());
    verify(this.saveOperationService).saveItem(this.itemUnSyncTobeUpdated, new ArrayList<>(), new ArrayList<>());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
    Mockito.verify(this.masterDataCacheService)
        .getMasterDataProductAndItems(REQUEST_ID, USERNAME, PRODUCT_CODE, Boolean.FALSE);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
        Arrays.asList(itemPickupPoint));
    verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
        eq(Arrays.asList(itemPickupPoint)));
    assertNotNull(result);
  }

  @Test
  public void isPriceEditDisabled_MerchantPromoEnabledTest() {
    prices = new HashSet<>();
    item.setPrice(prices);
    item.setMerchantPromoDiscount(true);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_CampaignTest() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(PRODUCT_CODE);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(false);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_BlibliSubsidyCampaignTest() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(false);
    assertFalse(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_BlibliSubsidyPlusMPDTest() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(true);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsTest() {
    prices = new HashSet<>();
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
    item.setMerchantPromoDiscount(false);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsTest1() {
    prices = new HashSet<>();
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
    item.setMerchantPromoDiscount(true);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsExpired() {
    prices = new HashSet<>();
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
    item.setMerchantPromoDiscount(false);
    assertFalse(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsExpiredPlusMPDTrue() {
    prices = new HashSet<>();
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
    item.setMerchantPromoDiscount(true);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsUpcoming() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(false);
    assertFalse(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MultipleCampaignsUpcomingPlusMPDTrue() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));
    price.setListOfDiscountPrices(Arrays.asList(discountPrice, discountPrice1));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(true);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabledEmptyDiscountPrices() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(true);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabledEmptyDiscountPricesFalseTest() {
    prices = new HashSet<>();
    Price price = new Price();
    price.setOfferPrice(100);
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode(null);
    discountPrice.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(1))));
    discountPrice.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    DiscountPrice discountPrice1 = new DiscountPrice();
    discountPrice1.setCampaignCode(PRODUCT_CODE);
    discountPrice1.setStartDateTime(Date.from(Instant.now().minus(Duration.ofDays(-1))));
    discountPrice1.setEndDateTime(Date.from(Instant.now().minus(Duration.ofDays(-2))));
    item.setPrice(new HashSet<>(Arrays.asList(price)));
    item.setMerchantPromoDiscount(false);
    assertFalse(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_CampaignActiveTest() {
    discountPrice.setStartDateTime(new Date(System.currentTimeMillis() - 100000));
    discountPrice.setEndDateTime(new Date(System.currentTimeMillis() + 100000));
    item.setMerchantPromoDiscount(false);
    item.setPrice(prices);
    assertFalse(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_MerchantPromoEnabledAndCampaignActiveTest() {
    item.setMerchantPromoDiscount(true);
    item.setPrice(prices);
    assertTrue(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void isPriceEditDisabled_FalseTest() {
    item.setMerchantPromoDiscount(false);
    prices = new HashSet<>();
    prices.add(price2);
    item.setPrice(prices);
    assertFalse(itemServiceImpl.isPriceEditDisabled(item));
  }

  @Test
  public void updateSubscriptionFlagByItemSkuTest() {
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(this.saveAndPublishService.saveItems(Arrays.asList(item))).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(this.cacheEvictHelperService).evictItemCache(item.getStoreId(), item);
    itemServiceImpl.updateSubscriptionFlagByItemSku(STORE_ID, ITEM_SKU, true, null);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(this.saveAndPublishService).saveItems(Arrays.asList(item));
    Mockito.verify(this.cacheEvictHelperService).evictItemCache(item.getStoreId(), item);
  }

  @Test
  public void updateSubscriptionFlagByItemSku_nullItemTest() {
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(null);
    itemServiceImpl.updateSubscriptionFlagByItemSku(STORE_ID, ITEM_SKU, true, new HashSet<>());
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void updateItemTest_isForceReviewTrue() throws Exception {
    this.itemUnSyncTobeUpdated.setForceReview(true);
    try {
      Item result = this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated,
          ItemServiceImplTest.USERNAME, false, false, false);
    } catch (ApplicationRuntimeException e) {

    } finally {
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
          ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
      Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
    }
  }

  @Test
  public void processPromoBundlingStatusChangedEventTest() {
    Mockito.when(saveOperationService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE))
        .thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEvent(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true, null);
    Mockito.verify(saveOperationService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE);
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedFalseTest() {
    Mockito.when(saveOperationService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE))
        .thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEvent(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true, false);
    Mockito.verify(saveOperationService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), false, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), false);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedTrueAndPromoBundlingFalseTest() {
    Mockito.when(saveOperationService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE))
        .thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEvent(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false, true);
    Mockito.verify(saveOperationService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), true, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), true);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedNullAndPromoBundlingFalseTest() {
    Mockito.when(saveOperationService.removeActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE))
        .thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEvent(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false, null);
    Mockito.verify(saveOperationService).removeActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void processPromoBundlingStatusChangedEventInItemPickupPointTest() {
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true, null);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
  }

  @Test
  public void processPromoBundlingStatusChangedEventInItemPickupPointNULLTest() {
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false))
        .thenReturn(null);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true, null);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false);
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedFalseInItemPickupPointTest() {
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true, false);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), false, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), false);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedTrueAndPromoBundlingFalseInItemPickupPointTest() {
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE, false))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false, true);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE, false);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), true, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), true);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedFalseItemPickupPointTest() {
    itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE, true))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU,
        Constants.WHOLESALE_PRICE, false, false);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE, true);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), false, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), false);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedNullAndPromoBundlingFalseInItemPickupPointTest() {
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true))
        .thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPoint(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, false, null);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundling(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
  }

  @Test
  public void processPromoBundlingStatusChangedEventInItemPickupPointByPPCodeTest() {
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE,
            false, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, true, null, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false,
            PICKUP_POINT_CODE);
  }

  @Test
  public void processPromoBundlingStatusChangedEventInItemPickupPointNULLByPPCodeTest() {
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE,
            false, PICKUP_POINT_CODE)).thenReturn(null);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, true, null, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false,
            PICKUP_POINT_CODE);
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedFalseInItemPickupPointByPPCodeTest() {
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE,
            false, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, true, false, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, false,
            PICKUP_POINT_CODE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), false, item.getMerchantCode());
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedTrueAndPromoBundlingFalseInItemPickupPointByPPCodeTest() {
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE,
            false, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, false, true, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, Constants.WHOLESALE_PRICE, false,
            PICKUP_POINT_CODE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), true, item.getMerchantCode());
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedFalseItemPickupPointByPPCodeTest() {
    itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE,
            true, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, false, false, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true,
            PICKUP_POINT_CODE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), false, item.getMerchantCode());
  }

  @Test
  public void processPromoBundlingStatusChangedEventWithWHPriceActivatedNullAndPromoBundlingFalseInItemPickupPointByPPCodeTest() {
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE,
            true, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    itemServiceImpl.processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(STORE_ID, MAIN_ITEM_SKU,
        PROMO_BUNDLING_TYPE, false, null, PICKUP_POINT_CODE);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, MAIN_ITEM_SKU, PROMO_BUNDLING_TYPE, true,
            PICKUP_POINT_CODE);
  }

  @Test
  public void getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseTest()
      throws Exception {
    Set<String> pristineIds = new HashSet<>();
    pristineIds.add(PRISTINE_ID);
    List<PristineDataItem> pristineItems = new ArrayList<>();
    pristineItems.add(new PristineDataItem());
    Mockito.when(pristineItemRepository.findByPristineIdIn(pristineIds)).thenReturn(pristineItems);
    Mockito.when(itemRepository.findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(
        STORE_ID, pristineItems)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        pristineIds);
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    Mockito.verify(itemRepository)
        .findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
            pristineItems);
  }

  @Test
  public void addItemPriceTest() {
    Price price = new Price();
    Set<String> pristineIds = new HashSet<>();
    pristineIds.add(PRISTINE_ID);
    List<PristineDataItem> pristineItems = new ArrayList<>();
    pristineItems.add(new PristineDataItem());
    Mockito.when(saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    Mockito.doNothing().when(productAndItemSolrIndexerService).updateSolrOnPriceChange(anyList());
    Mockito.when(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    itemServiceImpl.addItemPrice(STORE_ID, price, ITEM_SKU, USERNAME);
    Mockito.verify(saveOperationService).saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPriceChange(anyList());
  }

  @Test
  public void getItemsByMerchantSkuAndMerchantCodeTest() throws Exception {
    Set<String> pristineIds = new HashSet<>();
    pristineIds.add(PRISTINE_ID);
    Mockito.when(itemRepository.findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_SKU))
        .thenReturn(Arrays.asList(item));
    Mockito.when(productService.getProductsByMerchantCode(STORE_ID, MERCHANT_CODE)).thenReturn(Arrays.asList(product));
    itemServiceImpl.getItemsByMerchantSkuAndMerchantCode(STORE_ID, MERCHANT_SKU, MERCHANT_CODE);
    Mockito.verify(itemRepository).findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(STORE_ID, MERCHANT_SKU);
    Mockito.verify(productService).getProductsByMerchantCode(STORE_ID, MERCHANT_CODE);
    PristineDataItem pristineDataItem = new PristineDataItem();
    when(pristineItemRepository.findByPristineIdIn(pristineIds)).thenReturn(Arrays.asList(pristineDataItem));
    when(itemRepository.findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(
        STORE_ID, Arrays.asList(pristineDataItem))).thenReturn(Arrays.asList(item));
    List<Item> items = itemServiceImpl.getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        pristineIds);
    assertEquals(ITEM_SKU, items.get(0).getItemSku());
    Mockito.verify(pristineItemRepository).findByPristineIdIn(pristineIds);
    Mockito.verify(itemRepository)
        .findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
            Arrays.asList(pristineDataItem));
  }

  @Test
  public void updateEtdNoteTest() {
    itemServiceImpl.updateEtdNote(STORE_ID, ITEM_SKU, StringUtils.SPACE);
  }

  @Test
  public void getItemPriceAndOff2OnChannelActiveTest() {
    List<String> itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(itemRepository.getPriceAndOff2OnChannelActive(STORE_ID, itemSkus)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemPriceAndOff2OnChannelActive(STORE_ID, itemSkus);
    Mockito.verify(itemRepository).getPriceAndOff2OnChannelActive(STORE_ID, itemSkus);
  }

  @Test
  public void getItemPriceAndViewConfigsTest() {
    List<String> itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(itemRepository.getPriceAndViewConfigs(STORE_ID, itemSkus)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemPriceAndViewConfigs(STORE_ID, itemSkus);
    Mockito.verify(itemRepository).getPriceAndViewConfigs(STORE_ID, itemSkus);
  }

  @Test
  public void addItemTest_emptyExistingItems() throws Exception {
    Pageable pageable = PageRequest.of(0, 10);
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, ITEM_CODE, pageable))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU)).thenReturn(this.productWithProductAttribute);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(Item.class))).thenReturn(itemWithGeneratedItemSku);

    boolean result =
        this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO);

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, ITEM_CODE, pageable);
    Mockito.verify(this.productHelperService).containDefaultChannelPrice(this.itemRequestVO);
    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(this.productHelperService)
        .setItemDetail(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.MERCHANT_CODE,
            ItemServiceImplTest.SIZE_OF_PRODUCT_ATTRIBUTES, this.itemRequestVO);
    Mockito.verify(this.productHelperService)
        .setMasterDataItemFromMasterData(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, this.itemWithGeneratedItemSku);
    Mockito.verify(this.itemPriceService)
        .publishItemPriceChangeEvent(ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID,
            this.productWithProductAttribute, this.itemWithGeneratedItemSku);
    Mockito.verify(this.productHelperService).addItemAttributeToProductAttribute(this.productWithProductAttribute, this.itemWithGeneratedItemSku.getItemSku(),
        this.itemWithGeneratedItemSku.getMasterDataItem().getMasterDataItemAttributeValues());
    Mockito.verify(this.saveOperationService).saveProductAndItems(productAndItemsVOArgumentCaptor.capture(),
        eq(new ArrayList<>()));
    Mockito.verify(this.saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    assertTrue(result);
    assertNotNull(productAndItemsVOArgumentCaptor);
    ProductAndItemsVO productAndItemsVO = productAndItemsVOArgumentCaptor.getValue();
    assertEquals(this.productWithProductAttribute, productAndItemsVO.getProduct());
    assertEquals(this.itemWithGeneratedItemSku, productAndItemsVO.getItems().get(0));
    assertEquals(ITEM_SKU, productAndItemsVO.getItems().get(0).getItemSku());
  }


  @Test
  public void addItemTest_nullPristineMapping() throws Exception {
    pageOfItems.getContent().get(0).setPristineDataItem(null);
    Pageable pageable = PageRequest.of(0, 10);
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, ITEM_CODE, pageable))
        .thenReturn(pageOfItems);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU)).thenReturn(this.productWithProductAttribute);
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(Item.class))).thenReturn(itemWithGeneratedItemSku);

    boolean result =
        this.itemServiceImpl.addItem(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID, ItemServiceImplTest.USERNAME, ItemServiceImplTest.PRODUCT_SKU, this.itemRequestVO);

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(STORE_ID, ITEM_CODE, pageable);
    Mockito.verify(this.productHelperService).containDefaultChannelPrice(this.itemRequestVO);
    Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(this.productHelperService)
        .setItemDetail(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.MERCHANT_CODE,
            ItemServiceImplTest.SIZE_OF_PRODUCT_ATTRIBUTES, this.itemRequestVO);
    Mockito.verify(this.productHelperService)
        .setMasterDataItemFromMasterData(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.REQUEST_ID,
            ItemServiceImplTest.USERNAME, this.itemWithGeneratedItemSku);
    Mockito.verify(this.itemPriceService)
        .publishItemPriceChangeEvent(ItemServiceImplTest.USERNAME, ItemServiceImplTest.REQUEST_ID,
            this.productWithProductAttribute, this.itemWithGeneratedItemSku);
    Mockito.verify(this.productHelperService).addItemAttributeToProductAttribute(this.productWithProductAttribute, this.itemWithGeneratedItemSku.getItemSku(),
        this.itemWithGeneratedItemSku.getMasterDataItem().getMasterDataItemAttributeValues());
    Mockito.verify(this.saveOperationService).saveProductAndItems(productAndItemsVOArgumentCaptor.capture(),
        eq(new ArrayList<>()));
    Mockito.verify(this.saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    assertTrue(result);
    assertNotNull(productAndItemsVOArgumentCaptor);
    ProductAndItemsVO productAndItemsVO = productAndItemsVOArgumentCaptor.getValue();
    assertEquals(this.productWithProductAttribute, productAndItemsVO.getProduct());
    assertEquals(this.itemWithGeneratedItemSku, productAndItemsVO.getItems().get(0));
    assertEquals(ITEM_SKU, productAndItemsVO.getItems().get(0).getItemSku());
  }

  @Test
  public void updateDangerousGoodsLevelTest() throws Exception {
    item.setItemCode(null);
    productAndItemSolr = new ProductAndItemSolr();
    productAndItemSolr.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Map<Integer, Set<String>> dgLevel = new HashMap<>();
    dgLevel.put(2, itemSkus);
    when(productSearchService.getItemSkuAndCodeByStoreIdAndItemSkus(STORE_ID, itemSkus)).thenReturn(Arrays.asList(productAndItemSolr));
    when(saveOperationService.updateItemDGLevel(STORE_ID, itemSkus, 2)).thenReturn(Arrays.asList(item));
    itemServiceImpl.updateDangerousGoodsLevel(STORE_ID, dgLevel);
    Mockito.verify(productSearchService).getItemSkuAndCodeByStoreIdAndItemSkus(STORE_ID, itemSkus);
    Mockito.verify(saveOperationService).updateItemDGLevel(STORE_ID, itemSkus, 2);
  }

  @Test
  public void updateUnsynItemWithGenerateShippingWeightProductTypeRegularException() throws Exception {
    ReflectionTestUtils.setField(this.itemServiceImpl, "generateShippingWeight", Boolean.TRUE);
    ReflectionTestUtils.setField(this.itemServiceImpl, "maxShippingWeight", 50);
    Category category = new Category();
    category.setCategoryCode(CATEGORY_CODE_FOR_UNSYNC_CHECK);
    product.getMasterDataProduct().setMasterCatalog(new MasterCatalog());
    product.getMasterDataProduct().getMasterCatalog().setCategory(category);
    product.setProductType(ProductType.REGULAR);
    itemUnsyncUpdated.getMasterDataItem().setItemLength(2.0);
    itemUnsyncUpdated.getMasterDataItem().setItemWidth(5.0);
    itemUnsyncUpdated.getMasterDataItem().setItemHeight(4.0);
    itemUnsyncUpdated.getMasterDataItem().setItemWeight(11.0);
    when(this.itemHelperService.setItemPriceByChannel(this.itemUnSyncTobeUpdated, this.itemUnsyncUpdated.getPrice(),
        ItemServiceImplTest.USERNAME)).thenReturn(this.itemUnSyncTobeUpdated);
    Mockito.when(this.productService.getProduct(ItemServiceImplTest.STORE_ID, itemUnSyncTobeUpdated.getProductSku()))
        .thenReturn(product);
    listOfItems.add(itemUnSyncTobeUpdated);
    Mockito.when(this.itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU, false, false, false)).thenReturn(this.listOfItems);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(item);
    when(productService.generateShippingWeight(STORE_ID, CATEGORY_CODE_FOR_UNSYNC_CHECK, 2.0, 5.0, 4.0, 11.0)).thenReturn(122.0);
    Mockito.when(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(this.itemUnSyncTobeUpdated,
        this.itemUnSyncTobeUpdated.getItemViewConfigs())).thenReturn(true);
    try {
      this.itemServiceImpl.updateItem(ItemServiceImplTest.STORE_ID, this.itemUnsyncUpdated, ItemServiceImplTest.USERNAME, false, true, false);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(this.productService).getProduct(ItemServiceImplTest.STORE_ID, this.itemUnSyncTobeUpdated.getProductSku());
      verify(this.productHelperService).updateItemViewConfigForExistingChannel(this.itemUnSyncTobeUpdated, this.itemUnSyncTobeUpdated.getItemViewConfigs());
      verify(this.itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(ItemServiceImplTest.STORE_ID,
          ItemServiceImplTest.ITEM_SKU_UNSYNC_TOBE_UPDATED, false);
      verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID, Constants.CATEGORY_CODE_VARIABLE);
      verify(productService).generateShippingWeight(STORE_ID, CATEGORY_CODE_FOR_UNSYNC_CHECK, 2.0, 5.0, 4.0, 11.0);
      Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU_UNSYNC_TOBE_UPDATED);
      //      Mockito.verify(itemPickupPointService).saveItemPickupPoint(Mockito.anyList());
      verify(objectConverterService).overrideL4DetailsFromL5(Arrays.asList(itemUnSyncTobeUpdated),
          Arrays.asList(itemPickupPoint));
      //      verify(objectConverterService).overrideL5DetailsFromL4(listArgumentCaptor.capture(),
      //          Mockito.eq(Arrays.asList(itemPickupPoint)));
    }
  }

  @Test
  public void getItemsByStoreIdAndItemCodeAndMarkForDeleteFalseTest() {
    Mockito.when(this.itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE))
        .thenReturn(Arrays.asList(item));
    List<Item> items = itemServiceImpl.getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    Mockito.verify(this.itemRepository).findByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    assertFalse(CollectionUtils.isEmpty(items));
    assertEquals(ITEM_SKU, items.get(0).getItemSku());
    assertEquals(PRODUCT_SKU, items.get(0).getProductSku());
    assertEquals(MERCHANT_SKU, items.get(0).getMerchantSku());
  }

  @Test
  public void getItemsWithDiscountPriceTest() {
    Map<String, Set<Price>> priceMap = new HashMap<>();
    priceMap.put(ItemServiceImplTest.ITEM_SKU, new HashSet<Price>());
    itemServiceImpl.getItemsWithDiscountPrice(STORE_ID, USERNAME, REQUEST_ID, Collections.singletonList(item));
    Mockito.verify(this.itemPickupPointService)
        .findByItemSkusAndDelivery(anyString(), anyList(), eq(true));
    Mockito.verify(itemPriceService).getDiscountItemPickupPoint(anyList());
  }

  @Test
  public void publishItemSkusMigratedProductTest() throws Exception {
    productDomainEventModel.setPristineCategory(true);
    productDomainEventModel.setMigratedProduct(true);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    itemsChanged.put(ITEM_CODE, false);
    listOfItems.get(0).setSynchronized(false);
    product.setSynchronized(false);
    Mockito.when(this.productService.findByStoreIdAndProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(eq(STORE_ID), anySet()))
        .thenReturn(listOfItems);
    Mockito.when(saveOperationService.saveProduct(any(Product.class))).thenReturn(product);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSkuIn(eq(STORE_ID), anySet());
    Mockito.verify(this.saveOperationService).saveItems(anyList(), any());
    product.setSynchronized(true);
    Mockito.verify(this.saveOperationService).saveProduct(product);
  }

  @Test
  public void publishItemSkusMigratedProductWithSyncTrueTest() throws Exception {
    productDomainEventModel.setPristineCategory(true);
    productDomainEventModel.setMigratedProduct(true);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    listOfItems.get(0).setSynchronized(true);
    product.setSynchronized(true);
    Mockito.when(this.productService.findByStoreIdAndProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(eq(STORE_ID), anySet()))
        .thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSkuIn(eq(STORE_ID), anySet());
    Mockito.verify(this.saveOperationService).saveItems(anyList(), any());
    Mockito.verify(this.saveOperationService).saveProduct(product);
  }

  @Test
  public void publishItemSkusMigratedProductEmptyProductSkuListTest() throws Exception {
    productDomainEventModel.setPristineCategory(true);
    productDomainEventModel.setMigratedProduct(true);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.productService.findByStoreIdAndProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(new ArrayList<>());
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(eq(STORE_ID), anyString());
  }

  @Test
  public void publishItemSkusMigratedProductEmptyItemSkuListTest() throws Exception {
    productDomainEventModel.setPristineCategory(true);
    productDomainEventModel.setMigratedProduct(true);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    listOfItems.get(0).setSynchronized(true);
    product.setSynchronized(true);
    Mockito.when(this.productService.findByStoreIdAndProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuIn(eq(STORE_ID), anySet()))
        .thenReturn(new ArrayList());
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).findByStoreIdAndProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSkuIn(eq(STORE_ID), anySet());
    Mockito.verify(this.saveOperationService).saveItems(new ArrayList<>(), null);
    Mockito.verify(this.saveOperationService).saveProduct(product);
  }

  @Test
  public void publishItemSkusIsItemUpdatePublishTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(new HashSet<>());
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
    verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture(), anyList(),
        anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH),anyBoolean(), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void publishItemSkusIsItemUpdatePublishItemPickupPointChangeEventOnProductPublishTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
    verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture(), anyList(),
        anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH), anyBoolean(), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnSyncOffTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
    verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture(), anyList(),
        anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH), anyBoolean(), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnSyncOnTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(CATEGORY_ID);
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    productDomainEventModel.setProductCategories(Collections.singletonList(productCategoryDomainEventModel));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnSyncOnCategoryNullTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    productDomainEventModel.setProductCategories(Collections.singletonList(productCategoryDomainEventModel));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnSyncOnCategoryEmptyListTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    productDomainEventModel.setProductCategories(null);
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnBrandChangedTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    productDomainEventModel.setBrand(CNC);
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(CATEGORY_ID);
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    productDomainEventModel.setProductCategories(Collections.singletonList(productCategoryDomainEventModel));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnCategoryChangedTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setBrand(null);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(CATEGORY_ID);
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    productDomainEventModel.setProductCategories(Collections.singletonList(productCategoryDomainEventModel));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    product.setBrand(StringUtils.EMPTY);
    product.setCategoryCode(CAMPAIGN_CODE);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnProductNameTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setBrand(null);
    productDomainEventModel.setName(ADJUSTMENT_NAME);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(CATEGORY_ID);
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    productDomainEventModel.setProductCategories(Collections.singletonList(productCategoryDomainEventModel));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    product.setBrand(StringUtils.EMPTY);
    product.setCategoryCode(CATEGORY_ID);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));

    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void publishItemSkusIsSkipL4PublishSwitchOnNoUpdateTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setBrand(null);
    productDomainEventModel.setName(ADJUSTMENT_NAME);
    productDomainEventModel.setEventTypes(ImmutableSet
        .of(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name()));
    ProductCategoryDomainEventModel productCategoryDomainEventModel = new ProductCategoryDomainEventModel();
    CategoryDomainEventModel categoryDomainEventModel = new CategoryDomainEventModel();
    categoryDomainEventModel.setCategoryCode(CATEGORY_ID);
    productCategoryDomainEventModel.setCategory(categoryDomainEventModel);
    productDomainEventModel.setProductCategories(Collections.singletonList(productCategoryDomainEventModel));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "skipL4EventPublishOnMasterDataChange", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    product.setSynchronized(true);
    product.setBrand(StringUtils.EMPTY);
    product.setCategoryCode(CATEGORY_ID);
    product.setProductName(ADJUSTMENT_NAME);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));

    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
    verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture(), anyList(),
        anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH), anyBoolean(), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void publishItemSkusIsItemUpdatePublishItemPickupPointChangeEventOnProductPublishTrueTest() throws Exception {
    productDomainEventModel.setPristineCategory(false);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(true);
    productDomainEventModel.setEventTypes(ImmutableSet.of(ProductPublishEventType.PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name()));
    ReflectionTestUtils.setField(itemServiceImpl, "publishItemPickupPointChangeEventOnProductPublish", true);
    ReflectionTestUtils.setField(itemServiceImpl, "publishSpecificItemDataDataChangeEvent", true);
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, ImmutableSet.of(ITEM_CODE), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
    verify(this.saveAndPublishService).publishItemDataChangeEvent(itemListCaptor.capture(),
        eq(Constants.SOURCE_PRODUCT_PUBLISH), anyBoolean());
  }

  @Test
  public void publishItemSkusIsItemUpdatePublishTestPristineCategoryNull() throws Exception {
    productDomainEventModel.setPristineCategory(null);
    productDomainEventModel.setMigratedProduct(false);
    productDomainEventModel.setItemUpdatePublish(false);
    productDomainEventModel.setEventTypes(new HashSet<>());
    productDomainEventModel.getUpdatedFields().addAll(Arrays.asList(UPDATED_FIELD_1, UPDATED_FIELD_2));
    Mockito.when(this.productService.getProductsByProductCode(eq(this.STORE_ID), anyString()))
        .thenReturn(Arrays.asList(product));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID),
        anySet())).thenReturn(listOfItems);
    this.itemServiceImpl.publishItemSkus(this.STORE_ID, itemsChanged, new HashSet<>(), productDomainEventModel);
    Mockito.verify(this.productService).getProductsByProductCode(eq(STORE_ID), anyString());
    Mockito.verify(this.itemRepository)
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(eq(STORE_ID), anySet());
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
    verify(this.saveAndPublishService).publishListOfItems(itemListCaptor.capture(), anyList(),
        anyList(), eq(Constants.SOURCE_PRODUCT_PUBLISH), anyBoolean(), eq(Collections.EMPTY_MAP));
    assertEquals(2, itemListCaptor.getValue().get(0).getUpdatedFields().size());
    assertTrue(itemListCaptor.getValue().get(0).getUpdatedFields().contains(UPDATED_FIELD_1));
    assertTrue(itemListCaptor.getValue().get(0).getUpdatedFields().contains(UPDATED_FIELD_2));
    assertFalse(itemListCaptor.getValue().get(0).getUpdatedFields().contains(UPDATED_FIELD_3));
  }

  @Test
  public void updatePristineDPCTest() {
    List<PristineDataItem> pristineDataItems = Collections.singletonList(pristineDataItem);
    when(pristineItemRepository.findByPristineMasterId(PRISTINE_MASTER_ID)).thenReturn(pristineDataItems);
    doNothing().when(pristineItemRepository).updatePristineDPC(pristineDataItems);
    when(itemRepository.findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        pristineDataItems)).thenReturn(listOfItems);
    itemServiceImpl.updatePristineDPC(null, PRISTINE_MASTER_ID, PRODUCT_CODE);
    verify(pristineItemRepository).findByPristineMasterId(PRISTINE_MASTER_ID);
    verify(pristineItemRepository).updatePristineDPC(pristineDataItems);
    verify(itemRepository).findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        pristineDataItems);
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void updatePristineDPCTest_2() {
    doNothing().when(itemRepository).updatePristineDataItem(STORE_ID, item);
    when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(
        item);
    itemServiceImpl.updatePristineDPC(ITEM_SKU, PRISTINE_MASTER_ID, PRODUCT_CODE);
    verify(pristineItemRepository).updatePristineMasterDPC(any(PristineDataItem.class));
    verify(itemRepository).updatePristineDataItem(STORE_ID, item);
    verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    verify(this.saveAndPublishService).publishPristineItem(eq("10001"), any(Item.class));
    verify(cacheEvictHelperService).evictItemData(eq("10001"), any(Item.class));
  }

  @Test
  public void findItemsByStoreIdAndProductSkuTest() {
    Mockito.when(this.itemRepository.findItemsByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID, PRODUCT_SKU))
        .thenReturn(listOfItems);
    this.itemServiceImpl.findItemsByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID, ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void findItemsByStoreIdAndProductSku_whenStoreIdNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findItemsByStoreIdAndProductSku(null, ItemServiceImplTest.PRODUCT_SKU));
  }

  @Test
  public void findItemsByStoreIdAndProductSku_whenProductSkuNullTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.findItemsByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID, null));
  }

  @Test
  public void getProductAndItemsMapTest() {
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false))
        .thenReturn(Arrays.asList(item));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    Map<String, ProductAndItemsVO> response =
        itemServiceImpl.getProductAndItemsMap(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemCacheableService).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    assertEquals(PRODUCT_SKU, response.get(PRODUCT_SKU).getProduct().getProductSku());
    assertEquals(PRODUCT_CODE, response.get(PRODUCT_SKU).getProduct().getProductCode());
    assertEquals(MERCHANT_CODE, response.get(PRODUCT_SKU).getProduct().getMerchantCode());
    assertEquals(PRODUCT_SKU, response.get(PRODUCT_SKU).getProduct().getProductSku());
    assertEquals(PRODUCT_SKU, response.get(PRODUCT_SKU).getItems().get(0).getProductSku());
    assertEquals(ITEM_SKU, response.get(PRODUCT_SKU).getItems().get(0).getItemSku());
    assertEquals(ITEM_CODE, response.get(PRODUCT_SKU).getItems().get(0).getItemCode());
  }

  @Test
  public void getProductAndItemsMap_emptyCacheTest() {
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU)).thenReturn(product);
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false))
        .thenReturn(new ArrayList<>());
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    Map<String, ProductAndItemsVO> response =
        itemServiceImpl.getProductAndItemsMap(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemCacheableService).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false);
    Mockito.verify(itemRepository).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    assertEquals(PRODUCT_SKU, response.get(PRODUCT_SKU).getProduct().getProductSku());
    assertEquals(PRODUCT_CODE, response.get(PRODUCT_SKU).getProduct().getProductCode());
    assertEquals(MERCHANT_CODE, response.get(PRODUCT_SKU).getProduct().getMerchantCode());
    assertEquals(PRODUCT_SKU, response.get(PRODUCT_SKU).getProduct().getProductSku());
    assertEquals(PRODUCT_SKU, response.get(PRODUCT_SKU).getItems().get(0).getProductSku());
    assertEquals(ITEM_SKU, response.get(PRODUCT_SKU).getItems().get(0).getItemSku());
    assertEquals(ITEM_CODE, response.get(PRODUCT_SKU).getItems().get(0).getItemCode());
  }

  @Test
  public void getProductAndItemsMap_exceptionTest() {
    Mockito.when(productService.getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU)).thenThrow(RuntimeException.class);
    Map<String, ProductAndItemsVO> response =
        itemServiceImpl.getProductAndItemsMap(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(productService).getProductDeletedOrUndeleted(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void getItemsByProductSkuFromCacheOrElseDBTest() {
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false))
        .thenReturn(Arrays.asList(item));
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    List<Item> itemList = itemServiceImpl.getItemsByProductSkuFromCacheOrElseDB(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemCacheableService).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false);
    Mockito.verify(itemPickupPointService).findByItemSkuAndDelivery(STORE_ID, ITEM_SKU);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    assertEquals(PRODUCT_SKU, itemList.get(0).getProductSku());
    assertEquals(ITEM_SKU, itemList.get(0).getItemSku());
    assertEquals(ITEM_CODE, itemList.get(0).getItemCode());
  }

  @Test
  public void updatePickupPoints_differentLocation_nullItemTest() throws Exception {
    pickupPointUpdateRequest.setDifferentLocation(true);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false,
        false)).thenReturn(null);
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(
        item);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    Mockito.verify(dataSourceWrapperService)
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false, false);
  }

  @Test
  public void updatePickupPoints_sameLocation_emptyCollectionTest() throws Exception {
    when(dataSourceWrapperService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false,
        false, false)).thenReturn(Collections.singletonList(item));
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(itemPickupPointService.saveItemPickupPoint(anyList())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItem(any(Item.class), any(), anyList())).thenReturn(
        item);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    Mockito.verify(dataSourceWrapperService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false, false, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false);
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(Arrays.asList(itemPickupPoint),
        new ArrayList<>(), Collections.EMPTY_MAP);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(cacheEvictHelperService).evictItemPickupPointData(eq(itemPickupPoint.getStoreId()), any(),
        eq(itemPickupPoint.getPickupPointCode()));
    verify(productAndItemSolrIndexerService).pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, item.getItemSku(),
        itemPickupPoint.getPickupPointCode(), false);
    verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(eq(STORE_ID), anyList());
  }

  @Test
  public void getItemPriceAndViewConfigsAndPromoDetailsTest() {
    List<String> itemSkus = new ArrayList<>();
    itemSkus.add(ITEM_SKU);
    Mockito.when(itemRepository.getItemPriceAndViewConfigsAndPromoDetails(STORE_ID, itemSkus)).thenReturn(Arrays.asList(item));
    itemServiceImpl.getItemPriceAndViewConfigsAndPromoDetails(STORE_ID, itemSkus);
    Mockito.verify(itemRepository).getItemPriceAndViewConfigsAndPromoDetails(STORE_ID, itemSkus);
  }

  @Test
  public void toggleArchiveByProductSkuTest() throws Exception {
    item.setArchived(true);
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID, USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_FALSE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(saveOperationService)
        .saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(itemPickupPointService).updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, Boolean.FALSE, item, Arrays.asList(itemPickupPoint), false);
  }

  @Test
  public void toggleArchiveByProductSkuArchiveTrueTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setMasterDataItem(null);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    editItemResponse = this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID, USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_TRUE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, true, item, Arrays.asList(itemPickupPoint), false);
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(saveOperationService)
        .saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void toggleArchiveByProductSkuArchiveTrueVerifyItem() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setMasterDataItem(null);
    List<Item> updatedItemList = new ArrayList<>();
    updatedItemList.add(item);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    editItemResponse = this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID, USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_TRUE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, true, item, Arrays.asList(itemPickupPoint), false);
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(saveOperationService)
        .saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    assertEquals(editItemResponse.getUpdatedItems(),updatedItemList);
  }

  @Test
  public void toggleArchiveByProductSkuArchive_SameFlagTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setArchived(false);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    try {
      editItemResponse = itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID, USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_FALSE, StringUtils.EMPTY);
    } finally {
      Mockito.verify(this.cacheItemHelperService)
          .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
              ItemServiceImplTest.PRODUCT_SKU);
      Mockito.verify(itemPickupPointService)
          .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
      assertNotNull(editItemResponse.getApiErrorCode());
      assertEquals(ApiErrorCode.TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG,
          editItemResponse.getApiErrorCode());
    }

  }

  @Test
  public void toggleArchiveByProductSkuArchive_mppONTest() throws Exception {
    Item updatedItem = new Item();
    updatedItem.setItemSku(ITEM_SKU);
    updatedItem.setProductSku(PRODUCT_SKU);
    updatedItem.setCncActivated(Boolean.TRUE);
    EditItemResponse editItemResponse = new EditItemResponse();
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    this.itemPickupPointList = Collections.emptyList();
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(updatedItem);
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    editItemResponse = this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID, USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_TRUE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService)
        .saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(productAndItemSolrIndexerService, times(0))
        .updateSolrOnToggleArchiveItemAction(item);
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, Boolean.TRUE, item, Arrays.asList(itemPickupPoint), false);
    assertTrue(editItemResponse.isCncActivated());
  }

  @Test
  public void toggleArchiveByProductSkuArchive_cncFlagTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    Item updatedItem = new Item();
    updatedItem.setProductSku(PRODUCT_SKU);
    updatedItem.setItemSku(ITEM_SKU);
    updatedItem.setCncActivated(true);
    editItemResponse.setCncActivated(updatedItem.isCncActivated());
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    this.itemPickupPointList = Collections.emptyList();
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    when(this.saveOperationService.saveItemsWithoutUpdatingSolr(Arrays.asList(item))).thenReturn(
        Arrays.asList(updatedItem));
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID, USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_TRUE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService)
        .saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, Boolean.TRUE, item, Arrays.asList(itemPickupPoint), false);
    assertTrue(editItemResponse.isCncActivated());
  }

  @Test
  public void toggleArchiveByProductSkuArchive_NoL5Test() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setArchived(false);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Collections.emptyList());
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    try {
      editItemResponse = itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID,
          USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_FALSE, StringUtils.EMPTY);
    } finally {
      Mockito.verify(this.cacheItemHelperService)
          .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
              ItemServiceImplTest.PRODUCT_SKU);
      Mockito.verify(itemPickupPointService)
          .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
      assertNotNull(editItemResponse.getApiErrorCode());
      assertEquals(ApiErrorCode.L5_NOT_PRESENT,
          editItemResponse.getApiErrorCode());
    }
  }

  @Test
  public void toggleArchiveByProductSkuWithArchivedFalseTest() throws Exception {
    item.setArchived(false);
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    item.setCncActivated(false);
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Arrays.asList(itemPickupPoint));
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID,
        USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_FALSE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
  }

  @Test
  public void toggleArchiveByProductSkuWithArchivedTrueTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    Item updatedItem = new Item();
    updatedItem.setProductSku(PRODUCT_SKU);
    updatedItem.setItemSku(ITEM_SKU);
    updatedItem.setCncActivated(true);
    editItemResponse.setCncActivated(updatedItem.isCncActivated());
    item.setPickupPointCode(ItemServiceImplTest.PICKUP_POINT_CODE);
    this.itemPickupPointList = Collections.emptyList();
    when(this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID,
        PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    when(this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(
        anyString(), anyString())).thenReturn(Collections.emptyList());
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class),
        any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    when(this.saveOperationService.saveItemsWithoutUpdatingSolr(Arrays.asList(item))).thenReturn(
        Arrays.asList(updatedItem));
    Mockito.when(
        this.productAndItemSolrRepository.findFirstByStoreIdAndItemSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.ITEM_SKU)).thenReturn(this.productAndItemSolr);
    this.itemServiceImpl.toggleArchiveByProductSku(ItemServiceImplTest.STORE_ID,
        USERNAME, ItemServiceImplTest.PRODUCT_SKU, ItemServiceImplTest.DO_ARCHIVE_TRUE, StringUtils.EMPTY);
    Mockito.verify(this.cacheItemHelperService)
        .findCacheableByStoreIdAndProductSku(ItemServiceImplTest.STORE_ID,
            ItemServiceImplTest.PRODUCT_SKU);
    Mockito.verify(itemPickupPointService)
        .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    Mockito.verify(saveOperationService)
        .saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP));
    Mockito.verify(saveAndPublishService).publishMerchantVoucherViewConfigChange(anyList(), anyList());
    Mockito.verify(itemPickupPointService)
        .updateItemViewConfigByItemSku(new ArrayList<>(), STORE_ID, USERNAME, Boolean.TRUE, item, Collections.emptyList(), false);
    assertTrue(editItemResponse.isCncActivated());
  }



  @Test
  public void getItemsByProductSkuPaginatedTest() throws Exception {
    when(itemRepository.findItemsByStoreIdAndProductSkuIn(ItemServiceImplTest.STORE_ID,
        ImmutableSet.of(ItemServiceImplTest.PRODUCT_SKU), PageRequest.of(0, 10))).thenReturn(new PageImpl<>(Arrays.asList(item), PageRequest.of(0, 10), 1));
    Page<Item> items = this.itemServiceImpl.getItemsByProductSkuPaginated(ItemServiceImplTest.STORE_ID,
        ItemServiceImplTest.PRODUCT_SKU, 0, 10);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSkuIn(ItemServiceImplTest.STORE_ID,
        ImmutableSet.of(ItemServiceImplTest.PRODUCT_SKU), PageRequest.of(0, 10));
  }

  @Test
  public void getItemsByProductSkuPaginatedEmptyItemTest() throws Exception {
    when(itemRepository.findItemsByStoreIdAndProductSkuIn(ItemServiceImplTest.STORE_ID,
        ImmutableSet.of(ItemServiceImplTest.PRODUCT_SKU), PageRequest.of(0, 10))).thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 10), 0));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByProductSkuPaginated(ItemServiceImplTest.STORE_ID,
          ItemServiceImplTest.PRODUCT_SKU, 0, 10));
    } finally {
      Mockito.verify(this.itemRepository).findItemsByStoreIdAndProductSkuIn(ItemServiceImplTest.STORE_ID,
          ImmutableSet.of(ItemServiceImplTest.PRODUCT_SKU), PageRequest.of(0, 10));
    }
  }

  @Test
  public void updateContentChangeTrueTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, productSkus)).thenReturn(Arrays.asList(item));
    when(this.saveOperationService.saveItems(anyList(), any())).thenReturn(Arrays.asList(item));
    itemServiceImpl.updateContentChange(STORE_ID, PRODUCT_SKU, true, true);
    verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        productSkus);
    verify(saveOperationService).saveItemsWithoutUpdatingSolr(anyList());
    verify(productAndItemSolrIndexerService).updateSolrOnContentChange(anyList());
  }

  @Test
  public void updateContentChangeTruePublishFalseTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, productSkus)).thenReturn(Arrays.asList(item));
    when(this.saveOperationService.saveProductAndItemsWithoutPublishingItemChange(any(ProductAndItemsVO.class))).thenReturn(new ProductAndItemsVO());
    itemServiceImpl.updateContentChange(STORE_ID, PRODUCT_SKU, true, false);
    verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        productSkus);
    verify(saveOperationService).saveProductAndItemsWithoutPublishingItemChange(any(ProductAndItemsVO.class));
  }

  @Test
  public void updateContentChangeTrueNullPristineTest() {
    item.setPristineDataItem(null);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, productSkus)).thenReturn(Arrays.asList(item));
    when(this.saveOperationService.saveItems(anyList(), any()))
        .thenReturn(Arrays.asList(item));
    itemServiceImpl.updateContentChange(STORE_ID, PRODUCT_SKU, true, true);
    verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        productSkus);
    verify(saveOperationService).saveItemsWithoutUpdatingSolr(anyList());
  }

  @Test
  public void updateContentChangeFalseTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, productSkus)).thenReturn(Arrays.asList(item));
    when(this.saveOperationService.saveItems(anyList(), any())).thenReturn(Arrays.asList(item));
    itemServiceImpl.updateContentChange(STORE_ID, PRODUCT_SKU, false, true);
    verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        productSkus);
    verify(saveOperationService).saveItemsWithoutUpdatingSolr(anyList());
  }

  @Test
  public void getItemsByProductSkusPaginatedFromCacheTest() {
    PageRequest pageRequest = PageRequest.of(0, 1);
    List<Item> items = new ArrayList<>();
    Item item3 = new Item();
    item3.setItemSku(ITEM_SKU3);
    items.add(item3);
    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU2);
    items.add(item2);
    item.setItemSku(ITEM_SKU);
    items.add(item);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(items);
    Page<Item> itemsPage = itemServiceImpl.getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU, pageRequest);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    assertTrue(org.apache.commons.collections.CollectionUtils.isNotEmpty(itemsPage.getContent()));
    assertEquals(1, itemsPage.getContent().size());
    assertEquals(3, itemsPage.getTotalElements());
  }

  @Test
  public void getItemsByProductSkusPaginatedFromCachePageOneTest() {
    PageRequest pageRequest = PageRequest.of(1, 1);
    List<Item> items = new ArrayList<>();
    Item item3 = new Item();
    item3.setItemSku(ITEM_SKU3);
    items.add(item3);
    Item item2 = new Item();
    item2.setItemSku(ITEM_SKU2);
    items.add(item2);
    item.setItemSku(ITEM_SKU);
    items.add(item);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(items);
    Page<Item> itemsPage = itemServiceImpl.getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU, pageRequest);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    assertTrue(org.apache.commons.collections.CollectionUtils.isNotEmpty(itemsPage.getContent()));
    assertEquals(1, itemsPage.getContent().size());
    assertEquals(3, itemsPage.getTotalElements());
  }

  @Test
  public void getItemsByProductSkusPaginatedEmptyResponseTest() {
    PageRequest pageRequest = PageRequest.of(0, 10);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(new ArrayList<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getItemsByProductSkusPaginated(STORE_ID, PRODUCT_SKU, pageRequest));
    } finally {
      verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    }
  }

  @Test
  public void findByStoreIdAndItemSkuFromCacheTest() {
    when(itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, true, false, null, false)).thenReturn(item);
    Item item = itemServiceImpl.findByStoreIdAndItemSkuFromCache(STORE_ID, ITEM_SKU);
    verify(itemCacheableService).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, true, false, null, false);
  }

  @Test
  public void findListOfItemCodesByItemSkusTest() {
    Map<String, String> response = itemServiceImpl.findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKUS.get(0)));
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    assertTrue(response.isEmpty());
  }

  @Test
  public void findListOfItemCodesByItemSkus1Test() {
    Mockito.when(cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenReturn(item);
    Map<String, String> response = itemServiceImpl.findListOfItemCodesByItemSkus(STORE_ID, Collections.singleton(ITEM_SKUS.get(0)));
    Mockito.verify(cacheItemHelperService).findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU);
    assertEquals(ITEM_SKU, response.get(item.getItemCode()));
    assertTrue(response.containsKey(item.getItemCode()));
  }

  @Test
  public void activateItemsOnNeedCorrectionTest() throws Exception {
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    itemList.add(itemUpdated);
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false))
        .thenReturn(itemList);
    when(objectConverterService.getItemForActivation(item, itemActivationRequest, null)).thenReturn(false);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    itemPickupPoint.setItemSku(item.getItemSku());
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item.getItemSku()))
        .thenReturn(itemPickupPoint);
    itemPickupPoint.setItemSku(itemUpdated.getItemSku());
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, itemUpdated.getItemSku()))
        .thenReturn(itemPickupPoint);
    itemServiceImpl.activateItemsOnNeedCorrection(STORE_ID, PRODUCT_SKU, Arrays.asList(itemActivationRequest));
    Mockito.verify(itemCacheableService).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false);
    Mockito.verify(itemPickupPointService, times(3)).findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    itemPickupPoint.setItemSku(item.getItemSku());
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    itemList.add(itemUpdated);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(itemUpdated), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService).getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
  }

  @Test
  public void activateItemsOnNeedCorrectionTest_PPChange() throws Exception {
    List<Item> itemList = new ArrayList<>();
    itemList.add(item);
    itemList.add(itemUpdated);
    when(dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID, ItemServiceImplTest.ITEM_SKU,
        ItemServiceImplTest.PICKUP_POINT_CODE, false)).thenReturn(itemPickupPoint);
    Mockito.when(itemCacheableService.findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false))
        .thenReturn(itemList);
    when(objectConverterService.getItemForActivation(item, itemActivationRequest, itemPickupPoint)).thenReturn(true);
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoint);
    itemPickupPoint.setItemSku(item.getItemSku());
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, item.getItemSku()))
        .thenReturn(itemPickupPoint);
    itemPickupPoint.setItemSku(itemUpdated.getItemSku());
    Mockito.when(itemPickupPointService.findByItemSkuAndDelivery(STORE_ID, itemUpdated.getItemSku()))
        .thenReturn(itemPickupPoint);
    itemServiceImpl.activateItemsOnNeedCorrection(STORE_ID, PRODUCT_SKU, Arrays.asList(itemActivationRequest));
    Mockito.verify(itemCacheableService).findItemsByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU, false, false, false);
    Mockito.verify(itemPickupPointService, times(3)).findByItemSkuAndDelivery(anyString(), anyString());
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    itemPickupPoint.setItemSku(item.getItemSku());
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    itemList.add(itemUpdated);
    Mockito.verify(objectConverterService)
        .overrideL4DetailsFromL5(Collections.singletonList(itemUpdated), Collections.singletonList(itemPickupPoint));
    verify(objectConverterService).getItemForActivation(item, itemActivationRequest, itemPickupPoint);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, null, false);
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMppTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(true);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(true);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithBuyableOnMppTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(true);
    request1.setCncDiscoverable(false);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        false);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithDisplayOnMppTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setDiscoverable(false);
    request1.setBuyable(false);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setBuyable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        false);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMppTest3() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", false);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(false);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(true);
    request2.setBuyable(false);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    BundleRecipeVo bundleRecipeVo = new BundleRecipeVo();
    bundleRecipeVo.setItemSku(ITEM_SKU);
    bundleRecipeVo.setQuantity(0);
    itemPickupPointNeedrevisonList.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPointNeedrevisonList.get(1).setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemActivationRequests.get(0).setListPrice(price.getListPrice());
    itemActivationRequests.get(0).setOfferPrice(price.getOfferPrice());
    itemActivationRequests.get(1).setListPrice(price.getListPrice());
    itemActivationRequests.get(1).setOfferPrice(price.getOfferPrice());
    itemActivationRequests.get(0).setDiscoverable(itemViewConfig.isDiscoverable());
    itemActivationRequests.get(0).setBuyable(itemViewConfig.isBuyable());
    itemActivationRequests.get(1).setDiscoverable(itemViewConfig.isDiscoverable());
    itemActivationRequests.get(1).setBuyable(itemViewConfig.isBuyable());
    itemActivationRequests.get(0).setBundleRecipe(ImmutableSet.of(bundleRecipeVo));
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    itemUpdated.setPermanentDelete(true);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item, itemUpdated));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(3)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(3)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(3)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMppSingleL5DeliveryTest() throws Exception {
    List<ItemPickupPoint> itemPickupPoints1 = new ArrayList<>();
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    itemPickupPoint1.setItemSku(ITEM_SKU);
    itemPickupPoint1.setItemViewConfig(itemViewConfigs);
    itemPickupPoint1.setPickupPointCode(PICKUP_POINT_CODE);
    Price price3 = new Price();
    price3.setChannel(ItemServiceImplTest.CHANNEL_DEFAULT);
    price3.setOfferPrice(100000.0);
    price3.setListPrice(10000.0);
    price3.setListOfDiscountPrices(discountPrices);
    price3.setCurrency(CURRENCY);
    Set<Price> prices1 = new HashSet<>();
    prices1.add(price3);
    itemPickupPoint1.setPrice(prices1);
    itemPickupPoints1.add(itemPickupPoint1);
    List<NeedCorrectionItemActivationRequest> requests = new ArrayList<>();
    NeedCorrectionItemActivationRequest request = new NeedCorrectionItemActivationRequest();
    request.setItemSku(ITEM_SKU);
    request.setPickupPointCode(PICKUP_POINT_CODE_1);
    requests.add(request);
    requests.get(0).setListPrice(price.getListPrice());
    requests.get(0).setOfferPrice(price.getOfferPrice());
    requests.get(0).setDiscoverable(itemViewConfig.isDiscoverable());
    requests.get(0).setBuyable(itemViewConfig.isBuyable());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    itemUpdated.setPermanentDelete(true);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item, itemUpdated));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPoints1);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPoints1.get(0));
    EditItemResponse response = itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, requests,
        true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(1)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    assertTrue(response.getUpdatedItemPickupPoints().get(0).isDelivery());
  }
  @Test
  public void activateItemsOnNeedCorrectionWithMppWithDeliveryTrueTest() throws Exception {
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    itemPickupPointNeedrevisonList.get(0).setDelivery(true);
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        false);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(objectConverterService).overrideL4DetailsFromL5(anyList(), anyList());
  }

  @Test
  public void getItemsByStoreIdAndItemSkusAndMarkForDeleteFalseTest() {
    Set<String> itemSkuSet = new HashSet<>();
    itemSkuSet.add(ITEM_SKU);
    itemSkuSet.add(ITEM_SKU2);
    Mockito.when(this.itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkuSet))
        .thenReturn(Arrays.asList(item, item));
    List<Item> response = this.itemServiceImpl.getItemsByStoreIdAndItemSkus(STORE_ID, itemSkuSet);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSkuSet);
    assertEquals(ITEM_SKU, response.get(0).getItemSku());
    assertEquals(ITEM_SKU, response.get(1).getItemSku());
  }

  @Test
  public void getItemsByItemSkusTest() {
    String[] fields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME};
    when(this.itemRepository.findByStoreIdAndItemSkus(STORE_ID, itemSkus, fields)).thenReturn(new ArrayList<>());
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    this.itemServiceImpl.getItemsByItemSkus(STORE_ID, itemSkus, fields);
    verify(this.itemRepository).findByStoreIdAndItemSkus(STORE_ID, itemSkus, fields);
  }

  @Test
  public void getItemsByItemSkusStoreIdBlankTest() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    String[] fields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME};
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByItemSkus(StringUtils.EMPTY, itemSkus, fields));
  }

  @Test
  public void getItemsByItemSkusItemSkuSetEmptyTest() {
    String[] fields = {ProductFieldNames.ITEM_SKU, ProductFieldNames.GENERATED_ITEM_NAME};
    Set<String> itemSkuSet = new HashSet<>();
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByItemSkus(STORE_ID, itemSkuSet, fields));
  }

  @Test
  public void getItemsByItemSkusFieldsNullTest() {
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getItemsByItemSkus(STORE_ID, itemSkus, null));
  }

  @Test
  public void getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse_emptySet() {
    List<Item> response = this.itemServiceImpl.getItemsByStoreIdAndItemSkus(STORE_ID, Collections.emptySet());
    assertTrue(CollectionUtils.isEmpty(response));
  }

  @Test
  public void getItemsByOfflineItemIdsTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    BeanUtils.copyProperties(itemPickupPoint, offlineItem);
    offlineItem.setMarkForDelete(true);
    offlineItem.setNewData(false);
    offlineItem.setOfferPrice(itemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
    offlineItem.setListPrice(itemPickupPoint.getPrice().stream().findFirst().get().getListPrice());
    Mockito.when(
        this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKUS)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS))).thenReturn(Arrays.asList(item));
    List<Item> itemList = itemServiceImpl.getItemsByOfflineItemIds(STORE_ID, ITEM_SKUS, true);
    Mockito.verify(this.productHelperService).constructOfflineItem(item, offlineItem);
    Mockito.verify(this.itemPickupPointService)
        .findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS));
    assertEquals(ITEM_SKU, itemList.get(0).getItemSku());
    assertTrue(CollectionUtils.isEmpty(itemList.get(0).getOfflineItems()));
  }

  @Test
  public void getItemsByOfflineItemIds_setOfflineItemTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    Mockito.when(
        this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKUS)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS))).thenReturn(Arrays.asList(item));
    List<Item> itemList = itemServiceImpl.getItemsByOfflineItemIds(STORE_ID, ITEM_SKUS, false);
    Mockito.verify(this.productHelperService).constructOfflineItemForTransaction(item, itemPickupPoint);
    Mockito.verify(this.itemPickupPointService)
        .findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS);
    Mockito.verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS));
    assertEquals(ITEM_SKU, itemList.get(0).getItemSku());
  }

  @Test
  public void getItemsByOfflineItemIds_emptyOfflineItemsTest() {
    itemPickupPoint.setItemSku(ITEM_SKU);
    Mockito.when(
        this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKUS)).thenReturn(null);
    try {
      List<Item> itemList = itemServiceImpl.getItemsByOfflineItemIds(STORE_ID, ITEM_SKUS, true);
    } finally {
      Mockito.verify(this.itemPickupPointService)
          .findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS);
    }
  }

  @Test
  public void getItemsByOfflineItemIds_emptyItemsTest() {
    offlineItem.setItemSku(ITEM_SKU);
    Mockito.when(
        this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID,
            ITEM_SKUS)).thenReturn(Collections.singletonList(itemPickupPoint));
    Mockito.when(this.itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS))).thenReturn(null);
    try {
      List<Item> itemList = itemServiceImpl.getItemsByOfflineItemIds(STORE_ID, ITEM_SKUS, true);
    } finally {
      Mockito.verify(this.itemPickupPointService)
          .findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(STORE_ID, ITEM_SKUS);
      Mockito.verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS));
    }
  }

  @Test
  public void getItemDetailsByItemCodesTest() {
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setBusinessPartnerCode(item.getMerchantCode());
    businessPartner.setMerchantType(MERCHANT_TYPE);
    List<BusinessPartner> businessPartnerList = new ArrayList<>();
    businessPartnerList.add(null);
    businessPartnerList.add(businessPartner);
    Mockito.when(itemRepository.getItemDetailsByItemCodes(Collections.singleton(ITEM_SKU)))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(businessPartnerService.findByStoreIdAndBusinessPartnerCodes(STORE_ID, new SimpleListStringRequest(Collections.singletonList(item.getMerchantCode()))))
        .thenReturn(businessPartnerList);
    List<ItemCodeDetailResponse> response =
        itemServiceImpl.getItemDetailsByItemCodes(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemRepository).getItemDetailsByItemCodes(Collections.singleton(ITEM_SKU));
    Mockito.verify(businessPartnerService).findByStoreIdAndBusinessPartnerCodes(STORE_ID,
        new SimpleListStringRequest(Collections.singletonList(item.getMerchantCode())));
    assertEquals(1, response.size());
    assertEquals(item.getProductSku(), response.get(0).getProductSku());
    assertEquals(item.getItemCode(), response.get(0).getItemCode());
    assertEquals(item.getMerchantCode(), response.get(0).getMerchantCode());
    assertEquals(businessPartner.getMerchantType(), response.get(0).getMerchantType());
  }

  @Test
  public void getItemDetailsByItemCodesMultipleTest() {
    BusinessPartner businessPartner = new BusinessPartner();
    businessPartner.setBusinessPartnerCode(item.getMerchantCode());
    businessPartner.setMerchantType(MERCHANT_TYPE);
    BusinessPartner businessPartner1 = new BusinessPartner();
    businessPartner1.setBusinessPartnerCode(item.getMerchantCode());
    businessPartner1.setMerchantType(MERCHANT_TYPE);
    List<BusinessPartner> businessPartnerList = new ArrayList<>();
    businessPartnerList.add(businessPartner);
    businessPartnerList.add(businessPartner1);
    Mockito.when(itemRepository.getItemDetailsByItemCodes(Collections.singleton(ITEM_SKU)))
        .thenReturn(Collections.singletonList(item));
    Mockito.when(businessPartnerService.findByStoreIdAndBusinessPartnerCodes(STORE_ID, new SimpleListStringRequest(Collections.singletonList(item.getMerchantCode()))))
        .thenReturn(businessPartnerList);
    List<ItemCodeDetailResponse> response =
        itemServiceImpl.getItemDetailsByItemCodes(STORE_ID, Collections.singleton(ITEM_SKU));
    Mockito.verify(itemRepository).getItemDetailsByItemCodes(Collections.singleton(ITEM_SKU));
    Mockito.verify(businessPartnerService).findByStoreIdAndBusinessPartnerCodes(STORE_ID,
        new SimpleListStringRequest(Collections.singletonList(item.getMerchantCode())));
    assertEquals(1, response.size());
    assertEquals(item.getProductSku(), response.get(0).getProductSku());
    assertEquals(item.getItemCode(), response.get(0).getItemCode());
    assertEquals(item.getMerchantCode(), response.get(0).getMerchantCode());
    assertEquals(businessPartner.getMerchantType(), response.get(0).getMerchantType());
  }

  @Test
  public void updateMerchantDiscountPriceAndPublishEvent() throws Exception {
    when(this.itemRepository.updateItemMerchantDiscountPrice(ItemServiceImplTest.STORE_ID, item)).thenReturn(item);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(item);
    this.itemServiceImpl.updateMerchantDiscountPriceAndPublishEvent(STORE_ID, ITEM_SKU, null, false);
    verify(this.itemRepository).updateItemMerchantDiscountPrice(ItemServiceImplTest.STORE_ID, item);
    verify(this.saveAndPublishService).publishMerchantPromoDiscountEventChange(item);
    verify(this.productAndItemSolrIndexerService).indexMerchantPromoDiscountItem(item, false);
    verify(this.cacheEvictHelperService).evictItemData(item.getStoreId(), item);
    Mockito.verify(this.itemCacheableService)
        .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);
  }

  @Test
  public void updateMerchantDiscountPriceAndPublishEventNullItem() throws Exception {
    when(this.itemRepository.updateItemMerchantDiscountPrice(ItemServiceImplTest.STORE_ID, item)).thenReturn(item);
    Mockito.when(this.itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false))
        .thenReturn(null);
    try {
      this.itemServiceImpl.updateMerchantDiscountPriceAndPublishEvent(STORE_ID, ITEM_SKU, null, false);
    } catch (Exception e) {
    } finally {
      Mockito.verify(this.itemCacheableService)
          .findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, true, false, false, null, false);

    }
  }

  @Test
  public void updateProductTypeOrContentChangeTest() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(productService.findByStoreIdAndProductCode(anyString(), anyString())).thenReturn(Arrays.asList(product));
    ProductTypeEditRequest productTypeEditRequest =
        ProductTypeEditRequest.builder().productType(ProductType.BOPIS).contentChanged(true).productCode(PRODUCT_CODE)
            .productSku(PRODUCT_SKU).build();
    itemServiceImpl.updateProductTypeOrContentChange(productTypeEditRequest, STORE_ID);
    verify(productService).findByStoreIdAndProductCode(anyString(), anyString());
  }

  @Test
  public void updateContentChangeTrueTest_NoProductTypeChange() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, productSkus)).thenReturn(Arrays.asList(item));
    when(this.saveOperationService.saveItems(anyList(), any())).thenReturn(Arrays.asList(item));
    ProductTypeEditRequest productTypeEditRequest =
        ProductTypeEditRequest.builder().productType(null).contentChanged(true).productCode(PRODUCT_CODE)
            .productSku(PRODUCT_SKU).build();
    itemServiceImpl.updateProductTypeOrContentChange(productTypeEditRequest, STORE_ID);
    verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID,
        productSkus);
    verify(saveOperationService).saveItemsWithoutUpdatingSolr(anyList());
    verify(productAndItemSolrIndexerService).updateSolrOnContentChange(anyList());
  }

  @Test
  public void updateProductTypeOrContentChangeTest_2() {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU);
    product.setSynchronized(true);
    when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(STORE_ID, productSkus)).thenReturn(Arrays.asList(item));
    when(productService.findByStoreIdAndProductCode(anyString(), anyString())).thenReturn(Arrays.asList(product));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(Arrays.asList(item));
    ProductTypeEditRequest productTypeEditRequest =
        ProductTypeEditRequest.builder().productType(ProductType.BOPIS).productCode(PRODUCT_CODE)
            .productSku(PRODUCT_SKU).contentChanged(true).build();
    itemServiceImpl.updateProductTypeOrContentChange(productTypeEditRequest, STORE_ID);
    verify(productService).findByStoreIdAndProductCode(anyString(), anyString());
    verify(saveOperationService).saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(productAndItemSolrIndexerService).updateSolrOnProductTypeChange(anyList(), any());
    verify(saveOperationService).saveItemsWithoutUpdatingSolr(anyList());
  }

  @Test
  public void updateProductTypeOrContentChangeTest_Null() {
    ProductTypeEditRequest productTypeEditRequest =
        ProductTypeEditRequest.builder().productType(null).contentChanged(false).productCode(PRODUCT_SKU).build();
    itemServiceImpl.updateProductTypeOrContentChange(productTypeEditRequest, STORE_ID);
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode("CAMPAIGN_CODE");
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(true);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    boolean result = itemServiceImpl.isPriceEditDisabled(itemPickupPoint);

    assertTrue(result);
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointMerchantPromoDiscountFalseTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode("CAMPAIGN_CODE");
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    boolean result = itemServiceImpl.isPriceEditDisabled(itemPickupPoint);

    assertTrue(result);
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointInvalidEndDateTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode("CAMPAIGN_CODE");
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), -1));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    boolean result = itemServiceImpl.isPriceEditDisabled(itemPickupPoint);

    assertFalse(result);
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointInvalidStartDateTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setCampaignCode("CAMPAIGN_CODE");
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), -1));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    boolean result = itemServiceImpl.isPriceEditDisabled(itemPickupPoint);

    assertFalse(result);
  }


  @Test
  public void getPriceEditDisabledReasonsItemPickupPointCampaignCodeNullTest() {
    DiscountPrice discountPrice = new DiscountPrice();
    discountPrice.setStartDateTime(DateUtils.addDays(new Date(), -1));
    discountPrice.setEndDateTime(DateUtils.addDays(new Date(), 2));

    Price price = new Price();
    price.setListOfDiscountPrices(Arrays.asList(discountPrice));

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setMerchantPromoDiscount(false);
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    boolean result = itemServiceImpl.isPriceEditDisabled(itemPickupPoint);

    assertFalse(result);
  }

  @Test
  public void getPriceEditDisabledReasonsItemPickupPointMerchantPromoFalseTest() {
    Price price = new Price();

    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setPrice(ImmutableSet.of(price));

    boolean result = itemServiceImpl.isPriceEditDisabled(itemPickupPoint);

    assertFalse(result);
  }

  @Test
  public void activeDeactivatePromoBundlingTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(true);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType("WHOLE_SALE");
    itemServiceImpl.activeDeactivatePromoBundling(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(businessPartnerPromoService)
        .upsertBusinessPartnerPromo(promoBundlingActivatedDeactivatedEventModel.getStoreId(),
            promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(),
            promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
            promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
  }

  @Test
  public void activePromoBundlingItemSkuTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    Mockito.when(saveOperationService.updateActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE))
        .thenReturn(item);
    itemServiceImpl.activeDeactivatePromoBundling(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveOperationService).updateActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE);
  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuWholeSaleTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(Constants.WHOLESALE_PRICE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    Mockito.when(saveOperationService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE))
        .thenReturn(item);
    itemServiceImpl.activeDeactivatePromoBundling(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveOperationService).updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), true, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), true);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
  }

  @Test
  public void activeDeactivatePromoBundlingInItemPickupPointTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(true);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType("WHOLE_SALE");
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPoint(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(businessPartnerPromoService)
        .upsertBusinessPartnerPromo(promoBundlingActivatedDeactivatedEventModel.getStoreId(),
            promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(),
            promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
            promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuInItemPickupPointTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE, false))
        .thenReturn(itemPickupPoint);
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPoint(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE, false);
  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuWholeSaleInItemPickupPointTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(Constants.WHOLESALE_PRICE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    Mockito.when(itemPickupPointService.updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE, false))
        .thenReturn(itemPickupPoint);
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPoint(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), true, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), true);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
    Mockito.verify(cacheEvictHelperService).evictItemData(ItemServiceImplTest.STORE_ID, item);
    verify(cacheEvictItemService).evictFindL5ByItemSku(eq(STORE_ID), anyString());
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
    Mockito.verify(itemPickupPointService).updateActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE, false);
  }

  @Test
  public void activeDeactivatePromoBundlingInItemPickupPointByPPCodeTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(true);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType("WHOLE_SALE");
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(businessPartnerPromoService)
        .upsertBusinessPartnerPromo(promoBundlingActivatedDeactivatedEventModel.getStoreId(),
            promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(),
            promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
            promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuInItemPickupPointByPPCodeTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    promoBundlingActivatedDeactivatedEventModel.setItemInfoList(
        Arrays.asList(new ItemInfo(ITEM_SKU, PICKUP_POINT_CODE, PICKUP_POINT_CODE)));
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE,
            false, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE, false, PICKUP_POINT_CODE);
    Mockito.verify(productL3SolrService)
        .updatePromoOrWholesaleItemSkusByItemPickupPoint(anyList());
    Mockito.verify(cacheEvictItemService).evictFindL5ByItemSku(anyString(), anyString());

  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuInItemPickupPointByPPCodeReturnNullTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    promoBundlingActivatedDeactivatedEventModel.setItemInfoList(
        Arrays.asList(new ItemInfo(ITEM_SKU, PICKUP_POINT_CODE, PICKUP_POINT_CODE)));
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    Mockito.when(
        itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE,
            false, PICKUP_POINT_CODE)).thenReturn(null);
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE, false, PICKUP_POINT_CODE);
  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuWholeSaleInItemPickupPointByPPCodeTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(Constants.WHOLESALE_PRICE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(true);
    promoBundlingActivatedDeactivatedEventModel.setItemInfoList(
        Arrays.asList(new ItemInfo(ITEM_SKU, PICKUP_POINT_CODE, PICKUP_POINT_CODE)));
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(anyString(), anyString(), eq(false))).thenReturn(item);
    Mockito.when(itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU,
        Constants.WHOLESALE_PRICE, false, PICKUP_POINT_CODE)).thenReturn(itemPickupPoint);
    itemServiceImpl.activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), true, item.getMerchantCode());
    Mockito.verify(itemPickupPointService)
        .updateActivePromoBundlingByItemSkuAndPPCode(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE, false,
            PICKUP_POINT_CODE);
    Mockito.verify(productL3SolrService)
        .updatePromoOrWholesaleItemSkusByItemPickupPoint(anyList());
    Mockito.verify(cacheEvictItemService).evictFindL5ByItemSku(anyString(), anyString());

  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuWholeSaleDeActivationTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(Constants.WHOLESALE_PRICE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingEventType(PromoBundlingEventType.PROMO_ENDED);
    Mockito.when(saveOperationService.removeActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE))
        .thenReturn(item);
    itemServiceImpl.activeDeactivatePromoBundling(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveOperationService).removeActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE);
    Mockito.verify(saveAndPublishService)
        .publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), false, item.getMerchantCode());
    Mockito.verify(productAndItemSolrIndexerService).updateWholesalePriceActivatedFlag(item.getItemSku(), false);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(false));
  }

  @Test
  public void processAdjustmentProductChangeEventAtL5LevelTest() {
    AdjustmentProductChange adjustmentProductChange =
        new AdjustmentProductChange(ADJUSTMENT_NAME, DESCRIPTION, ITEM_SKU, CAMPAIGN_CODE, ITEM_SKU, START_DATE,
            END_DATE, DISCOUNT_PRICE, IS_ACTIVATED, false, 0, 1, PP_CODE);
    itemPickupPoint.setMerchantCode(LEVEL2_MERCHANT_CODE);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT, "true", StringUtils.EMPTY);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT)).thenReturn(systemParameter);
    when(itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(),
        anyString())).thenReturn(itemPickupPoint);
    itemServiceImpl.processAdjustmentProductChangeEventAtL5Level(adjustmentProductChange);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPromoFlagChangeByItemSkus(any(), eq(true),
        eq(null), eq(productSkuAndMerchantCodeMap));
    Mockito.verify(itemPickupPointService).updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(), anyString());
  }

  @Test
  public void processAdjustmentProductChangeEventAtL5LevelFalseTest() {
    AdjustmentProductChange adjustmentProductChange =
        new AdjustmentProductChange(ADJUSTMENT_NAME, DESCRIPTION, ITEM_SKU, CAMPAIGN_CODE, ITEM_SKU, START_DATE,
            END_DATE, DISCOUNT_PRICE, false, false, 0, 1, PP_CODE);
    itemPickupPoint.setMerchantCode(LEVEL2_MERCHANT_CODE);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT, "true", StringUtils.EMPTY);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT)).thenReturn(systemParameter);
    when(itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(),
        anyString())).thenReturn(itemPickupPoint);
    itemServiceImpl.processAdjustmentProductChangeEventAtL5Level(adjustmentProductChange);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    Mockito.verify(productAndItemSolrIndexerService).updateSolrOnPromoFlagChangeByItemSkus(any(), eq(false),
        eq(null), eq(productSkuAndMerchantCodeMap));
    Mockito.verify(itemPickupPointService).updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPointArgumentCaptor.capture());
    Mockito.verify(itemPickupPointService).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(), anyString());
  }

  @Test
  public void processAdjustmentProductChangeEventAtL5LevelSwitchFalseTest() {
    AdjustmentProductChange adjustmentProductChange =
        new AdjustmentProductChange(ADJUSTMENT_NAME, DESCRIPTION, ITEM_SKU, CAMPAIGN_CODE, ITEM_SKU, START_DATE,
            END_DATE, DISCOUNT_PRICE, false, false, 0, 1, PP_CODE);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT, "false", StringUtils.EMPTY);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT)).thenReturn(systemParameter);
    when(itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(),
        anyString())).thenReturn(itemPickupPoint);
    itemServiceImpl.processAdjustmentProductChangeEventAtL5Level(adjustmentProductChange);
    Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
  }

  @Test
  public void processAdjustmentProductChangeEventAtL5LevelNullIPPTest() {
    AdjustmentProductChange adjustmentProductChange =
        new AdjustmentProductChange(ADJUSTMENT_NAME, DESCRIPTION, ITEM_SKU, CAMPAIGN_CODE, ITEM_SKU, START_DATE,
            END_DATE, DISCOUNT_PRICE, false, false, 0, 1, PP_CODE);
    SystemParameter systemParameter = new SystemParameter(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT, "true", StringUtils.EMPTY);
    Mockito.when(systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT)).thenReturn(systemParameter);
    when(itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(),
        anyString())).thenReturn(null);
    try {
      itemServiceImpl.processAdjustmentProductChangeEventAtL5Level(adjustmentProductChange);
    } catch (ApplicationRuntimeException e) {
    } finally {
      Mockito.verify(systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
      Mockito.verify(itemPickupPointService).findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(anyString(), anyString(), anyString());
    }
  }

  @Test
  public void deactivatePromoBundlingItemSkuTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(false);
    Mockito.when(saveOperationService.updateActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE))
        .thenReturn(item);
    itemServiceImpl.activeDeactivatePromoBundling(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveOperationService).removeActivePromoBundling(STORE_ID, ITEM_SKU, PROMO_BUNDLING_TYPE);
    Mockito.verify(productL3SolrService).updatePromoOrWholesaleItemSkus(itemListCaptor.capture(), eq(true));
  }

  @Test
  public void activeDeactivatePromoBundlingItemSkuWholeSaleDeActivationExceptionTest() {
    promoBundlingActivatedDeactivatedEventModel.setAllStore(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingType(Constants.WHOLESALE_PRICE);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingActivated(false);
    promoBundlingActivatedDeactivatedEventModel.setPromoBundlingEventType(PromoBundlingEventType.PROMO_ENDED);
    itemServiceImpl.activeDeactivatePromoBundling(promoBundlingActivatedDeactivatedEventModel);
    Mockito.verify(saveOperationService).removeActivePromoBundling(STORE_ID, ITEM_SKU, Constants.WHOLESALE_PRICE);
  }

  @Test
  public void updatePickupPoints_DifferentPickupPointMppTrueTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    pickupPointUpdateRequest.setDifferentLocation(true);
    pickupPointUpdateRequest.setFbbActivated(true);
    itemPickupPoint.setFbbActivated(Boolean.TRUE);
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false)).thenReturn(item);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false,
        true, false)).thenReturn(Collections.singletonList(item));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(
        item);
    doNothing().when(productAndItemSolrIndexerService).pickupPointCodesAtomicUpdate(item);
    when(productAndItemSolrIndexerService.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true)).thenReturn(
        product);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, true, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false);
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    verify(productAndItemSolrIndexerService).pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(cacheEvictHelperService, times(1)).evictItemPickupPointData(anyString(), any(), anyString());
    verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(eq(STORE_ID),
        anyList());
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ITEM_SKU, PICKUP_POINT_CODE, false);
  }

  @Test
  public void updatePickupPoints_DifferentPickupPointMppTrueFbbFalseTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    pickupPointUpdateRequest.setDifferentLocation(true);
    itemPickupPoint.setFbbActivated(Boolean.FALSE);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false,
        false)).thenReturn(item);
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    doNothing().when(productAndItemSolrIndexerService).pickupPointCodesAtomicUpdate(item);
    when(productAndItemSolrIndexerService.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true)).thenReturn(
        product);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false,
        false);
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    verify(productAndItemSolrIndexerService).pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(cacheEvictHelperService, times(1))
        .evictItemPickupPointData(anyString(), any(), anyString());
    verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(eq(STORE_ID),
        anyList());
  }

  @Test
  public void updatePickupPoints_DifferentPickupPointMppTrueFbbTrueTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    pickupPointUpdateRequest.setDifferentLocation(true);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false,
        false)).thenReturn(item);
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(item);
    doNothing().when(productAndItemSolrIndexerService).pickupPointCodesAtomicUpdate(item);
    when(productAndItemSolrIndexerService.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true)).thenReturn(
        product);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    verify(dataSourceWrapperService).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU, false,
        false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false);
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    verify(productAndItemSolrIndexerService).pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(cacheEvictHelperService, times(1))
        .evictItemPickupPointData(anyString(), any(), anyString());
    verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(eq(STORE_ID),
        anyList());
  }

  @Test
  public void updatePickupPoints_DifferentPickupPointMppTrueFbbTrueCombinedRequestTest() throws Exception {
    pickupPointUpdateRequest.setDifferentLocation(true);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
  }

  @Test
  public void updatePickupPoints_samePickupPointMppTrueFbbTrueCombinedRequestTest() throws Exception {
    pickupPointUpdateRequest.setDifferentLocation(false);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
  }

  @Test
  public void updatePickupPoints_samePickupPointMppTrueFbbTrueTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    pickupPointUpdateRequest.setDifferentLocation(false);
    when(dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false)).thenReturn(Collections.singletonList(itemPickupPoint));
    when(itemCacheableService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false, false,
        false)).thenReturn(Arrays.asList(item));
    when(dataSourceWrapperService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false,
        false, false)).thenReturn(Collections.singletonList(item));
    when(this.saveOperationService.saveItemWithoutUpdatingSolr(any(Item.class), any(), anyBoolean(), anyString(), eq(Collections.EMPTY_MAP))).thenReturn(
        item);
    doNothing().when(productAndItemSolrIndexerService).pickupPointCodesAtomicUpdate(item);
    when(productAndItemSolrIndexerService.pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), true)).thenReturn(
        product);
    itemServiceImpl.updatePickupPoints(STORE_ID, pickupPointUpdateRequest, false);
    Mockito.verify(dataSourceWrapperService)
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU, false, false, false);
    verify(dataSourceWrapperService).findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, false);
    verify(dataSourceWrapperService).findItemPickupPointByItemSkuAndPickupPointCode(STORE_ID,
        ItemServiceImplTest.ITEM_SKU, ItemServiceImplTest.PICKUP_POINT_CODE, false);
    verify(saveAndPublishService).publishItemPickupPointDataChangeEvent(anyList(), anyList(),
      eq(Collections.EMPTY_MAP));
    verify(productAndItemSolrIndexerService).pickupPointCodesUpdateAndSolrPublish(Arrays.asList(item), false);
    verify(itemPickupPointService).saveItemPickupPoint(anyList());
    verify(cacheEvictHelperService, times(1)).evictItemPickupPointData(anyString(), any(), anyString());
    verify(productL3SolrReindexStatusService).insertProductSkusToReindexStatusCollection(eq(STORE_ID),
        anyList());
  }

  @Test
  public void saveItemsTest() {
    itemServiceImpl.saveItems(Arrays.asList(item));
    verify(itemRepository).saveAll(Arrays.asList(item));
  }

  @Test
  public void saveItemsEmpty() {
    itemServiceImpl.saveItems(new ArrayList<>());
  }

  @Test
  public void findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalseTest() {
    itemServiceImpl.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(Arrays.asList(ITEM_SKU)));
  }

  @Test
  public void findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalseEmptyTest() {
    itemServiceImpl.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>());
  }

  @Test
  public void findItemsByStoreIdAndProductSkuInAndItemSkuInAndMarkForDeleteFalseTest() {
    itemServiceImpl.findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID,
        Collections.singletonList(PRODUCT_SKU), new ArrayList<>());
    Mockito.verify(itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDelete(STORE_ID,
        new HashSet<>(Collections.singleton(PRODUCT_SKU)), false);
  }

  @Test
  public void findItemsByStoreIdAndProductSkuInAndItemSkuInAndMarkForDeleteFalseTest2() {
    itemServiceImpl.findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new ArrayList<>(), ITEM_SKUS);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        new HashSet<>(ITEM_SKUS));
  }

  private ProductItemsVo generateProductItemVo(Product product, Item item, ItemPickupPoint itemPickupPoint) {
    ProductItemsVo productItemsVo = new ProductItemsVo();
    productItemsVo.setProductVo(Objects.nonNull(product) ? gdnMapper.deepCopy(product, ProductVo.class) : null);
    productItemsVo.setItemVoList(Objects.nonNull(item) ? Arrays.asList(gdnMapper.deepCopy(item, ItemVo.class)) : null);
    productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(new ArrayList<>());
    productItemsVo.getItemVoList().get(0).setItemPickupPointVoList(Objects.nonNull(itemPickupPoint) ?
        new ArrayList<>(Arrays.asList(gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))) :
        null);
    productItemsVo.getItemVoList().get(0).setSourceItemCode(ITEM_CODE);
    return productItemsVo;
  }

  @Test
  public void updateItemPickupPointDeliveryFalseTest() {
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint1.setDelivery(false);
    Mockito.when(itemPickupPointService
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(), anyString(), eq(true),
            eq(false))).thenReturn(Arrays.asList(itemPickupPoint1));
    this.itemServiceImpl.updateItemPickupPointDeliveryFalse(STORE_ID, item, new ArrayList<>());
    Mockito.verify(itemPickupPointService).saveItemPickupPoint(anyList());
    Mockito.verify(itemPickupPointService)
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(anyString(), anyString(), eq(true),
            eq(false));
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointTest() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    campaignPriceSkuResponseMap.put(ITEM_SKU, new CampaignPriceSkuResponse());
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(0.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    product.setCategoryCode(CATEGORY_ID);
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(
            masterDataService.getProductDetailFromMasterData(anyString(), anyString(), anyString()))
        .thenReturn(productDetailResponse);
    Mockito.when(xCampaignOutbound.getCampaignPriceInfo(anyString(), any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    boolean result = this.itemServiceImpl.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
    assertTrue(result);
    Mockito.verify(productService)
        .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
    Mockito.verify(xCampaignOutbound)
        .validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any(CampaignUpdateDiscountRequest.class));
  }

  @Test
  public void validatePriceForDeliveryTrueItemPickupPointTestPriceEditDisabled() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    campaignPriceSkuResponseMap.put(ITEM_SKU, new CampaignPriceSkuResponse());
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(0.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField =
        {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    itemPickupPoint.setMerchantPromoDiscount(true);
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(
            masterDataService.getProductDetailFromMasterData(anyString(), anyString(), anyString()))
        .thenReturn(productDetailResponse);
    Mockito.when(xCampaignOutbound.getCampaignPriceInfo(anyString(), any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl
          .validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint, updateOfflineItemPriceRequest));
    } finally {
      Mockito.verify(productService)
          .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
    }
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointTestNonNullResponse() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    campaignPriceSkuResponseMap.put(ITEM_SKU, new CampaignPriceSkuResponse());
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(0.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField =
        {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(
            masterDataService.getProductDetailFromMasterData(anyString(), anyString(), anyString()))
        .thenReturn(productDetailResponse);
    Mockito.when(xCampaignOutbound.getCampaignPriceInfo(anyString(), any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    Mockito.when(xCampaignOutbound.validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any()))
        .thenReturn(new CampaignUpdateDiscountResponse());
    boolean result = this.itemServiceImpl
        .validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint, updateOfflineItemPriceRequest);
    assertTrue(result);
    Mockito.verify(productService)
        .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
    Mockito.verify(xCampaignOutbound)
        .validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any(CampaignUpdateDiscountRequest.class));
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointTestNonNullErrorResponse() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    campaignPriceSkuResponseMap.put(ITEM_SKU, new CampaignPriceSkuResponse());
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(0.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(
            masterDataService.getProductDetailFromMasterData(anyString(), anyString(), anyString()))
        .thenReturn(productDetailResponse);
    Mockito.when(xCampaignOutbound.getCampaignPriceInfo(anyString(), any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
    campaignUpdateDiscountResponse.setItemSkuStatusMap(new HashMap<>());
    Mockito.when(xCampaignOutbound.validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any()))
        .thenReturn(campaignUpdateDiscountResponse);
    boolean result = this.itemServiceImpl.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
    assertTrue(result);
    Mockito.verify(productService)
        .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
    Mockito.verify(xCampaignOutbound)
        .validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any(CampaignUpdateDiscountRequest.class));
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointTestNonNullErrorValidResponse() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    campaignPriceSkuResponseMap.put(ITEM_SKU, new CampaignPriceSkuResponse());
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(0.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField =
        {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(
            masterDataService.getProductDetailFromMasterData(anyString(), anyString(), anyString()))
        .thenReturn(productDetailResponse);
    Mockito.when(xCampaignOutbound.getCampaignPriceInfo(anyString(), any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
    campaignUpdateDiscountResponse.setItemSkuStatusMap(new HashMap<>());
    campaignUpdateDiscountResponse.getItemSkuStatusMap().put(ITEM_SKU, ITEM_SKU);
    Mockito.when(xCampaignOutbound.validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any()))
        .thenReturn(campaignUpdateDiscountResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl
          .validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint, updateOfflineItemPriceRequest));
    } finally {
      Mockito.verify(productService)
          .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
      Mockito.verify(xCampaignOutbound)
          .validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any(CampaignUpdateDiscountRequest.class));

    }
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointTest1() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setMaxAllowedPrice(100);
    campaignPriceSkuResponseMap.put(ITEM_SKU, campaignPriceSkuResponse);
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(1.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(Collections.singletonList(product));
    Mockito.when(
            masterDataService.getProductDetailFromMasterData(anyString(), anyString(), anyString()))
        .thenReturn(productDetailResponse);
    Mockito.when(xCampaignOutbound.getCampaignPriceInfo(anyString(), any(CampaignPriceRequest.class)))
        .thenReturn(campaignPriceResponse);
    boolean result = this.itemServiceImpl.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
    assertTrue(result);
    Mockito.verify(productService)
        .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
    Mockito.verify(xCampaignOutbound)
        .validateAndUpdateDiscountPrice(eq(Boolean.TRUE), any(CampaignUpdateDiscountRequest.class));
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointThrowException() throws Exception {
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap = new HashMap<>();
    CampaignPriceSkuResponse campaignPriceSkuResponse = new CampaignPriceSkuResponse();
    campaignPriceSkuResponse.setMaxAllowedPrice(100);
    campaignPriceSkuResponseMap.put(ITEM_SKU, campaignPriceSkuResponse);
    CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();
    campaignPriceResponse.setItemSkuToPriceResponse(campaignPriceSkuResponseMap);
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(1.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenThrow(RuntimeException.class);
    try{
      boolean result = this.itemServiceImpl.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
          updateOfflineItemPriceRequest);
    }
    finally {
      Mockito.verify(productService)
          .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
    }
  }

  @Test
  public void valiadtePriceForDeliveryTrueItemPickupPointEmptyProductsListTest() throws Exception {
    UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
    updateOfflineItemPriceRequest.setItemSku(ITEM_SKU);
    updateOfflineItemPriceRequest.setOfferPrice(0.0);
    updateOfflineItemPriceRequest.setListPrice(0.0);
    String[] includedField = {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    Mockito.when(productService.getProductsByProductSkus(any(), anySet(), any(), Mockito.anyBoolean()))
        .thenReturn(null);
    boolean result = this.itemServiceImpl.validatePriceForDeliveryTrueItemPickupPoint(STORE_ID, itemPickupPoint,
        updateOfflineItemPriceRequest);
    assertFalse(result);
    Mockito.verify(productService)
        .getProductsByProductSkus(STORE_ID, Collections.singleton(itemPickupPoint.getProductSku()), includedField, false);
  }

  @Test
  public void fetchItemMapByItemSkusTest() {
    when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    Map<String, Item> itemMap = itemServiceImpl.fetchItemMapByItemSkus(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, ImmutableSet.of(ITEM_SKU));
    assertTrue(itemMap.keySet().contains(item.getItemSku()));
  }

  @Test
  public void fetchItemMapByItemSkusEmptyStoreIdTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.fetchItemMapByItemSkus(StringUtils.EMPTY, ImmutableSet.of(ITEM_SKU)));
  }

  @Test
  public void fetchItemMapByItemSkusEmptyItemSkuSetTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.fetchItemMapByItemSkus(STORE_ID, new HashSet<>()));
  }

  @Test
  public void getL4ItemListByProductSkuTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    String storeId = STORE_ID;
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    item1.setItemCode(ITEM_CODE);
    item1.setProductSku(PRODUCT_SKU);
    Item item2 = ITEM2;
    item2.setItemCode(ITEM_CODE);
    item2.setProductSku(PRODUCT_SKU);
    Item item3 = ITEM3;
    item3.setItemCode(ITEM_CODE);
    item3.setProductSku(PRODUCT_SKU);
    items.add(item2);
    items.add(item1);
    items.add(item3);
    Page<Item> itemPage = new PageImpl<>(items, pageRequest, 1000);

    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse2 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse3 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(item1.getItemSku());
    itemLevel4ListingResponse2.setItemSku(item2.getItemSku());
    itemLevel4ListingResponse3.setItemSku(item3.getItemSku());
    List<ItemLevel4ListingResponse> level4SummaryResponses = new CopyOnWriteArrayList<>();
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    conversionList.add(itemLevel4ListingResponse1);
    conversionList.add(itemLevel4ListingResponse2);
    conversionList.add(itemLevel4ListingResponse3);
    Mockito.when(
        itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, productSkus,
            pageRequest)).thenReturn(itemPage);
    Mockito.when(objectConverterService.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>()))
        .thenReturn(conversionList);
    Mockito.when(
            productService.getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
            productService.getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(product.getProductCode())))
        .thenReturn(Map.of(product.getProductCode(), true));
    Page<ItemLevel4ListingResponse> l4ItemListByProductSku =
        this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0,10);
    assertFalse(CollectionUtils.isEmpty(productSkus));
    assertTrue(
        org.apache.commons.collections.CollectionUtils.isNotEmpty(itemPage.getContent()));
    assertEquals(3, itemPage.getContent().size());
    assertEquals(1000, itemPage.getTotalElements());
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
        productSkus, pageRequest);
    itemsChanged = new HashMap<>();
    itemsChanged.put(PRODUCT_SKU + "2", false);
    itemsChanged.put(PRODUCT_SKU + "1", false);
    verify(objectConverterService).convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, new ArrayList<>(), Map.of(PRODUCT_SKU, true));
    assertTrue(itemPage.hasContent());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    verify(itemPickupPointService).findItemPickupPointByProductSkus(STORE_ID, new ArrayList<>(productSkus));
    Mockito.verify(productService)
        .getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false);
    Mockito.verify(productService).getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(),
        Set.of(product.getProductCode()));
    assertTrue(itemPage.hasContent());
    assertFalse(CollectionUtils.isEmpty(conversionList));
  }

  @Test
  @Disabled("Disabled due to the issue jenkins ci issue")
  public void getL4ItemListByProductSkuWithSizeZeroTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    String storeId = STORE_ID;
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    item1.setProductSku(PRODUCT_SKU);
    item1.setItemCode(ITEM_CODE);
    Item item2 = ITEM2;
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemCode(ITEM_CODE_NOT_ADD);
    Item item3 = ITEM3;
    item3.setProductSku(PRODUCT_SKU);
    item3.setItemCode(SOURCE_ITEM_CODE);
    items.add(item2);
    items.add(item1);
    items.add(item3);

    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse2 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse3 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(item1.getItemSku());
    itemLevel4ListingResponse2.setItemSku(item2.getItemSku());
    itemLevel4ListingResponse3.setItemSku(item3.getItemSku());
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    conversionList.add(itemLevel4ListingResponse1);
    conversionList.add(itemLevel4ListingResponse2);
    conversionList.add(itemLevel4ListingResponse3);
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDelete(STORE_ID, productSkus, false))
        .thenReturn(items);
    Mockito.when(
            objectConverterService.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, Map.of(ITEM_CODE, true)))
        .thenReturn(conversionList);
    Mockito.when(
            productService.getAllProducts(STORE_ID, Set.of(PRODUCT_SKU), false))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
            productService.getProductCodeAndSharedProductMap(STORE_ID, Set.of(product.getProductCode())))
        .thenReturn(Map.of(product.getProductCode(), true));

    Page<ItemLevel4ListingResponse> l4ItemListByProductSku =
        this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0, null);
    assertFalse(CollectionUtils.isEmpty(productSkus));
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDelete(storeId, productSkus, false);
    itemsChanged = new HashMap<>();
    itemsChanged.put(PRODUCT_SKU + "2", false);
    itemsChanged.put(PRODUCT_SKU + "1", false);

    verify(objectConverterService).convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, new ArrayList<>(),
        Map.of(PRODUCT_SKU, true));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    verify(itemPickupPointService).findItemPickupPointByProductSkus(STORE_ID, new ArrayList<>(productSkus));
    Mockito.verify(productService)
        .getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false);
    Mockito.verify(productService).getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(),
        Set.of(product.getProductCode()));
    assertFalse(CollectionUtils.isEmpty(conversionList));
  }


  @Test
  @Disabled("Disabled due to the issue jenkins ci issue")
  public void getL4ItemListByProductSkuWithSizeZeroBundleProductTest() throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    String storeId = STORE_ID;
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    item1.setProductSku(PRODUCT_SKU);
    item1.setItemCode(ITEM_CODE);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU_1, 1)));
    Item item2 = ITEM2;
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemCode(ITEM_CODE_NOT_ADD);
    Item item3 = ITEM3;
    item3.setProductSku(PRODUCT_SKU);
    item3.setItemCode(SOURCE_ITEM_CODE);
    items.add(item2);
    items.add(item1);
    items.add(item3);

    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse2 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse3 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(item1.getItemSku());
    itemLevel4ListingResponse2.setItemSku(item2.getItemSku());
    itemLevel4ListingResponse3.setItemSku(item3.getItemSku());
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    conversionList.add(itemLevel4ListingResponse1);
    conversionList.add(itemLevel4ListingResponse2);
    conversionList.add(itemLevel4ListingResponse3);
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDelete(STORE_ID, productSkus, false))
        .thenReturn(items);
    Mockito.when(
            objectConverterService.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, Map.of(ITEM_CODE, true)))
        .thenReturn(conversionList);
    Mockito.when(
            productService.getAllProducts(STORE_ID, Set.of(PRODUCT_SKU), false))
        .thenReturn(Arrays.asList(product));
    Mockito.when(productService.getProductCodeAndSharedProductMap(STORE_ID, Set.of(product.getProductCode())))
        .thenReturn(Map.of(product.getProductCode(), true));
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, Set.of(ITEM_SKU_1)))
        .thenReturn(List.of(item1));

    Page<ItemLevel4ListingResponse> l4ItemListByProductSku =
        this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0, null);
    assertFalse(CollectionUtils.isEmpty(productSkus));
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDelete(storeId, productSkus, false);
    itemsChanged = new HashMap<>();
    itemsChanged.put(PRODUCT_SKU + "2", false);
    itemsChanged.put(PRODUCT_SKU + "1", false);

    verify(objectConverterService).convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, new ArrayList<>(),
        Map.of(PRODUCT_SKU, true));
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    verify(itemPickupPointService).findItemPickupPointByProductSkus(STORE_ID, new ArrayList<>(productSkus));
    Mockito.verify(productService)
        .getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false);
    Mockito.verify(productService).getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(),
        Set.of(product.getProductCode()));
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, Set.of(ITEM_SKU_1));
    assertFalse(CollectionUtils.isEmpty(conversionList));
  }

  @Test
  public void getL4ItemListByProductSkuNotEmptyTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    String storeId = STORE_ID;
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    item1.setItemCode(ITEM_CODE);
    item1.setProductSku(PRODUCT_SKU);
    Item item2 = ITEM2;
    item2.setItemCode(ITEM_CODE);
    item2.setProductSku(PRODUCT_SKU);
    Item item3 = ITEM3;
    item3.setItemCode(ITEM_CODE);
    item3.setProductSku(PRODUCT_SKU);
    items.add(item2);
    items.add(item1);
    items.add(item3);
    Page<Item> itemPage = new PageImpl<>(items, pageRequest, 1000);

    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse2 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse3 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(item1.getItemSku());
    itemLevel4ListingResponse2.setItemSku(item2.getItemSku());
    itemLevel4ListingResponse3.setItemSku(item3.getItemSku());
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    conversionList.add(itemLevel4ListingResponse1);
    conversionList.add(itemLevel4ListingResponse2);
    conversionList.add(itemLevel4ListingResponse3);
    Mockito.when(
        itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, productSkus,
            pageRequest)).thenReturn(itemPage);
    Mockito.when(objectConverterService.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, itemPickupPointList, Map.of(ITEM_CODE,true))
    ).thenReturn(conversionList);
    Mockito.when(itemPickupPointService.findItemPickupPointByProductSkus(STORE_ID, new ArrayList<>(productSkus)))
        .thenReturn(itemPickupPointList);
    Mockito.when(
            productService.getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
            productService.getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(product.getProductCode())))
        .thenReturn(Map.of(product.getProductCode(), true));
    Page<ItemLevel4ListingResponse> l4ItemListByProductSku =
        this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0,10);
    assertFalse(CollectionUtils.isEmpty(productSkus));
    assertTrue(
        org.apache.commons.collections.CollectionUtils.isNotEmpty(itemPage.getContent()));
    assertEquals(3, itemPage.getContent().size());
    assertEquals(1000, itemPage.getTotalElements());
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
        productSkus, pageRequest);
    itemsChanged = new HashMap<>();
    itemsChanged.put(PRODUCT_SKU + "2", false);
    itemsChanged.put(PRODUCT_SKU + "1", false);
    verify(objectConverterService).convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, itemPickupPointList,
        Map.of(PRODUCT_SKU, true));
    assertTrue(itemPage.hasContent());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    verify(itemPickupPointService).findItemPickupPointByProductSkus(STORE_ID, new ArrayList<>(productSkus));
    Mockito.verify(productService)
        .getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false);
    Mockito.verify(productService).getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(),
        Set.of(product.getProductCode()));
    assertTrue(itemPage.hasContent());
    assertFalse(CollectionUtils.isEmpty(conversionList));
  }

  @Test
  public void getL4ItemListByProductSkuNonNullTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    String storeId = STORE_ID;
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    item1.setItemCode(ITEM_CODE);
    item1.setProductSku(PRODUCT_SKU);
    Item item2 = ITEM2;
    item2.setProductSku(PRODUCT_SKU);
    item2.setItemCode(ITEM_CODE);
    Item item3 = ITEM3;
    item3.setItemCode(ITEM_CODE);
    item3.setProductSku(PRODUCT_SKU);
    items.add(item2);
    items.add(item1);
    items.add(item3);
    Page<Item> itemPage = new PageImpl<>(items);
    ItemLevel4ListingResponse itemLevel4ListingResponse1 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse2 = new ItemLevel4ListingResponse();
    ItemLevel4ListingResponse itemLevel4ListingResponse3 = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse1.setItemSku(item1.getItemSku());
    itemLevel4ListingResponse2.setItemSku(item2.getItemSku());
    itemLevel4ListingResponse3.setItemSku(item3.getItemSku());
    List<ItemLevel4ListingResponse> level4SummaryResponses = new CopyOnWriteArrayList<>();
    List<ItemLevel4ListingResponse> conversionList = new ArrayList<>();
    conversionList.add(itemLevel4ListingResponse1);
    conversionList.add(itemLevel4ListingResponse2);
    conversionList.add(itemLevel4ListingResponse3);
    when(this.systemParameterService.findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT))
        .thenReturn(new SystemParameter(STORE_ID,
            SystemParameterNames.PRODUCT_SKU_LIST_LIMIT, "2", ""));
    Mockito.when(
        itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, productSkus,
            pageRequest)).thenReturn(itemPage);
    Mockito.when(objectConverterService.convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, null, new HashMap<>()))
        .thenReturn(conversionList);
    when(productService.isSharedProduct(anyString(), anyString(), eq(false), anyString())).thenReturn(new SimpleBooleanResponse(false));
    Mockito.when(itemPickupPointService.findOneByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU + "2"))
        .thenReturn(itemPickupPoint);
    Mockito.when(
            productService.getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false))
        .thenReturn(Arrays.asList(product));
    Mockito.when(
            productService.getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(product.getProductCode())))
        .thenReturn(Map.of(product.getProductCode(), true));
    Page<ItemLevel4ListingResponse> l4ItemListByProductSku =
        this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0,  10);
    assertFalse(CollectionUtils.isEmpty(productSkus));
    assertTrue(
        org.apache.commons.collections.CollectionUtils.isNotEmpty(itemPage.getContent()));
    assertEquals(3, itemPage.getContent().size());
    assertEquals(3, itemPage.getTotalElements());
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
        productSkus, pageRequest);
    itemsChanged = new HashMap<>();
    itemsChanged.put(PRODUCT_SKU + "2", true);
    itemsChanged.put(PRODUCT_SKU + "1", false);
    verify(objectConverterService).convertItemToItemLevel4SummaryResponse(items, REQUEST_ID, new ArrayList<>(), Map.of(PRODUCT_SKU, true));
    verify(itemPickupPointService).findItemPickupPointByProductSkus(STORE_ID, new ArrayList<>(productSkus));
    assertTrue(itemPage.hasContent());
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    Mockito.verify(productService)
        .getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), Set.of(PRODUCT_SKU), false);
    Mockito.verify(productService).getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(),
        Set.of(product.getProductCode()));
    assertTrue(itemPage.hasContent());
    assertFalse(CollectionUtils.isEmpty(conversionList));
  }

  @Test
  public void getL5ItemListByProductSkuTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Product product1= new Product();
    product1.setProductSku(PRODUCT_SKU);
    product1.setProductName(PRODUCT_NAME);
    productList.add(product1);
    items.add(item);
    itemPickupPoint.setActivePromoBundlings(Collections.singleton(PROMO_BUNDLING_TYPE));
    itemPickupPointList.add(itemPickupPoint);

    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemLevel5Response.setItemSku(item.getItemSku());

    List<ItemLevel5Response> level5ResponseList = new CopyOnWriteArrayList<>();
    List<ItemLevel5Response> conversionList = new ArrayList<>();
    conversionList.add(itemLevel5Response);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(itemPickupPointList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product1);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(items);
    Mockito.when(objectConverterService.convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), anyList(),
        eq(false), eq(new HashMap<>()), eq(true), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL))).thenReturn(conversionList);
    List<ItemLevel5Response> itemLevel5ResponseList =
        this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(PRODUCT_SKU), true,
            Collections.singletonList(PICKUP_POINT_CODE), Collections.singletonList(PROMO_BUNDLING_TYPE), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL,
            false);
    verify(itemPickupPointService).findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(PICKUP_POINT_CODE));
    verify(productCacheableService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID,new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(objectConverterService).convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), anyList(),
        eq(false), eq(new HashMap<>()), eq(false), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL));
    assertFalse(CollectionUtils.isEmpty(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void getL5ItemListByProductSkuCategoryDataTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Product product1= new Product();
    product1.setProductSku(PRODUCT_SKU);
    product1.setProductName(PRODUCT_NAME);
    productList.add(product1);
    items.add(item);
    itemPickupPoint.setActivePromoBundlings(Collections.singleton(PROMO_BUNDLING_TYPE));
    itemPickupPointList.add(itemPickupPoint);

    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemLevel5Response.setItemSku(item.getItemSku());

    List<ItemLevel5Response> level5ResponseList = new CopyOnWriteArrayList<>();
    List<ItemLevel5Response> conversionList = new ArrayList<>();
    conversionList.add(itemLevel5Response);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(itemPickupPointList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product1);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(items);
    Mockito.when(objectConverterService.convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), anyList(),
        eq(false), eq(new HashMap<>()), eq(false), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL))).thenReturn(conversionList);
    List<ItemLevel5Response> itemLevel5ResponseList =
        this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(PRODUCT_SKU), true,
            Collections.singletonList(PICKUP_POINT_CODE), Collections.singletonList(PROMO_BUNDLING_TYPE), false, true, FETCH_VIEW_CONFIGS_BY_CHANNEL,
            true);
    verify(itemPickupPointService).findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(PICKUP_POINT_CODE));
    verify(productCacheableService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID,new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(objectConverterService).convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), anyList(),
        eq(false), eq(new HashMap<>()), eq(false), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL));
    verify(cachedService).getParentCategoriesFromDbAndCache(GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(),
        Optional.ofNullable(items).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
            .map(item -> Optional.ofNullable(item.getCategoryCode()).orElse(StringUtils.EMPTY))
            .collect(Collectors.toSet()));
    assertFalse(CollectionUtils.isEmpty(itemLevel5ResponseList));
  }

  @Test
  public void getL5ItemListByProductSkuEmptyPickupPointTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Product product1= new Product();
    product1.setProductSku(PRODUCT_SKU);
    product1.setProductName(PRODUCT_NAME);
    productList.add(product1);
    items.add(item);
    itemPickupPoint.setActivePromoBundlings(Collections.singleton(PROMO_BUNDLING_TYPE));
    itemPickupPointList.add(itemPickupPoint);

    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemLevel5Response.setItemSku(item.getItemSku());

    List<ItemLevel5Response> level5ResponseList = new CopyOnWriteArrayList<>();
    List<ItemLevel5Response> conversionList = new ArrayList<>();
    conversionList.add(itemLevel5Response);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(itemPickupPointList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(product1);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(items);
    Mockito.when(objectConverterService.convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), anyList(),
        eq(false), eq(new HashMap<>()), eq(true), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL))).thenReturn(conversionList);
    List<ItemLevel5Response> itemLevel5ResponseList =
        this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(PRODUCT_SKU), true,
            Collections.singletonList(PICKUP_POINT_CODE), new ArrayList<>(), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL,
            false);
    verify(itemPickupPointService).findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(PICKUP_POINT_CODE));
    verify(productCacheableService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID,new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(objectConverterService).convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), anyList(),
        eq(false), eq(new HashMap<>()), eq(false), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL));
    assertFalse(CollectionUtils.isEmpty(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void getL5ItemListByProductSkuEmptyPickupPointProductNullTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    List<Product> productList = new ArrayList<>();
    Product product1= new Product();
    product1.setProductSku(PRODUCT_SKU);
    product1.setProductName(PRODUCT_NAME);
    productList.add(product1);
    items.add(item);
    itemPickupPoint.setActivePromoBundlings(Collections.singleton(PROMO_BUNDLING_TYPE));
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemLevel5Response.setItemSku(item.getItemSku());
    List<ItemLevel5Response> level5ResponseList = new CopyOnWriteArrayList<>();
    List<ItemLevel5Response> conversionList = new ArrayList<>();
    conversionList.add(itemLevel5Response);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(itemPickupPointList);
    Mockito.when(productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU)).thenReturn(null);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(Arrays.asList(ITEM_SKU)))).thenReturn(items);
    Mockito.when(objectConverterService.convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), eq(new ArrayList<>()),
        eq(false), eq(new HashMap<>()), eq(false), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL))).thenReturn(conversionList);
    List<ItemLevel5Response> itemLevel5ResponseList =
        this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(PRODUCT_SKU), true,
            Collections.singletonList(PICKUP_POINT_CODE), new ArrayList<>(), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL,
            false);
    verify(itemPickupPointService).findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(PICKUP_POINT_CODE));
    verify(productCacheableService).findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(STORE_ID, PRODUCT_SKU);
    verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID,new HashSet<>(Arrays.asList(ITEM_SKU)));
    verify(objectConverterService).convertItemToItemLevel5Response(eq(STORE_ID), eq(itemPickupPointList), eq(items), eq(new ArrayList<>()),
        eq(false), eq(new HashMap<>()), eq(false), eq(FETCH_VIEW_CONFIGS_BY_CHANNEL));
    assertFalse(CollectionUtils.isEmpty(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void getL5ItemListByProductSkuEmptyItemTest() throws Exception {
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPoint.setActivePromoBundlings(Collections.singleton(PROMO_BUNDLING_TYPE));
    itemPickupPointList.add(itemPickupPoint);
    Set<String> itemSku = new HashSet<>();
    itemSku.add(ITEM_SKU);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    itemLevel5Response.setItemSku(item.getItemSku());
    List<ItemLevel5Response> level5ResponseList = new CopyOnWriteArrayList<>();
    List<ItemLevel5Response> conversionList = new ArrayList<>();
    conversionList.add(itemLevel5Response);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(itemPickupPointList);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSku)).thenReturn(items);
    List<ItemLevel5Response> itemLevel5ResponseList =
        this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(PRODUCT_SKU), false,
            Collections.singletonList(PICKUP_POINT_CODE), Collections.singletonList(PROMO_BUNDLING_TYPE), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL,
            false);
    verify(itemPickupPointService).findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(PICKUP_POINT_CODE));
    verify(this.itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, itemSku);
    assertFalse(CollectionUtils.isEmpty(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void getL5ItemListByProductSkuEmptyItemPickupPointTest() throws Exception {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    itemPickupPointList.add(itemPickupPoint);
    ItemLevel5Response itemLevel5Response = new ItemLevel5Response();
    List<ItemLevel5Response> level5ResponseList = new CopyOnWriteArrayList<>();
    List<ItemLevel5Response> conversionList = new ArrayList<>();
    conversionList.add(itemLevel5Response);
    Mockito.when(
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(PICKUP_POINT_CODE))).thenReturn(itemPickupPointList);
    List<ItemLevel5Response> itemLevel5ResponseList =
        this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(PRODUCT_SKU), false,
            Collections.singletonList(PICKUP_POINT_CODE), Collections.singletonList(PROMO_BUNDLING_TYPE), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL,
            false);
    verify(itemPickupPointService).findItemPickupPointByProductSkusAndPickupPointCodes(STORE_ID,
        Arrays.asList(PRODUCT_SKU), Arrays.asList(PICKUP_POINT_CODE));
    assertFalse(CollectionUtils.isEmpty(Arrays.asList(PRODUCT_SKU)));
  }

  @Test
  public void getL5ItemListByEmptyProductSkuTest() throws Exception  {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getL5ItemListByProductSku(STORE_ID, Arrays.asList(),
        false, new ArrayList<>(), new ArrayList<>(), false, false, FETCH_VIEW_CONFIGS_BY_CHANNEL, false));
  }

  @Test
  public void getL4ItemListByProductSkuTest_ExceptionTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    Exception exception = new Exception();
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    Page<Item> itemPageResponse = null;
    List<Item> itemSet = new ArrayList<>();
    List<ItemLevel4ListingResponse> level4SummaryResponses = new ArrayList<>();
    Mockito.when(
        itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, productSkus,
            pageRequest)).thenReturn(itemPageResponse);
    try {
      this.itemServiceImpl.getL4ItemListByProductSku(productSkus, STORE_ID, 0, 10);
      assertFalse(itemPageResponse.hasContent());
    } catch (Exception e) {

    } finally {
      verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
      assertTrue(itemSet.isEmpty());
      assertTrue(level4SummaryResponses.isEmpty());
      assertTrue(Objects.isNull(itemPageResponse));
    }
  }

  @Test
  public void getL4ItemListByProductSkuTest_Exception_NoContentTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    Page<Item> itemPageResponse = new PageImpl<>(new ArrayList<>());
    List<Item> itemSet = new ArrayList<>();
    List<ItemLevel4ListingResponse> level4SummaryResponses = new ArrayList<>();
    Mockito.when(itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(null, null,
        pageRequest)).thenReturn(itemPageResponse);
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        this.itemServiceImpl.getL4ItemListByProductSku(null, null, 0, 10));
  }

  @Test()
  public void getL4ItemListByProductSkuTest_Exception_NullTest() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "1");
    String storeId = STORE_ID;
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    Item item2 = ITEM2;
    Item item3 = ITEM3;
    items.add(item2);
    items.add(item1);
    items.add(item3);
    Page<Item> itemPage = new PageImpl<>(new ArrayList<>());
    Mockito.when(
        itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, productSkus,
            pageRequest)).thenReturn(itemPage);
    Page<ItemLevel4ListingResponse> l4ItemListByProductSku =
        this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0, 10);
    assertFalse(CollectionUtils.isEmpty(productSkus));
    verify(this.itemRepository).findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId,
        productSkus, pageRequest);
    verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    assertFalse(itemPage.hasContent());

  }

  @Test
  public void getL4ItemListByProductSkuTest_forProductSkuLimit_exceeded() throws Exception {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(PRODUCT_SKU + "1");
    productSkus.add(PRODUCT_SKU + "2");
    productSkus.add(PRODUCT_SKU + "3");
    String storeId = STORE_ID;
    PageRequest pageRequest = PageRequest.of(0, 10, Sort.Direction.ASC, "itemSku");
    List<Item> items = new ArrayList<>();
    Item item1 = ITEM1;
    Item item2 = ITEM2;
    Item item3 = ITEM3;
    items.add(item2);
    items.add(item1);
    items.add(item3);
    Page<Item> itemPage = new PageImpl<>(items);
    ItemLevel4ListingResponse itemLevel4ListingResponse = new ItemLevel4ListingResponse();
    itemLevel4ListingResponse.setItemSku(ITEM_SKU);
    List<ItemLevel4ListingResponse> level4SummaryResponses = new CopyOnWriteArrayList<>();
    level4SummaryResponses.add(itemLevel4ListingResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> this.itemServiceImpl.getL4ItemListByProductSku(productSkus, storeId, 0, 10));
    } finally {
      verify(this.systemParameterService).findValueByStoreIdAndVariable(STORE_ID,
          SystemParameterNames.PRODUCT_SKU_LIST_LIMIT);
    }
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointCodeTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(new ItemViewConfig(true, true, CHANNEL_DEFAULT, null, null)));

    when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE)).thenReturn(
        Arrays.asList(item));
    when(itemCacheableService.getItemsByPristineId(STORE_ID, PRISTINE_ID, false)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE, false);

    verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(itemCacheableService).getItemsByPristineId(STORE_ID, PRISTINE_ID, false);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertFalse(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointCodeBuyableFalseTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(new ItemViewConfig(false, true, CHANNEL_DEFAULT, null, null)));

    when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE)).thenReturn(
        Arrays.asList(item));
    when(itemCacheableService.getItemsByPristineId(STORE_ID, PRISTINE_ID, false)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE, false);

    verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(itemCacheableService).getItemsByPristineId(STORE_ID, PRISTINE_ID, false);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointCodeDiscoverableFalseTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(new ItemViewConfig(true, false, CHANNEL_DEFAULT, null, null)));

    when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE)).thenReturn(
        Arrays.asList(item));
    when(itemCacheableService.getItemsByPristineId(STORE_ID, PRISTINE_ID, false)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE, false);

    verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(itemCacheableService).getItemsByPristineId(STORE_ID, PRISTINE_ID, false);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointCodeFullFetchFalseTest() {
    item.setPristineDataItem(null);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU2);

    when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE, false);

    verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointCodeEmptyItemsTest(){
    when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE)).thenReturn(
        new ArrayList<>());

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE, true);

    verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);

    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointStoreIdBlankTest(){
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(StringUtils.EMPTY, ITEM_CODE, PICKUP_POINT_CODE, true));
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointItemCodeBlankTest(){
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, StringUtils.EMPTY, PICKUP_POINT_CODE, true));
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointPickupPointCodeBlankTest(){
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, StringUtils.EMPTY, true));
  }

  @Test
  public void getAllItemSkuByItemCodeAndPickupPointCodeFullFetchTrueTest() {
    item.setPristineDataItem(null);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU);

    when(itemCacheableService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByItemCodeAndPickupPointCode(STORE_ID, ITEM_CODE, PICKUP_POINT_CODE, true);

    verify(itemCacheableService).findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(STORE_ID, ITEM_CODE);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertFalse(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByPristineIdAndPickupPointCodeTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(new ItemViewConfig(true, true, CHANNEL_DEFAULT, null, null)));

    when(itemCacheableService.getItemsByPristineId(STORE_ID, PRISTINE_ID, false)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByPristineIdAndPickupPointCode(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);

    verify(itemCacheableService).getItemsByPristineId(STORE_ID, PRISTINE_ID, false);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertFalse(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByPristineIdAndPickupPointCodeBuyableFalseTest() {
    item.setPristineDataItem(pristineDataItem);
    item.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setItemViewConfig(ImmutableSet.of(new ItemViewConfig(false, true, CHANNEL_DEFAULT, null, null)));

    when(itemCacheableService.getItemsByPristineId(STORE_ID, PRISTINE_ID, false)).thenReturn(Arrays.asList(item));
    when(itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE)).thenReturn(Arrays.asList(itemPickupPoint));
    when(itemPriceService.getDiscountItemPickupPoint(Mockito.anyCollection())).thenReturn(
        ImmutableMap.of(ITEM_SKU, prices));

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByPristineIdAndPickupPointCode(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);

    verify(itemCacheableService).getItemsByPristineId(STORE_ID, PRISTINE_ID, false);
    verify(itemPickupPointService).getItemPickupPointByItemSkuInAndPickupPointCode(STORE_ID, ImmutableSet.of(ITEM_SKU),
        PICKUP_POINT_CODE);
    verify(itemPriceService).getDiscountItemPickupPoint(Mockito.anyCollection());

    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByPristineIdAndPickupPointCodeEmptyResponseTest() {
    when(itemCacheableService.getItemsByPristineId(STORE_ID, PRISTINE_ID, false)).thenReturn(new ArrayList<>());

    List<ItemPriceResponse> itemPriceResponseList =
        itemServiceImpl.getAllItemSkuByPristineIdAndPickupPointCode(STORE_ID, PRISTINE_ID, PICKUP_POINT_CODE);

    verify(itemCacheableService).getItemsByPristineId(STORE_ID, PRISTINE_ID, false);

    assertTrue(itemPriceResponseList.isEmpty());
  }

  @Test
  public void getAllItemSkuByPristineIdAndPickupPointCodeEmptyResponseStoreIdBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemServiceImpl.getAllItemSkuByPristineIdAndPickupPointCode(StringUtils.EMPTY, PRISTINE_ID, PICKUP_POINT_CODE));
  }

  @Test
  public void getAllItemSkuByPristineIdAndPickupPointCodeEmptyResponsePristineIdBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemServiceImpl.getAllItemSkuByPristineIdAndPickupPointCode(STORE_ID, StringUtils.EMPTY, PICKUP_POINT_CODE));
  }

  @Test
  public void getAllItemSkuByPristineIdAndPickupPointCodeEmptyResponsePickupPointCodeBlankTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class, () ->
        itemServiceImpl.getAllItemSkuByPristineIdAndPickupPointCode(STORE_ID, PRISTINE_ID, StringUtils.EMPTY));
  }


  public void updatePromoBundlingByItemSkusInItemPickupPointByItemInfoEmptyRequestTest() throws Exception {
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(STORE_ID, new ArrayList<>(), true));
  }

  @Test
  public void updatePromoBundlingByItemSkusInItemPickupPointByItemInfoTest() throws Exception {
    List<ItemInfo> itemInfos = new ArrayList<>();
    ItemInfo itemInfo = new ItemInfo();
    itemInfo.setItemSku(ITEM_SKU);
    itemInfos.add(itemInfo);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    itemPickupPoint.setMerchantCode(MERCHANT_CODE);
    when(itemPickupPointService.updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos, ProductFieldNames.PROMO_BUNDLING,
        true)).thenReturn(Arrays.asList(itemPickupPoint));
    itemServiceImpl.updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(STORE_ID, itemInfos, true);
    verify(itemPickupPointService).updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos, ProductFieldNames.PROMO_BUNDLING,
        true);
    verify(productAndItemSolrIndexerService).updateSolrOnPromoFlagChangeByItemSkus(Mockito.anyMap(), eq(true),
        eq(SolrFieldNames.PROMO_BUNDLING), eq(productSkuAndMerchantCodeMap));
  }

  @Test
  public void updatePromoBundlingByItemSkusInItemPickupPointByItemInfoEmptyTest() throws Exception {
    List<ItemInfo> itemInfos = new ArrayList<>();
    ItemInfo itemInfo = new ItemInfo();
    itemInfo.setItemSku(ITEM_SKU);
    itemInfos.add(itemInfo);
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    itemPickupPoint.setItemSku(ITEM_SKU);
    itemPickupPoint.setPickupPointCode(PICKUP_POINT_CODE);
    itemPickupPoint.setProductSku(PRODUCT_SKU);
    when(itemPickupPointService.updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos, ProductFieldNames.PROMO_BUNDLING,
        true)).thenReturn(new ArrayList<>());
    itemServiceImpl.updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(STORE_ID, itemInfos, true);
    verify(itemPickupPointService).updateFieldByItemSkusAndPPCode(STORE_ID, itemInfos, ProductFieldNames.PROMO_BUNDLING,
        true);
  }

  @Test
  public void findByStoreIdAndItemSkusNotCachedTest() {
    when(itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU))).thenReturn(currentItems);
    itemServiceImpl.findByStoreIdAndItemSkusNotCached(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID,
        ImmutableSet.of(ITEM_SKU));
  }

  @Test
  public void republishItemsToAgpMppOnTest() {
    when(itemRepository.streamAllByStoreIdAndItemSkuIn(STORE_ID, ITEM_SKUS))
        .thenReturn(ITEM_STREAM_1);
    doNothing().when(saveAndPublishService).publishItemDataChangeEvent(anyList());
    itemServiceImpl.republishItemsToAgp(STORE_ID, ITEM_SKUS);
    verify(itemRepository).streamAllByStoreIdAndItemSkuIn(STORE_ID, ITEM_SKUS);
    verify(saveAndPublishService).publishItemDataChangeEvent(anyList());
  }

  @Test
  public void getItemsByStoreIdAndItemSkusTest() {
    when(itemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU))).thenReturn(
        Arrays.asList(item));
    itemServiceImpl.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
    verify(itemRepository).findByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_SKU));
  }

  @Test
  public void getItemsByStoreIdAndItemSkusEmptyTest() {
    assertTrue(
        itemServiceImpl.getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(STORE_ID, new HashSet<>()).isEmpty());
  }

  @Test
  public void updateSubscriptionFlagByItemSkuSubTestTest() {
    Set<String> existingSubscriptionType = Sets.newHashSet("WH");
    Set<String> preferredSubType = Collections.singleton("MKT");
    item.setPreferredSubscriptionType(existingSubscriptionType);
    Mockito.when(this.itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    Mockito.when(this.saveAndPublishService.saveItems(Arrays.asList(item))).thenReturn(Arrays.asList(item));
    Mockito.doNothing().when(this.cacheEvictHelperService).evictItemCache(item.getStoreId(), item);
    itemServiceImpl.updateSubscriptionFlagByItemSku(STORE_ID, ITEM_SKU, true, preferredSubType);
    Mockito.verify(this.itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(this.saveAndPublishService).saveItems(Arrays.asList(item));
    Mockito.verify(this.cacheEvictHelperService).evictItemCache(item.getStoreId(), item);
  }

  @Test
  public void getItemAndPickupPointDetailsForCnc_Test() {
    Mockito.when(
            this.itemCacheableService.findItemAndItemPickPointByproductSkusAndCncActive(STORE_ID,
                Arrays.asList(PRODUCT_SKU), null, false, PAGE, PAGE_SIZE))
        .thenReturn(new ItemAndItemPickupPointVo());

    ItemAndItemPickupPointVo result =
        this.itemServiceImpl.getItemAndPickupPointDetailsForCnc(STORE_ID,
            Arrays.asList(PRODUCT_SKU), null, false, PAGE, PAGE_SIZE);

    Mockito.verify(this.itemCacheableService)
        .findItemAndItemPickPointByproductSkusAndCncActive(STORE_ID, Arrays.asList(PRODUCT_SKU),
            null, false, PAGE, PAGE_SIZE);
    assertNotNull(result);
  }

  @Test
  public void getItemsByStoreIdAndProductSkusOrItemSkusInTest() {
    itemServiceImpl.getItemsByStoreIdAndProductSkusOrItemSkusIn(STORE_ID, Collections.singletonList(PRODUCT_SKU),
        new ArrayList<>());
    Mockito.verify(itemRepository).findItemsByStoreIdAndProductSkuIn(STORE_ID, Collections.singleton(PRODUCT_SKU));
  }

  @Test
  public void getItemsByStoreIdAndProductSkusOrItemSkusInTest2() {
    itemServiceImpl.getItemsByStoreIdAndProductSkusOrItemSkusIn(STORE_ID, new ArrayList<>(), ITEM_SKUS);
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS));
  }

  @Test
  public void getCategoryCodeForCombinedEditRequestTest() {
    product.setCategoryCode(CATEGORY_CODE_FOR_UNSYNC_CHECK);
    String categoryCode = itemServiceImpl.getCategoryCodeForCombinedEditRequest(editProductDetailDTO);
    assertEquals(CATEGORY_CODE_FOR_UNSYNC_CHECK, categoryCode);
  }

  @Test
  public void getCategoryCodeForCombinedEditRequestCategoryCodeEmptyTest() {
    product.setCategoryCode(StringUtils.EMPTY);
    editProductDetailDTO.setProductDetailResponse(null);
    String categoryCode = itemServiceImpl.getCategoryCodeForCombinedEditRequest(editProductDetailDTO);
    assertEquals(StringUtils.EMPTY, categoryCode);
  }

  @Test
  public void getCategoryCodeForCombinedEditRequestCategoryCodeEmptyDetailResponseNonEmptyTest() {
    product.setCategoryCode(StringUtils.EMPTY);
    editProductDetailDTO.getProductDetailResponse().setProductCategoryResponses(null);
    String categoryCode = itemServiceImpl.getCategoryCodeForCombinedEditRequest(editProductDetailDTO);
    assertEquals(StringUtils.EMPTY, categoryCode);
  }

  @Test
  public void getAllItemAndPickupPointDetailsWithoutPagination_EmptyProductSkuTest() {
    Set<String> activePromoBundlings = new HashSet<>();
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    activePromoBundlings.add("WHOLESALE");
    activePromoBundlings.add("COMBO");
    Mockito.when(itemPickupPointRepository.findByStoreIdAndOfflineItemIdInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(OFFLINE_ITEM_ID))).thenReturn(itemPickupPointList);
    Mockito.when(
        itemCacheableService.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, Boolean.TRUE, Boolean.FALSE, false, null,
            false)).thenReturn(item);
    Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.getAllItemAndPickupPointDetailsWithoutPagination(STORE_ID, null,
        Arrays.asList(OFFLINE_ITEM_ID)));
  }

  @Test
  public void getAllItemAndPickupPointDetailsWithoutPaginationTest_EmptyResponseTest() {
    Set<String> activePromoBundlings = new HashSet<>();
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    activePromoBundlings.add("WHOLESALE");
    activePromoBundlings.add("COMBO");
    Mockito.when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(new ArrayList<>());
    ItemAndItemPickupPointVo result =
        itemServiceImpl.getAllItemAndPickupPointDetailsWithoutPagination(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(OFFLINE_ITEM_ID));

    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void getAllItemAndPickupPointDetailsWithoutTest() {
    Set<String> activePromoBundlings = new HashSet<>();
    Item item = new Item();
    item.setItemSku(ITEM_SKU);
    item.setProductSku(PRODUCT_SKU);
    Set<String> itemSkus = new HashSet<>();
    itemSkus.add(ITEM_SKU);
    activePromoBundlings.add("WHOLESALE");
    activePromoBundlings.add("COMBO");
    Mockito.when(itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID,
        Arrays.asList(PRODUCT_SKU))).thenReturn(itemPickupPointList);
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus))
        .thenReturn(Arrays.asList(item));
    ItemAndItemPickupPointVo result =
        itemServiceImpl.getAllItemAndPickupPointDetailsWithoutPagination(STORE_ID, Arrays.asList(PRODUCT_SKU),
            Arrays.asList(OFFLINE_ITEM_ID));

    Mockito.verify(itemPickupPointRepository)
        .findByStoreIdAndProductSkuInAndMarkForDeleteFalse(STORE_ID, Arrays.asList(PRODUCT_SKU));
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(STORE_ID, itemSkus);
  }

  @Test
  public void updateMasterSkuTest() {
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    itemServiceImpl.updateMasterSku(masterSkuMapping);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(saveAndPublishService).saveItems(itemListCaptor.capture());
    Mockito.verify(cacheEvictHelperService).evictItemCache(STORE_ID, itemListCaptor.getValue().get(0));
    assertEquals(itemListCaptor.getValue().get(0).getMasterSku(), MASTER_SKU);
    assertEquals(itemListCaptor.getValue().get(0).getItemChangeEventTypes().get(0),
        ItemChangeEventType.MASTER_SKU_UPDATE);
  }

  @Test
  public void updateMasterSkuSwitchOnTest() {
    ReflectionTestUtils.setField(itemServiceImpl, "checkIfMasterSkuChanged", true);
    item.setMasterSku(null);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    itemServiceImpl.updateMasterSku(masterSkuMapping);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    Mockito.verify(saveAndPublishService).saveItems(itemListCaptor.capture());
    Mockito.verify(cacheEvictHelperService).evictItemCache(STORE_ID, itemListCaptor.getValue().get(0));
    assertEquals(itemListCaptor.getValue().get(0).getMasterSku(), MASTER_SKU);
    assertEquals(itemListCaptor.getValue().get(0).getItemChangeEventTypes().get(0),
        ItemChangeEventType.MASTER_SKU_UPDATE);
  }

  @Test
  public void updateMasterSkuSwitchOn2Test() {
    ReflectionTestUtils.setField(itemServiceImpl, "checkIfMasterSkuChanged", true);
    item.setMasterSku(MASTER_SKU);
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(item);
    itemServiceImpl.updateMasterSku(masterSkuMapping);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
  }

  @Test
  public void updateMasterSkuExceptionTest() {
    Mockito.when(itemRepository.findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false)).thenReturn(null);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> itemServiceImpl.updateMasterSku(masterSkuMapping));
    } finally {
      Mockito.verify(itemRepository).findItemByStoreIdAndItemSku(STORE_ID, ITEM_SKU, false);
    }
  }

  @Test
  public void addRecipeToOtherSisterProductForSharedProductTest() throws Exception {
    item.setItemCode(ITEM_CODE);
    item.setItemSku(ITEM_SKU3);
    product.setTradingProduct(true);
    BundleRecipeRequest bundleRecipeRequest =
        new BundleRecipeRequest(ITEM_SKU, ImmutableSet.of(new BundleRecipeVo(ITEM_SKU2, 1)));
    Pair<String, BundleRecipeRequest> bundleRecipeRequestPair = Pair.of(ITEM_CODE, bundleRecipeRequest);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_CODE), false))
        .thenReturn(Arrays.asList(item, item));
    Mockito.when(saveOperationService.saveAndEvictItems(anyList())).thenReturn(Arrays.asList(item));
    Mockito.when(productService.getAllProducts(anyString(), anySet(), eq(false))).thenReturn(Arrays.asList(product));
    Mockito.when(saveOperationService.saveProduct(any())).thenReturn(product);

    itemServiceImpl.addRecipeToOtherSisterProductForSharedProduct(STORE_ID, Arrays.asList(bundleRecipeRequestPair), false);

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_CODE), false);
    Mockito.verify(saveOperationService).saveAndEvictItems(anyList());
    Mockito.verify(productService).getAllProducts(anyString(), anySet(), eq(false));
    Mockito.verify(saveOperationService).saveProduct(any());
  }

  @Test
  public void addRecipeToOtherSisterProductForSharedProductNoUpdatesTest() throws Exception {
    item.setItemCode(ITEM_CODE);
    BundleRecipeRequest bundleRecipeRequest =
        new BundleRecipeRequest(ITEM_SKU, ImmutableSet.of(new BundleRecipeVo(ITEM_SKU2, 1)));
    Pair<String, BundleRecipeRequest> bundleRecipeRequestPair = Pair.of(ITEM_CODE, bundleRecipeRequest);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_CODE), false))
        .thenReturn(Arrays.asList(item, item));

    itemServiceImpl.addRecipeToOtherSisterProductForSharedProduct(STORE_ID, Arrays.asList(bundleRecipeRequestPair), false);

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(ITEM_CODE), false);
  }

  @Test
  public void addRecipeToOtherSisterProductForSharedProductItemCodeNotFoundTest() throws Exception {
    item.setItemCode(ITEM_CODE);
    BundleRecipeRequest bundleRecipeRequest =
        new BundleRecipeRequest(ITEM_SKU, ImmutableSet.of(new BundleRecipeVo(ITEM_SKU2, 1)));
    Pair<String, BundleRecipeRequest> bundleRecipeRequestPair = Pair.of(SOURCE_ITEM_CODE, bundleRecipeRequest);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(SOURCE_ITEM_CODE), false))
        .thenReturn(Arrays.asList(item, item));
    itemServiceImpl.addRecipeToOtherSisterProductForSharedProduct(STORE_ID, Arrays.asList(bundleRecipeRequestPair), false);

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(SOURCE_ITEM_CODE), false);
  }

  @Test
  public void addRecipeToOtherSisterProductForSharedProductNonSharedProductTest() throws Exception {
    item.setItemCode(ITEM_CODE);
    BundleRecipeRequest bundleRecipeRequest =
        new BundleRecipeRequest(ITEM_SKU, ImmutableSet.of(new BundleRecipeVo(ITEM_SKU2, 1)));
    Pair<String, BundleRecipeRequest> bundleRecipeRequestPair = Pair.of(SOURCE_ITEM_CODE, bundleRecipeRequest);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(SOURCE_ITEM_CODE), false))
        .thenReturn(Arrays.asList(item));
    itemServiceImpl.addRecipeToOtherSisterProductForSharedProduct(STORE_ID, Arrays.asList(bundleRecipeRequestPair), false);

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeInAndMarkForDeleteFalse(STORE_ID, ImmutableSet.of(SOURCE_ITEM_CODE), false);
  }


  @Test
  public void addRecipeToOtherSisterProductForSharedProductNonBundleProductTest() throws Exception {
    item.setItemCode(ITEM_CODE);
    itemServiceImpl.addRecipeToOtherSisterProductForSharedProduct(STORE_ID, new ArrayList<>(), false);
  }

  @Test
  public void findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimaryTest() {
    Mockito.when(skuValidator.isItemSkuL4OrL5(ITEM_SKU_1)).thenReturn(true);
    itemServiceImpl.findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(STORE_ID, ITEM_SKU_1);
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU_1, true);
    Mockito.verify(skuValidator).isItemSkuL4OrL5(ITEM_SKU_1);
  }

  @Test
  public void findByStoreIdAndItemSkusReadFromPrimaryTest() {
    itemServiceImpl.findByStoreIdAndItemSkusReadFromPrimary(STORE_ID, new HashSet<>(ITEM_SKUS));
    Mockito.verify(itemRepository).findItemByStoreIdAndItemSkuIn(STORE_ID, new HashSet<>(ITEM_SKUS), true);
  }

  @Test
  public void findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedReadFromPrimaryTest() {
    itemServiceImpl.findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedReadFromPrimary(STORE_ID,
        PRODUCT_SKU, false);
    Mockito.verify(itemRepository).countByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(STORE_ID,
        PRODUCT_SKU, false, true);
  }

  @Test
  public void findItemsByStoreIdAndProductSkuAndMarkForDeleteReadFromPrimaryTest() {
    itemServiceImpl.findItemsByStoreIdAndProductSkuAndMarkForDeleteReadFromPrimary(STORE_ID, PRODUCT_SKU, false);
    Mockito.verify(itemRepository).findItemsByStoreIdAndProductSkuAndMarkForDelete(STORE_ID, PRODUCT_SKU, false, true);
  }

  @Test
  public void getBundleRecipeForSharedItemsTest() {
    item.setItemCode(ITEM_CODE);
    item.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU2, 1)));
    itemUpdated.setItemSku(ITEM_SKU2);
    itemUpdated.setItemCode(ITEM_CODE);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE))).thenReturn(Arrays.asList(item));
    Mockito.when(itemRepository.findItemsByStoreIdAndItemSkuIn(STORE_ID, Set.of(ITEM_SKU2))).thenReturn(Arrays.asList(itemUpdated));

    itemServiceImpl.getBundleRecipeForSharedItems(STORE_ID, Set.of(ITEM_CODE));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
    Mockito.verify(itemRepository).findItemsByStoreIdAndItemSkuIn(STORE_ID, Set.of(ITEM_SKU2));
  }

  @Test
  public void getBundleRecipeForSharedItemsBundleRecipeEmptyTest() {
    item.setItemCode(ITEM_CODE);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE))).thenReturn(Arrays.asList(item));

    itemServiceImpl.getBundleRecipeForSharedItems(STORE_ID, Set.of(ITEM_CODE));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
  }

  @Test
  public void getBundleRecipeForSharedItemsEmptyTest() {
    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE))).thenReturn(new ArrayList<>());

    itemServiceImpl.getBundleRecipeForSharedItems(STORE_ID, Set.of(ITEM_CODE));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
  }

  @Test
  public void updateRecipeForSharedProductsTest() throws Exception {
    Item item1 = new Item();
    item1.setItemCode(ITEM_CODE);
    item1.setItemSku(ITEM_SKU);
    item1.setProductSku(PRODUCT_SKU);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));

    Item item2 = new Item();
    item2.setItemCode(ITEM_CODE);
    item2.setItemSku(ITEM_SKU2);
    item2.setProductSku(PRODUCT_SKU);

    product.setTradingProduct(true);

    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE)))
        .thenReturn(Arrays.asList(item1, item2));
    Mockito.when(productService.getAllProducts(STORE_ID, Set.of(PRODUCT_SKU), true))
        .thenReturn(Arrays.asList(product));
    Mockito.when(saveOperationService.saveItemsWithoutUpdatingSolr(Arrays.asList(item2)))
        .thenReturn(Arrays.asList(item2));
    Mockito.when(saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP))
        .thenReturn(product);

    itemServiceImpl.updateRecipeForSharedProducts(STORE_ID, Arrays.asList(item1));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
    Mockito.verify(productService).getAllProducts(STORE_ID, Set.of(PRODUCT_SKU), true);
    Mockito.verify(saveOperationService).saveItemsWithoutUpdatingSolr(Arrays.asList(item2));
    Mockito.verify(saveOperationService).saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    Mockito.verify(saveAndPublishService).publishProductBundleOneToOneMappingEvent(anyList());
  }

  @Test
  public void updateRecipeForSharedProductsUnsharedItemst() throws Exception {
    Item item1 = new Item();
    item1.setItemCode(ITEM_CODE);
    item1.setItemSku(ITEM_SKU);
    item1.setProductSku(PRODUCT_SKU);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));

    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE)))
        .thenReturn(Arrays.asList(item1));

    itemServiceImpl.updateRecipeForSharedProducts(STORE_ID, Arrays.asList(item1));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
  }

  @Test
  public void updateRecipeForSharedProductsDifferentItemTest() throws Exception {
    Item item1 = new Item();
    item1.setItemCode(ITEM_CODE);
    item1.setItemSku(ITEM_SKU);
    item1.setProductSku(PRODUCT_SKU);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));

    Item item2 = new Item();
    item2.setItemCode(ITEM_CODE_NOT_ADD);
    item2.setItemSku(ITEM_SKU2);
    item2.setProductSku(PRODUCT_SKU);
    item2.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));

    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE)))
        .thenReturn(Arrays.asList(item1, item2));

    itemServiceImpl.updateRecipeForSharedProducts(STORE_ID, Arrays.asList(item1));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
  }

  @Test
  public void updateRecipeForSharedProductsNoItemsUpdatedTest() throws Exception {
    Item item1 = new Item();
    item1.setItemCode(ITEM_CODE);
    item1.setItemSku(ITEM_SKU);
    item1.setProductSku(PRODUCT_SKU);
    item1.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));

    Item item2 = new Item();
    item2.setItemCode(ITEM_CODE);
    item2.setItemSku(ITEM_SKU2);
    item2.setProductSku(PRODUCT_SKU);
    item2.setBundleRecipe(Set.of(new BundleRecipe(ITEM_SKU, 1)));

    Mockito.when(itemRepository.findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE)))
        .thenReturn(Arrays.asList(item1, item2));

    itemServiceImpl.updateRecipeForSharedProducts(STORE_ID, Arrays.asList(item1));

    Mockito.verify(itemRepository).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
  }

  @Test
  public void updateRecipeForSharedProductsEmptyItemsTest() throws Exception {
    itemServiceImpl.updateRecipeForSharedProducts(STORE_ID, new ArrayList<>());
    Mockito.verify(itemRepository, times(0)).findByStoreIdAndItemCodeIn(STORE_ID, Set.of(ITEM_CODE));
  }

  @Test
  public void addItems_cncForWarehouseOn() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    ReflectionTestUtils.setField(itemServiceImpl, "ranchIntegrationEnabled", true);
    cncItemViewConfig.setBuyable(true);
    businessPartnerPickupPoint.setCncActivated(true);
    itemPickupPoints.get(0).setPrice(prices);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    itemPickupPoints.forEach(itemPickupPoint1 -> itemPickupPoint1.setDistribution(true));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.setSourceItemCode(null);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(true);
    itemVo.getItemPickupPointVoList().get(0).getAllItemViewConfigs().add(cncItemViewConfig);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        List.of(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService, times(2)).getDefaultChannel();
    Mockito.verify(channelService, times(2)).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(true, itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouse_cncOffViewConfigOff() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoints.get(0).setPrice(prices);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).getAllItemViewConfigs().add(cncItemViewConfig);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        List.of(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService, times(2)).getDefaultChannel();
    Mockito.verify(channelService, times(3)).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouse_cncOffViewConfigBuyable() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoints.get(0).setPrice(prices);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    cncItemViewConfig.setBuyable(true);
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    businessPartnerPickupPoint.setCncActivated(true);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).getAllItemViewConfigs().add(cncItemViewConfig);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        List.of(PICKUP_POINT_CODE))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService, times(2)).getDefaultChannel();
    Mockito.verify(channelService, times(2)).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(true, itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouse_cncOffViewConfigDiscoverable() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoints.get(0).setPrice(prices);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    cncItemViewConfig.setDiscoverable(true);
    businessPartnerPickupPoint.setCncActivated(true);
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).getAllItemViewConfigs().add(cncItemViewConfig);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        List.of(itemPickupPoints.get(0).getPickupPointCode()))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService, times(2)).getDefaultChannel();
    Mockito.verify(channelService, times(2)).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(), any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertTrue(itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouse_cncOffViewConfigOnline() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoints.get(0).setPrice(prices);
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    cncItemViewConfig.setDiscoverable(true);
    cncItemViewConfig.setBuyable(true);
    businessPartnerPickupPoint.setCncActivated(true);
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).getAllItemViewConfigs().add(cncItemViewConfig);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    itemPickupPoints.get(0).setPickupPointCode(PICKUP_POINT_CODE);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(STORE_ID,
        List.of(itemPickupPoints.get(0).getPickupPointCode()))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService, times(2)).getDefaultChannel();
    Mockito.verify(channelService, times(2)).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertTrue(itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouseOn_pickupPointNonCnc() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    businessPartnerPickupPoint.setCncActivated(false);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(List.of(PICKUP_POINT_CODE)))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService, times(2)).getDefaultChannel();
    Mockito.verify(channelService, times(2)).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouseOff_cncFalse() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", false);
    ReflectionTestUtils.setField(itemServiceImpl, "ranchIntegrationEnabled", true);
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(true);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    businessPartnerPickupPoint.setCncActivated(false);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(List.of(PICKUP_POINT_CODE)))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(channelService).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouseOffDistribution() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", false);
    ReflectionTestUtils.setField(itemServiceImpl, "ranchIntegrationEnabled", true);
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(true);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().get(0).setDistribution(true);
    productItemsVo.getItemVoList().get(0).getItemPickupPointVoList().add(new ItemPickupPointVo());
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    businessPartnerPickupPoint.setCncActivated(false);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(List.of(PICKUP_POINT_CODE)))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(channelService).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void addItems_cncForWarehouseOff_cncFalse_cncViewConfigExists() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", false);
    ReflectionTestUtils.setField(itemServiceImpl, "ranchIntegrationEnabled", true);
    itemPickupPoints.get(0).setPrice(prices);
    for (Item item : this.listOfItems) {
      Mockito.when(this.productHelperService.setItemDetail(eq(this.STORE_ID), eq(this.PRODUCT_SKU_FOR_ADD_ITEMS), eq(this.MERCHANT_CODE),
              eq(this.product.getDefiningAttributes().size()), any(Item.class)))
          .thenReturn(item);
    }
    profileResponse.getCompany().setSalesChannel(Arrays.asList(B2B_SELLER_CHANNEL, B2C_SELLER_CHANNEL));
    Item existingItem = new Item();
    existingItem.setPristineDataItem(new PristineDataItem());
    itemPickupPoints.get(0).setItemViewConfig(new HashSet<>(Arrays.asList(itemViewConfig, b2bItemViewConfig, cncItemViewConfig)));
    ItemVo itemVo = gdnMapper.deepCopy(item, ItemVo.class);
    itemVo.setItemPickupPointVoList(
        itemPickupPoints.stream().map(itemPickupPoint -> gdnMapper.deepCopy(itemPickupPoint, ItemPickupPointVo.class))
            .collect(Collectors.toList()));
    itemVo.getItemPickupPointVoList().get(0).setWholesalePriceExists(true);
    itemVo.setWholesalePriceActivated(true);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setDiscoverable(false);
    itemVo.getItemPickupPointVoList().get(0).getItemViewConfig().iterator().next().setBuyable(false);
    itemVo.getItemPickupPointVoList().get(0).setCncActive(true);
    itemVo.getItemPickupPointVoList().get(0).setDistribution(true);
    itemVo.getItemPickupPointVoList().get(0).setPickupPointCode(PICKUP_POINT_CODE);
    itemVo.getItemPickupPointVoList().get(0).getAllItemViewConfigs().add(cncItemViewConfig);
    itemVo.setSourceItemCode(null);
    productDetailResponse.getProductItemResponses().forEach(item -> item.setSourceItemCode(null));
    Mockito.when(this.productHelperService.setItemDetail(anyString(), anyString(), anyString(),
        Mockito.anyInt(), any(ItemVo.class))).thenReturn(itemVo);
    Page<Item> itemPage =
        new PageImpl<Item>(Arrays.asList(existingItem), PageRequest.of(0, 10), 1);
    ProductItemsVo productItemsVo = generateProductItemVo(product, listOfItems.get(0), itemPickupPoints.get(0));
    Mockito.when(itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
            eq(this.STORE_ID), anyString(), any(Pageable.class)))
        .thenReturn(itemPage);
    Mockito.when(objectConverterService.convertToMasterDataItems(
        this.productDetailResponse.getProductItemResponses(),
        productDetailResponse.getProductCode())).thenReturn(mapOfMasterDataItems);
    Mockito.when(productHelperService.addItemAttributeToProductAttribute(any(Product.class),
        anyString(), anyList())).thenReturn(this.product);
    businessPartnerPickupPoint.setCncActivated(false);
    Mockito.when(this.businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(List.of(PICKUP_POINT_CODE)))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    List<ItemVo> items = this.itemServiceImpl.addItems(this.STORE_ID, this.REQUEST_ID, this.USERNAME,
        this.PRODUCT_SKU_FOR_ADD_ITEMS, productItemsVo.getItemVoList(), this.product, this.productDetailResponse,
        businessPartner);
    Mockito.verify(channelService).getDefaultChannel();
    Mockito.verify(channelService).getCncChannel();
    Mockito.verify(productHelperService, times(this.listOfItems.size())).setItemDetail(
        anyString(), anyString(), anyString(), Mockito.anyInt(),
        any(ItemVo.class));
    Mockito.verify(objectConverterService).convertToMasterDataItems(anySet(),
        anyString());
    Mockito.verify(productHelperService, Mockito.times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    Mockito.verify(productHelperService, times(this.listOfItems.size()))
        .addItemAttributeToProductAttribute(any(Product.class), anyString(),
            anyList());
    verify(this.saveOperationService).saveProductAndItemsAndPickupPoint(productArgumentCaptor.capture(),
        itemVoListArgumentCaptor.capture());
    Assertions.assertNotNull(items);
    Assertions.assertNotNull(items.get(0).getPristineDataItem());
    Assertions.assertTrue(items.get(0).isContentChanged());
    Assertions.assertTrue(items.get(0).isInitialContentChanged());
    Assertions.assertEquals(false, itemVo.isCncActive());
  }

  @Test
  public void existsRecordForStoreIdAndProductSkuAndCncActivated_true() throws Exception {
    Mockito.when(
            itemRepository.existsByStoreIdAndProductSkuAndCncActivated(eq(STORE_ID), eq(PRODUCT_SKU),
                eq(true)))
        .thenReturn(Boolean.TRUE);
    boolean result =
        itemServiceImpl.existsRecordForStoreIdAndProductSkuAndCncActivated(STORE_ID, PRODUCT_SKU,
            true);
    verify(itemRepository).existsByStoreIdAndProductSkuAndCncActivated(eq(STORE_ID), eq(PRODUCT_SKU),
        eq(true));
    Assertions.assertTrue(result);
  }

  @Test
  public void fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalseTest() {
    itemServiceImpl.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(STORE_ID,
        STORE_ID, new HashSet<>());
    Mockito.verify(itemRepository)
        .fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndMarkForDeleteAndIsArchived(STORE_ID, STORE_ID, new HashSet<>(),
            false, false);
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMpp_multiVariantTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    itemPickupPoint.getAllItemViewConfigs()
        .add(ItemViewConfig.builder().isDiscoverable(true).channel(Constants.CNC).build());
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(false);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU_1);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(false);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).setItemSku(ITEM_SKU_1);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item, item1));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Collections.singletonList(itemPickupPointNeedrevisonList.get(0)));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_1)).thenReturn(
        Collections.singletonList(itemPickupPointNeedrevisonList.get(1)));
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    EditItemResponse editItemResponse =
        itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE,
            itemActivationRequests, true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_1);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertTrue(editItemResponse.getUpdatedItems().stream().allMatch(Item::isCncActivated));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMpp_multiVarianBuyabletTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    itemPickupPoint.getAllItemViewConfigs()
        .add(ItemViewConfig.builder().isBuyable(true).channel(Constants.CNC).build());
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(false);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU_1);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(false);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).setItemSku(ITEM_SKU_1);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item, item1));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Collections.singletonList(itemPickupPointNeedrevisonList.get(0)));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_1)).thenReturn(
        Collections.singletonList(itemPickupPointNeedrevisonList.get(1)));
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    EditItemResponse editItemResponse =
        itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE,
            itemActivationRequests, true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_1);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertTrue(editItemResponse.getUpdatedItems().stream().allMatch(Item::isCncActivated));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMpp_singleVarianBuyabletTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    ItemPickupPoint itemPickupPoint1 = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, itemPickupPoint1);
    itemPickupPoint.getAllItemViewConfigs()
        .add(ItemViewConfig.builder().isBuyable(true).channel(Constants.CNC).build());
    itemPickupPoint1.getAllItemViewConfigs()
        .add(ItemViewConfig.builder().channel(Constants.CNC).build());
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(false);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU_1);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(false);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).setItemSku(ITEM_SKU_1);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    EditItemResponse editItemResponse =
        itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE,
            itemActivationRequests, true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    Assertions.assertTrue(editItemResponse.getUpdatedItems().stream().allMatch(Item::isCncActivated));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMpp_withPickupPointsTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(true);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(true);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1)))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Arrays.asList(PICKUP_POINT_CODE_2, PICKUP_POINT_CODE_1)));
  }

  @Test
  public void activateItemsOnNeedCorrectionWithMpp_multiVariant_withPickupPointDetailsTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    businessPartnerPickupPoint.setCode(PICKUP_POINT_CODE_1);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    itemPickupPoint.getAllItemViewConfigs()
        .add(ItemViewConfig.builder().isDiscoverable(true).channel(Constants.CNC).build());
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(false);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU_1);
    request2.setPickupPointCode(PICKUP_POINT_CODE_1);
    request2.setDiscoverable(false);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(1).setPickupPointCode(PICKUP_POINT_CODE_1);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).setItemSku(ITEM_SKU_1);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    item1.setItemSku(ITEM_SKU_1);
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Arrays.asList(PICKUP_POINT_CODE_1)))).thenReturn(Arrays.asList(businessPartnerPickupPoint));
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item, item1));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(
        Collections.singletonList(itemPickupPointNeedrevisonList.get(0)));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_1)).thenReturn(
        Collections.singletonList(itemPickupPointNeedrevisonList.get(1)));
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    EditItemResponse editItemResponse =
        itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE,
            itemActivationRequests, true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU_1);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Arrays.asList(PICKUP_POINT_CODE_1)));
    Assertions.assertTrue(editItemResponse.getUpdatedItems().stream().allMatch(Item::isCncActivated));
  }

  @Test
  void activateItemsOnNeedCorrectionWithMpp_requestBpFetchTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    NeedCorrectionItemActivationRequest request1 = new NeedCorrectionItemActivationRequest();
    request1.setItemSku(ITEM_SKU);
    request1.setPickupPointCode(PICKUP_POINT_CODE_1);
    request1.setBuyable(true);
    request1.setCncBuyable(true);
    request1.setCncDiscoverable(true);
    NeedCorrectionItemActivationRequest request2 = new NeedCorrectionItemActivationRequest();
    request2.setItemSku(ITEM_SKU);
    request2.setPickupPointCode(PICKUP_POINT_CODE_2);
    request2.setDiscoverable(true);
    request2.setCncBuyable(true);
    request2.setCncDiscoverable(true);
    itemActivationRequests = new ArrayList<>();
    itemActivationRequests.add(request1);
    itemActivationRequests.add(request2);
    itemPickupPointNeedrevisonList.get(0).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(1).getPrice().add(price);
    itemPickupPointNeedrevisonList.get(0).getAllItemViewConfigs().add(ItemViewConfig.builder().channel(Constants.CNC).isDiscoverable(false).build());
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = new ItemPickupPointDataChangeEventModel();
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Arrays.asList(item));
    when(itemPickupPointService.findByStoreIdAndItemSku(STORE_ID, ITEM_SKU)).thenReturn(itemPickupPointNeedrevisonList);
    when(objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any())).thenReturn(
        itemPickupPoint);
    when(objectConverterService.convertToItemPickupPointChangeEventModel(any(ItemPickupPoint.class),
        Mockito.anyBoolean())).thenReturn(itemPickupPointDataChangeEventModel);
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    when(businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2)))).thenReturn(
        Collections.singletonList(businessPartnerPickupPoint));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, itemActivationRequests,
        true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).findByStoreIdAndItemSku(STORE_ID, ITEM_SKU);
    verify(objectConverterService, times(2)).updateItemPickupPointOnNeedCorrectionActivation(any(), any(), any());
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
    verify(objectConverterService, times(2)).convertToItemPickupPointChangeEventModel(
        any(ItemPickupPoint.class), Mockito.anyBoolean());
    verify(itemPickupPointService, Mockito.times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP);
    verify(itemPickupPointService, times(2)).publishItemPickupPointDataChangeEventWithPureCncStatusChange(
        any(ItemPickupPointDataChangeEventModel.class), eq(Collections.EMPTY_MAP));
    verify(businessPartnerPickupPointService).getBusinessPartnerPickupPointByPickupPointCodes(eq(STORE_ID),
        eq(Arrays.asList(PICKUP_POINT_CODE_1, PICKUP_POINT_CODE_2)));
  }

  @Test
  void activateItemsOnNeedCorrectionWithMpp_emptyRequestsTest() throws Exception {
    ReflectionTestUtils.setField(itemServiceImpl, "clearScheduleForNeedRevision", true);
    ReflectionTestUtils.setField(itemServiceImpl, "cncForWarehouseFeatureSwitch", true);
    item.setItemSku(ITEM_SKU);
    when(cacheItemHelperService.findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU)).thenReturn(
        Collections.emptyList());
    when(itemPickupPointService.findByItemSkuAndDelivery(anyString(), anyString())).thenReturn(
        itemPickupPointNeedrevisonList.get(0));
    itemServiceImpl.activateItemsOnNeedCorrectionWithMpp(STORE_ID, PRODUCT_SKU, MERCHANT_CODE, Collections.emptyList(),
        true);
    verify(cacheItemHelperService).findCacheableByStoreIdAndProductSku(STORE_ID, PRODUCT_SKU);
    verify(itemPickupPointService).saveItemPickupPoint(anyList(), anyList());
  }

  @Test
  public void fetchUpcCodeStatus() throws Exception {
    Mockito.when(
        itemRepository.existsByStoreIdAndUpcCodeAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(
            eq(STORE_ID), eq("ABC-123"), eq(MERCHANT_CODE))).thenReturn(Boolean.TRUE);
    List<UpcStatusResponse> result =
        itemServiceImpl.fetchUpcCodeStatus(STORE_ID, MERCHANT_CODE, Set.of("ABC-123"));
    verify(itemRepository).existsByStoreIdAndUpcCodeAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(
        eq(STORE_ID), eq("ABC-123"), eq(MERCHANT_CODE));
    Assertions.assertTrue(result.get(0).isExists());
  }
}
