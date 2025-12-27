package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;
import static com.gdn.x.product.domain.event.enums.ProductFieldNames.ITEM_SKU;
import static java.util.function.Predicate.not;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.product.enums.DistributionStatus;
import com.gdn.x.product.rest.web.model.response.ItemBasicL4Response;
import com.gdn.x.product.rest.web.model.response.UpcStatusResponse;
import com.gdn.x.product.service.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Slice;
import org.springframework.data.domain.Sort;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.ext.catalog.FieldNames;
import com.gdn.partners.merchant.voucher.streaming.model.VoucherItemSkusEventModel;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.ItemInfo;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedDeactivatedEventModel;
import com.gdn.x.campaign.dto.UpdateDiscountDTO;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemPickupPointRepository;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.PristineItemRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.ItemPickupPointDataChangeEventModel;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.PristineCategory;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductReindexStatus;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SolrFieldNames;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.MasterDataItem;
import com.gdn.x.product.model.entity.MasterDataProduct;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.PristineDataItem;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductAndItemSolr;
import com.gdn.x.product.model.vo.BundleRecipeRequest;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.ComboRuleVO;
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
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingVO;
import com.gdn.x.product.outbound.api.XCampaignOutbound;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.ProductAndItemPickupPointDTO;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateItemRequest;
import com.gdn.x.product.rest.web.model.request.PickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductTypeEditRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.UpdateOfflineItemPriceRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.BasicItemDTO;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemCodeDetailResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel4ListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemLevel5Response;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.product.service.annotation.SolrIndex;
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
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ItemViewConfigService;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PriceHistoryService;
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
import com.gdn.x.product.service.task.SalesCatalogUpdateForPristineTask;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductAndItemsUtil;
import com.gdn.x.product.service.util.ResponseHelper;
import com.gdn.x.productcategorybase.ProductPublishEventType;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ItemServiceImpl implements ItemService {

  private static final String INVALID_ITEM_SKU_FORMAT = "Invalid item sku format : ";

  private static final String INVALID_PRODUCT_SKU_FORMAT = "Invalid product sku format : ";

  private static final String ITEM_NOT_FOUND_FOR_PRODUCT_WITH_PRODUCT_SKU =
      "item not found for product with product sku ";

  private static final String REQUEST_ID_MUST_NOT_BE_BLANK = "Request id must not be blank";

  private static final String ITEM_MUST_NOT_BE_NULL = "Item must not be null";

  private static final String ITEM_CODE_MUST_NOT_BE_BLANK = "Item code must not be blank";

  private static final String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK = "Pickup point code must not be blank";

  private static final String ITEM_SKU_MUST_NOT_BE_BLANK = "Item sku must not be blank";

  private static final String ITEM_SKUS_MUST_NOT_BE_EMPTY = "Item skus must not be empty";

  private static final String PRICE_MUST_NOT_BE_NULL = "Price must not be null";

  private static final String ITEM_NOT_FOUND = "Item not found ";

  private static final String PRODUCT_NOT_FOUND = "Product not found ";

  private static final String ITEM_PICKUPOINT_NOT_FOUND = "ItemPickupPoint not found";

  private static final String PRISTINE_DATA_ITEM_MUST_NOT_BE_NULL = "pristine data item must not be null";

  private static final String PRICE_MUST_HAVE_CHANNEL_DEFAULT =
      "Item Price must have channel DEFAULT";

  private static final String CHANNEL_DEFAULT_CANNOT_BE_DELETED =
      "channel default cannot be deleted";

  private static final String CHANNEL_NOT_FOUND = "channel not found";

  private static final String PRODUCT_MUST_NOT_BE_NULL = "product not found";

  private static final String PRODUCT_SKU_MUST_NOT_BE_BLANK = "productSku must not be blank";

  private static final String LIST_OF_ITEM_REQUEST_VO_MUST_NOT_BE_NULL = "items must not be null";

  private static final String ITEM_REQUEST_VO_MUST_NOT_BE_NULL =
      "listOfItemRequestVO must not be null";

  private static final String LIST_OF_ITEM_REQUEST_VO_MUST_NOT_BE_EMPTY_LIST =
      "listOfItemRequestVO must not be empty list";

  private static final String MERCHANT_CODE_MUST_NOT_BE_BLANK = "merchantCode must not be blank";

  private static final String MERCHANT_SKU_MUST_NOT_BE_BLANK = "merchantSku must not be blank";

  private static final String PRICE_WITH_THAT_CHANNEL_ALREADY_EXISTS =
      "price with that channel already exists";

  private static final String LIST_OF_PRISTINEIDS_MUST_NOT_BE_BLANK =
      "list of pristineIds must not be blank";

  private static final String LIST_OF_PRODUCT_SKUS_MUST_NOT_BE_BLANK =
      "list of productSkus must not be blank";

  private static final String PRISTINE_MASTER_IDS_SET_MUST_NOT_BE_EMPTY =
      "pristineMasterIds set must not be empty.";

  private static final String LIST_OF_MERCHANT_SKU_MUST_NOT_BE_BLANK =
      "list of merchantSku must not be blank";

  private static final String PROMO_BUNDLING_TYPE_MUST_NOT_BE_BLANK =
      "promo bundling type must not be blank";

  private static final Logger LOG = LoggerFactory.getLogger(ItemServiceImpl.class);

  private static final String ROUND_BRACKET_OPEN = "(";

  private static final String ROUND_BRACKET_CLOSE = ")";

  private static final String COMMA = ",";

  private static final String HYPHEN = "-";

  private static final String FORWARD_SLASH = "/";

  private static final String PRODUCT_CONDITION_NEW = "NEW";

  private static final String PRODUCT_CONDITION_BARU = "BARU";

  private static final String DEFAULT = "DEFAULT";

  private static final int DEFAULT_PAGE_SIZE = 500;

  private static final String ITEM_PICKUP_POINT_MUST_NOT_BE_NULL = "item pickupPoint must not null";

  private static final String SKU_SIZE_LIMIT_EXCEEDED = "ProductSku List size exceeded";
  private static final int ITEM_CHANGE_DEFAULT_PAGE_SIZE = 100;
  private static final double MINIMUM_DIMENSION_ALLOWED = 0.0;
  private static final String ID = "id";
  private static final String VERSION = "version";
  private static final String PRICE = "price";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String CNC_ACTIVE = "cncActive";
  private static final String OFFLINE_ITEM_ID = "offlineItemId";
  private static final String WHOLESALE_PRICE_EXISTS = "wholesalePriceExists";
  private static final String CREATED_DATE = "createdDate";
  private static final String CREATED_BY = "createdBy";
  private static final String UPDATED_DATE = "updatedDate";
  private static final String UPDATED_BY = "updatedBy";


  @Value("${product.max.shipping.weight}")
  private double maxShippingWeight;

  @Value("${validate.dimension.warehouse.event.switch.enabled}")
  private boolean validateDimensionSwitch;

  @Value("${publish.item.pickup.point.event.on.product.publish}")
  private boolean publishItemPickupPointChangeEventOnProductPublish;

  @Value("${publish.specific.item.data.change.event}")
  private boolean publishSpecificItemDataDataChangeEvent;

  @Value("${override.latefulfillment.by.product.type}")
  private boolean overrideLateFulfillmentByProductType;

  @Value("${delete.l3.l4.switch.enabled}")
  private boolean isDeleteL3AndL4Enabled;

  @Value("${suspension.publish.only.delivery.true.L5}")
  private boolean suspensionPublishOnlyDeliveryTrueL5;

  @Value("${check.if.master.sku.changed}")
  private boolean checkIfMasterSkuChanged;

  @Value("${clear.schedule.for.need.revision}")
  private boolean clearScheduleForNeedRevision;

  @Value("${update.product.in.wms.dimension.event}")
  private boolean updateProductInDimensionEvent;

  @Value("${skip.l4.event.publish.on.master.data.change}")
  private boolean skipL4EventPublishOnMasterDataChange;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ItemCacheableService itemCacheHelperService;

  @Autowired
  private ItemRepository itemRepository;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private ProductSearchService productSearchService;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ChannelService channelService;

  @Autowired
  @Lazy
  private ItemPriceService itemPriceService;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private PriceHistoryService priceHistoryService;

  @Autowired
  private ItemHelperService itemHelperService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private ExecutorService executorService;

  @Autowired
  private PristineItemRepository pristineItemRepository;

  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @Autowired
  private PromoBundlingService promoBundlingService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ItemViewConfigService itemViewConfigService;

  @Autowired
  private MasterDataCacheService masterDataCacheService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ProductL3SolrService productL3SolrService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private XCampaignOutbound xCampaignOutbound;

  @Autowired
  private ProductL3SolrReindexStatusService productL3SolrReindexStatusService;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Lazy
  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Lazy
  @Autowired
  private DataSourceWrapperService dataSourceWrapperService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private CachedService cachedService;

  @Autowired
  private ItemPickupPointRepository itemPickupPointRepository;

  @Autowired
  @Lazy
  private ItemPickupPointSummaryService itemPickupPointSummaryService;

  @Value("${storeid.default.value}")
  private String storeId;

  @Value("${username.default.value:username}")
  private String username;

  @Value("${requestId.default.value:requestId}")
  private String requestId;

  @Value("${update.sales.catalog.page.size:20}")
  private int size;

  @Value("#{${pristine.product.attribute.map}}")
  private Map<String, String> attributeMap;

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${product.generate.shipping.weight}")
  private boolean generateShippingWeight;

  @Value("${categories.to.generate.master.sku.in.flow2.product.creation}")
  private String categoriesToGenerateMasterSkuInFlow2ProductCreation;

  @Value("${skip.override.l5.data}")
  private boolean skipOverrideL5Data;

  @Value("${master.data.change.solr.reindex.enabled}")
  private boolean masterDataChangeSolrReindexEnabled;

  @Value("${fetch.item.pickup.point.without.delivery.en}")
  private boolean fetchItemPickupPointWithoutDelivery;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  /**
   * This api is not used anywhere. We  can clean it up later
   */
  @Override
  public boolean addItem(String storeId, String requestId, String username, String productSku,
      Item itemRequestVO) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ItemServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku),
        ItemServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(itemRequestVO != null, ItemServiceImpl.ITEM_REQUEST_VO_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(itemRequestVO.getItemCode()),
        ItemServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    checkState(this.productHelperService.containDefaultChannelPrice(itemRequestVO),
        ItemServiceImpl.PRICE_MUST_HAVE_CHANNEL_DEFAULT);
    Product product = this.productService.getProduct(storeId, productSku);
    checkState(product != null, ItemServiceImpl.PRODUCT_MUST_NOT_BE_NULL);

    Item item = this.productHelperService
        .setItemDetail(storeId, productSku, product.getMerchantCode(),
            product.getDefiningAttributes().size(), itemRequestVO);
    item.setPristineDataItem(this.getPristineDataItem(storeId, itemRequestVO.getItemCode()));
    item = this.productHelperService
        .setMasterDataItemFromMasterData(storeId, requestId, username, item);
    this.productHelperService.addItemAttributeToProductAttribute(product, item.getItemSku(),
        item.getMasterDataItem().getMasterDataItemAttributeValues());
    this.saveOperationService
        .saveProductAndItems(new ProductAndItemsVO(product, Arrays.asList(item)), new ArrayList<>());
    this.itemPriceService.publishItemPriceChangeEvent(username, requestId, product, item);
    saveAndPublishService.publishMerchantVoucherViewConfigChange(new ArrayList<>(), Arrays.asList(item));
    return true;
  }

  private PristineDataItem getPristineDataItem(String storeId, String itemCode) {
    PristineDataItem pristineDataItem = null;
    Pageable pageable = PageRequest.of(0, 10);
    Page<Item> existingItems = itemRepository
      .findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(storeId, itemCode, pageable);
    LOG.info("existingItems records : {}" + existingItems.getContent());
    if (CollectionUtils.isNotEmpty(existingItems.getContent())) {
      for (Item item : existingItems.getContent()) {
        if (item.getPristineDataItem() != null) {
          pristineDataItem = item.getPristineDataItem();
          break;
        }
      }
    }
    return pristineDataItem;
  }

  @Override
  public boolean addItemPrice(String storeId, Price price, String itemSku, String username) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(price != null, ItemServiceImpl.PRICE_MUST_NOT_BE_NULL);

    Item currentItem =
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
    checkState(currentItem != null, ItemServiceImpl.ITEM_NOT_FOUND);
    checkState(!currentItem.getPrice().contains(price),
        ItemServiceImpl.PRICE_WITH_THAT_CHANNEL_ALREADY_EXISTS);
    price.setLastUpdatedBy(username);
    price.setLastUpdatedDate(new Date());
    currentItem.getPrice().add(price);
    List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
    itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
    currentItem.setItemChangeEventTypes(itemChangeEventTypes);
    this.saveOperationService.saveItemWithoutUpdatingSolr(currentItem, null, false,
      StringUtils.EMPTY, Collections.EMPTY_MAP);
    this.productAndItemSolrIndexerService.updateSolrOnPriceChange(Arrays.asList(currentItem));
    return true;
  }

  @Override
  public boolean addItemPriceByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, Price price) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(price != null, ItemServiceImpl.PRICE_MUST_NOT_BE_NULL);

    List<Item> items =
        this.getItemsByMerchantSkuAndMerchantCode(storeId, merchantSku, merchantCode);
    checkState(!items.isEmpty(), ItemServiceImpl.ITEM_NOT_FOUND);
    for (Item item : items) {
      checkState(!item.getPrice().contains(price),
          ItemServiceImpl.PRICE_WITH_THAT_CHANNEL_ALREADY_EXISTS);
      item.getPrice().add(price);
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
    }
    this.saveOperationService.saveItems(items, null);
    return true;
  }

  @Override
  public List<ItemVo> addItems(String storeId, String requestId, String username, String productSku,
      List<ItemVo> listOfItemRequestVO, Product product,
      ProductDetailResponse productDetailResponse, BusinessPartner businessPartner) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ItemServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(product != null, ItemServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(listOfItemRequestVO != null, ItemServiceImpl.LIST_OF_ITEM_REQUEST_VO_MUST_NOT_BE_NULL);
    checkArgument(!listOfItemRequestVO.isEmpty(), ItemServiceImpl.LIST_OF_ITEM_REQUEST_VO_MUST_NOT_BE_EMPTY_LIST);
    Date timestamp = new Date();
    String defaultChannel = this.channelService.getDefaultChannel();
    int sizeOfProductAttributes = product.getDefiningAttributes().size();
    List<ItemVo> itemVos = new ArrayList<>();
    List<Item> updatedItems = new ArrayList<>();
    Map<String, String> mapOfItemCodeToSku = new HashMap<>();
    Map<String, ProductItemResponse> productItemResponsesMap = productDetailResponse.getProductItemResponses().stream()
        .collect(Collectors.toMap(productItemResponse -> productItemResponse.getSkuCode(), Function.identity(),
            (v1, v2) -> v2));

    int totalCountOfL5s = 0;
    int totalCountOfDistributionL5 = 0;
    for (ItemVo itemRequestVO : listOfItemRequestVO) {
      checkArgument(StringUtils.isNotBlank(itemRequestVO.getItemCode()),
          String.format("%s has no itemCode", itemRequestVO.getItemSku()));
      boolean containDefaultChannel = false;
      for (ItemPickupPointVo itemPickupPoint : itemRequestVO.getItemPickupPointVoList()) {
        totalCountOfL5s++;
        itemPickupPoint.setNewData(true);
        if (itemRequestVO.isForceReview()) {
          itemPickupPoint.setForceReview(true);
        }
        if(itemPickupPoint.isDistribution()) {
          totalCountOfDistributionL5++;
        }
        for (Price price : itemPickupPoint.getPrice()) {
          setPriceUpdatedByAndUpdatedDate(price, username, timestamp);
          if (defaultChannel.equals(price.getChannel())) {
            containDefaultChannel = true;
          }
        }
        checkState(containDefaultChannel == true, ItemServiceImpl.PRICE_MUST_HAVE_CHANNEL_DEFAULT);
      }
      //set channel and updated by and update data in price
      listOfItemRequestVO.forEach(itemVo -> itemVo.getItemPickupPointVoList().forEach(
          itemPickupPoint -> itemPickupPoint.getPrice()
              .forEach(price -> setPriceUpdatedByAndUpdatedDate(price, username, timestamp))));
      ItemVo itemVo = this.productHelperService.setItemDetail(storeId, productSku, product.getMerchantCode(),
          sizeOfProductAttributes, itemRequestVO);
      for (ItemPickupPointVo itemPickupPoint : itemVo.getItemPickupPointVoList()) {
        if (itemPickupPoint.isWholesalePriceExists() && Boolean.TRUE.equals(itemPickupPoint.getWholesalePriceActivated())) {
          Set<String> activePromoBundlingSet = new HashSet();
          activePromoBundlingSet.add(Constants.WHOLESALE_PRICE);
          itemPickupPoint.setActivePromoBundlings(activePromoBundlingSet);
          itemVo.setActivePromoBundlings(activePromoBundlingSet);
        }
        itemVo.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
      }
      ProductItemResponse productItemResponse = productItemResponsesMap.get(itemVo.getItemCode());
      itemVo.setContentChanged(productItemResponse.isContentChanged());
      itemVo.setInitialContentChanged(productItemResponse.isContentChanged());
      itemVo.setSourceItemCode(productItemResponse.getSourceItemCode());
      itemRequestVO.setGeneratedItemName(productItemResponse.getGeneratedItemName());
      itemRequestVO.setCategoryCode(
          productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode());
      itemRequestVO.setLength(productDetailResponse.getLength());
      itemRequestVO.setWeight(productDetailResponse.getWeight());
      itemRequestVO.setShippingWeight(productDetailResponse.getShippingWeight());
      itemRequestVO.setWidth(productDetailResponse.getWidth());
      itemRequestVO.setHeight(productDetailResponse.getHeight());
      itemRequestVO.setDangerousLevel(productItemResponse.getDangerousGoodsLevel());
      itemRequestVO.setMainImageUrl(
          productItemResponse.getImages().stream().filter(Image::isMainImages).map(Image::getLocationPath).findFirst()
              .orElse(StringUtils.EMPTY));
      itemRequestVO.setDefiningAttributes(productItemResponse.getProductItemAttributeValueResponses().stream().filter(
          productItemAttributeValueResponse -> CommonUtil.isDefiningOrVariantCreating(
              productItemAttributeValueResponse)).map(
          productItemAttributeValueResponse -> objectConverterService.toProductAttributeDetail(
              productItemAttributeValueResponse)).collect(Collectors.toList()));
      itemRequestVO.setBrand(productDetailResponse.getBrand());
      itemRequestVO.setUpcCode(productItemResponse.getUpcCode());
      updatedItems.addAll(setMasterSkuForFlow2CreationProducts(itemVo,
          itemRequestVO.getSourceItemCode(), productDetailResponse.getCategoryCodes()));
      itemVos.add(itemVo);
      mapOfItemCodeToSku.put(itemVo.getItemCode(), itemVo.getItemSku());
      sizeOfProductAttributes++;
    }

    Map<String, MasterDataItem> masterDataItems =
        this.objectConverterService.convertToMasterDataItems(productDetailResponse.getProductItemResponses(),
            productDetailResponse.getProductCode());

    for (Item itemRequestVO : listOfItemRequestVO) {
      String itemSkuCode = itemRequestVO.getItemCode();
      if (masterDataItems.containsKey(itemSkuCode)) {
        MasterDataItem masterDataItem = masterDataItems.get(itemSkuCode);
        product =
            this.productHelperService.addItemAttributeToProductAttribute(product, mapOfItemCodeToSku.get(itemSkuCode),
                masterDataItem.getMasterDataItemAttributeValues());
      }
    }
    checkDeliveryAndCncActiveFlag(storeId, itemVos);
    setCncAndOnlineAndArchiveAndB2cAndB2bActivatedFlag(product, itemVos, businessPartner);
    setDistributionStatus(product, totalCountOfDistributionL5, totalCountOfL5s);
    this.saveOperationService.saveProductAndItemsAndPickupPoint(product, itemVos);
    if (CollectionUtils.isNotEmpty(updatedItems)) {
      saveOperationService.saveItemsWithoutUpdatingSolr(updatedItems);
    }
    return itemVos;
  }

  private void setDistributionStatus(Product product, int totalCountOfDistributionL5, int totalCountOfL5s) {
    product.setDistributionStatus(DistributionStatus.NON_DISTRIBUTION);
    if (ranchIntegrationEnabled) {
      if (totalCountOfDistributionL5 == totalCountOfL5s) {
        product.setDistributionStatus(DistributionStatus.PURE_DISTRIBUTION);
      } else if (totalCountOfDistributionL5 > 0) {
        product.setDistributionStatus(DistributionStatus.DISTRIBUTION);
      }
    }
  }

  private List<Item> setMasterSkuForFlow2CreationProducts(ItemVo itemVo, String sourceItemCode,
      List<String> categoryCodesHierarchy) {
    List<Item> updatedItems = new ArrayList<>();
    if (StringUtils.isNotBlank(sourceItemCode) && !itemVo.isContentChanged()) {
      List<Item> sourceItems = fetchSourceItems(itemVo.getStoreId(), sourceItemCode);
      if (CollectionUtils.isNotEmpty(sourceItems)) {
        String masterSku = CommonUtil.findMasterSkuFromSourceItems(sourceItems);
        if (StringUtils.isBlank(masterSku) && CommonUtil.shouldCreateMasterSku(categoryCodesHierarchy,
            categoriesToGenerateMasterSkuInFlow2ProductCreation)) {
            masterSku = sourceItems.get(0).getItemSku();
          for (Item sourceItem : sourceItems) {
              sourceItem.setMasterSku(masterSku);
              sourceItem.getItemChangeEventTypes().add(ItemChangeEventType.MASTER_SKU_UPDATE);
            }
            updatedItems = sourceItems;
        }
        if (StringUtils.isNotBlank(masterSku)) {
          itemVo.setMasterSku(masterSku);
        }
      }
    }
    return updatedItems;
  }

  private List<Item> fetchSourceItems(String storeId, String sourceItemCode) {
    List<Item> sourceItems = new ArrayList<>();
    Pageable pageable = PageRequest.of(0, 10);
    Page<Item> items;
    do {
      items = itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalseAndIsSynchronizedTrue(
          storeId, sourceItemCode, pageable);
      sourceItems.addAll(items.getContent());
      pageable = items.nextPageable();
    } while (items.hasNext());
    return sourceItems;
  }

  private void setCncAndOnlineAndArchiveAndB2cAndB2bActivatedFlag(Product product, List<ItemVo> itemVos,
      BusinessPartner businessPartner) {
    CommonUtil.checkEligibilityForB2cAndB2bActivated(product, itemVos, businessPartner);
    for (ItemVo itemVo : itemVos) {
      if (CollectionUtils.isEmpty(itemVo.getItemPickupPointVoList())) {
        itemVo.setArchived(true);
      }
      for (ItemPickupPointVo itemPickupPointVo : itemVo.getItemPickupPointVoList()) {
        boolean cncActive = !cncForWarehouseFeatureSwitch && itemPickupPointVo.isCncActive();
        boolean cncBuyable = cncForWarehouseFeatureSwitch &&
            itemPickupPointVo.getAllItemViewConfigs().stream().anyMatch(
            itemViewConfig ->
                channelService.getCncChannel().equalsIgnoreCase(itemViewConfig.getChannel()) && (itemViewConfig.isBuyable() || itemViewConfig.isDiscoverable()));
        boolean result = cncActive || cncBuyable;
        if (result) {
          product.setCncActivated(true);
          itemVo.setCncActive(true);
          itemVo.setCncActivated(true);
          product.setB2cActivated(true);
        }
        if (itemPickupPointVo.getItemViewConfig().iterator().next().isBuyable() || itemPickupPointVo.getItemViewConfig()
            .iterator().next().isDiscoverable()) {
          product.setOnline(true);
          product.setB2cActivated(true);
        }
        ItemViewConfig b2bItemViewConfig =  itemPickupPointVo.getAllItemViewConfigs().stream()
            .filter(itemViewConfig -> Constants.B2B.equals(itemViewConfig.getChannel())).findFirst().orElse(null);
        if (Objects.nonNull(b2bItemViewConfig) && (b2bItemViewConfig.isBuyable() || b2bItemViewConfig.isDiscoverable())) {
          product.setB2bActivated(true);
        }
      }
    }
  }

  private void checkDeliveryAndCncActiveFlag(String storeId, List<ItemVo> itemVos) {
    if (cncForWarehouseFeatureSwitch) {
      checkViewConfigsForCncSwitchOn(storeId, itemVos);
    } else {
      List<String> pickupPointCodes =
        itemVos.stream().flatMap(itemVo -> itemVo.getItemPickupPointVoList().stream())
          .filter(ItemPickupPointVo::isCncActive).map(ItemPickupPointVo::getPickupPointCode)
          .distinct().collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
        checkCncStatusForCncForWarehouseSwitchOff(storeId, itemVos, pickupPointCodes);
      }
    }
  }

  private void checkCncStatusForCncForWarehouseSwitchOff(String storeId, List<ItemVo> itemVos, List<String> pickupPointCodes) {
    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList =
      businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId, pickupPointCodes);
    Map<String, Boolean> cncActiveMap = businessPartnerPickupPointList.stream().collect(
      Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::isCncActivated));
    List<ItemPickupPointVo> cncActiveL5s =
      itemVos.stream().flatMap(itemVo -> itemVo.getItemPickupPointVoList().stream())
        .filter(ItemPickupPoint::isCncActive).collect(toList());
    cncActiveL5s.forEach(itemPickupPointVo -> itemPickupPointVo.setCncActive(
      cncActiveMap.getOrDefault(itemPickupPointVo.getPickupPointCode(), false)));
    cncActiveL5s.forEach(itemPickupPointVo -> {
      ItemViewConfig cncItemViewConfig =
        itemPickupPointVo.getItemViewConfigByChannel(channelService.getCncChannel()).stream()
          .findFirst().orElse(null);
      if (Objects.nonNull(cncItemViewConfig)) {
        cncItemViewConfig.setDiscoverable(itemPickupPointVo.isCncActive());
        cncItemViewConfig.setBuyable(itemPickupPointVo.isCncActive());
      }
    });
  }

  private void checkViewConfigsForCncSwitchOn(String storeId, List<ItemVo> itemVos) {
    List<String> pickupPointCodes = itemVos.stream().flatMap(itemVo -> itemVo.getItemPickupPointVoList().stream())
      .map(ItemPickupPointVo::getPickupPointCode).distinct()
      .collect(Collectors.toList());
    List<BusinessPartnerPickupPoint> businessPartnerPickupPointList =
      businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
        pickupPointCodes);
    Map<String, Boolean> cncActiveMap = businessPartnerPickupPointList.stream().collect(
      Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::isCncActivated));
    Map<String, Boolean> deliveryActiveMap = businessPartnerPickupPointList.stream().collect(
      Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::isDelivery));
    //Validate cnc view config using pickupPoint's cncActivated flag
    itemVos.stream().flatMap(itemVo -> itemVo.getItemPickupPointVoList().stream())
      .forEach(itemPickupPointVo -> validateItemViewConfig(itemPickupPointVo,
        channelService.getCncChannel(),
        cncActiveMap.getOrDefault(itemPickupPointVo.getPickupPointCode(), false)));
    //Validate default view config using pickupPoint's delivery flag
    itemVos.stream().flatMap(itemVo -> itemVo.getItemPickupPointVoList().stream())
      .forEach(itemPickupPointVo -> validateItemViewConfig(itemPickupPointVo,
        channelService.getDefaultChannel(),
        deliveryActiveMap.getOrDefault(itemPickupPointVo.getPickupPointCode(), false)));
  }

  private void validateItemViewConfig(ItemPickupPointVo itemPickupPointVo, String channel,
    Boolean channelAllowed) {
    ItemViewConfig itemViewConfig =
      itemPickupPointVo.getItemViewConfigByChannel(channel).stream().findFirst().orElse(null);
    if (Objects.nonNull(itemViewConfig) && Boolean.FALSE.equals(channelAllowed)) {
      itemViewConfig.setBuyable(false);
      itemViewConfig.setDiscoverable(false);
      //TODO - BM-13866 - Check cncActive or delivery flag to be set or not
    }
  }

  private void setPriceUpdatedByAndUpdatedDate(Price price, String username, Date timestamp) {
    price.setLastUpdatedBy(username);
    price.setLastUpdatedDate(timestamp);
  }

  @Override
  public Item addActivePromoBundling(String storeId, String itemSku, String promoBundlingType) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(promoBundlingType),
        ItemServiceImpl.PROMO_BUNDLING_TYPE_MUST_NOT_BE_BLANK);
    return this.saveOperationService.addActivePromoBundling(storeId, itemSku, promoBundlingType);
  }

  @Override
  public Item removeActivePromoBundling(String storeId, String itemSku, String promoBundlingType) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(promoBundlingType),
        ItemServiceImpl.PROMO_BUNDLING_TYPE_MUST_NOT_BE_BLANK);

    return this.saveOperationService.removeActivePromoBundling(storeId, itemSku, promoBundlingType);
  }

  @Override
  public void processPromoBundlingStatusChangedEvent(String storeId, String itemSku, String promoBundlingType,
      boolean promoBundlingActivated, Boolean wholesalePriceActivated) {
    if (promoBundlingActivated) {
      Item item = saveOperationService.updateActivePromoBundling(storeId, itemSku, promoBundlingType);
      updateSolrAndPublishEventOnWholesalePriceChanges(item, wholesalePriceActivated, item.getMerchantCode());
    } else {
      if (Objects.isNull(wholesalePriceActivated)) {
        Item item = saveOperationService.removeActivePromoBundling(storeId, itemSku, promoBundlingType);
        this.productL3SolrService.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
      } else if (wholesalePriceActivated) {
        Item item = saveOperationService.updateActivePromoBundling(storeId, itemSku, Constants.WHOLESALE_PRICE);
        updateSolrAndPublishEventOnWholesalePriceChanges(item, wholesalePriceActivated, item.getMerchantCode());
      } else {
        Item item = saveOperationService.removeActivePromoBundling(storeId, itemSku, promoBundlingType);
        updateSolrAndPublishEventOnWholesalePriceChanges(item, wholesalePriceActivated, item.getMerchantCode());
      }
    }
  }

  @Override
  public void processPromoBundlingStatusChangedEventInItemPickupPoint(String storeId, String itemSku,
      String promoBundlingType, boolean promoBundlingActivated, Boolean wholesalePriceActivated) {
    ItemPickupPoint itemPickupPoint = null;
    if (promoBundlingActivated) {
      itemPickupPoint = itemPickupPointService.updateActivePromoBundling(storeId, itemSku, promoBundlingType, false);
    } else {
      if (Objects.isNull(wholesalePriceActivated)) {
        itemPickupPoint = itemPickupPointService.updateActivePromoBundling(storeId, itemSku, promoBundlingType, true);
      } else if (wholesalePriceActivated) {
        itemPickupPoint =
            itemPickupPointService.updateActivePromoBundling(storeId, itemSku, Constants.WHOLESALE_PRICE, false);
      } else {
        itemPickupPoint = itemPickupPointService.updateActivePromoBundling(storeId, itemSku, promoBundlingType, true);
      }
    }
    if (Objects.nonNull(itemPickupPoint)) {
      Item item = overrideL5DetailsAndClearCache(itemPickupPoint);
      updateSolrAndPublishEventOnWholesalePriceChanges(item, wholesalePriceActivated, item.getMerchantCode());
    }
  }

  @Override
  public ItemPickupPoint processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(String storeId, String itemSku,
      String promoBundlingType, boolean promoBundlingActivated, Boolean wholesalePriceActivated, String ppCode) {
    ItemPickupPoint itemPickupPoint = null;
    if (promoBundlingActivated) {
      itemPickupPoint =
          itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(storeId, itemSku, promoBundlingType, false,
              ppCode);
    } else {
      if (Objects.isNull(wholesalePriceActivated)) {
        itemPickupPoint =
            itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(storeId, itemSku, promoBundlingType,
                true, ppCode);
      } else if (wholesalePriceActivated) {
        itemPickupPoint = itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(storeId, itemSku,
            Constants.WHOLESALE_PRICE, false, ppCode);
      } else {
        itemPickupPoint =
            itemPickupPointService.updateActivePromoBundlingByItemSkuAndPPCode(storeId, itemSku, promoBundlingType,
                true, ppCode);
      }
    }

    if (Objects.nonNull(itemPickupPoint)) {
      updateSolrAndPublishEventOnWholesalePriceChangesForItemPickupPoint(itemPickupPoint, wholesalePriceActivated,
          itemPickupPoint.getMerchantCode());
    }
    return itemPickupPoint;
  }

  private Item overrideL5DetailsAndClearCache(ItemPickupPoint itemPickupPoint) {
    Item item = itemRepository.findItemByStoreIdAndItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku(), false);
    item.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    cacheEvictHelperService.evictItemData(itemPickupPoint.getStoreId(), item);
    cacheEvictItemService.evictFindL5ByItemSku(itemPickupPoint.getStoreId(), itemPickupPoint.getItemSku());
    return item;
  }

  private void updateSolrAndPublishEventOnWholesalePriceChanges(Item item, Boolean wholesalePriceActivated, String merchantCode) {
    if (Objects.nonNull(wholesalePriceActivated)) {
      productAndItemSolrIndexerService.updateWholesalePriceActivatedFlag(item.getItemSku(), wholesalePriceActivated);
      productL3SolrService.updatePromoOrWholesaleItemSkus(Arrays.asList(item), false);
      saveAndPublishService.publishWholesalePriceActivatedOrDeactivatedEvent(item.getItemSku(), wholesalePriceActivated, merchantCode);
    }
  }

  private void updateSolrAndPublishEventOnWholesalePriceChangesForItemPickupPoint(ItemPickupPoint itemPickupPoint,
      Boolean wholesalePriceActivated, String merchantCode) {
    if (Objects.nonNull(wholesalePriceActivated)) {
      saveAndPublishService.publishWholesalePriceActivatedOrDeactivatedEvent(itemPickupPoint.getItemSku(),
          wholesalePriceActivated, merchantCode);
    }
  }

  @Override
  @SolrIndex
  public List<Item> assignTicketTemplateToItems(String storeId, List<String> itemSkus,
      String ticketTemplateCode) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!itemSkus.isEmpty(), CommonConstants.ITEM_SKUS_MUST_NOT_BE_EMPTY);
    List<Item> result =
        this.itemRepository.assignTicketTemplate(storeId, itemSkus, ticketTemplateCode);
    for (Item item : result) {
      this.cacheEvictHelperService.evictItemData(storeId, item);
    }
    return result;
  }


  @Override
  public boolean deleteItem(String storeId, String itemSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);

    Item item =
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
    checkState(item != null, ItemServiceImpl.ITEM_NOT_FOUND + itemSku);
    item.setMarkForDelete(true);
    Product product = this.productService.getProduct(storeId, item.getProductSku());
    checkState(product != null, ItemServiceImpl.PRODUCT_NOT_FOUND + item.getProductSku());
    product = this.productHelperService.deleteItemAttributeFromProductAttribute(product, itemSku);
    if (product.getDefiningAttributes().isEmpty()) {
      product.setMarkForDelete(true);
    }
    this.saveOperationService
        .saveProductAndItems(new ProductAndItemsVO(product, Arrays.asList(item)), new ArrayList<>());
    return true;
  }

  @Override
  public boolean deleteItemPrice(String storeId, String itemSku, String channel)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    checkArgument(channel != null, ItemServiceImpl.PRICE_MUST_NOT_BE_NULL);
    checkArgument(!this.channelService.getDefaultChannel().equals(channel),
        ItemServiceImpl.CHANNEL_DEFAULT_CANNOT_BE_DELETED);

    Item item =
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
    checkState(item != null, ItemServiceImpl.ITEM_NOT_FOUND);

    for (Price price : item.getPrice()) {
      if (price.getChannel().equals(channel)) {
        this.priceHistoryService
            .savePriceHistory(this.objectConverterService.convertToPriceHistory(price, itemSku));
        item.getPrice().remove(price);
        List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
        itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
        item.setItemChangeEventTypes(itemChangeEventTypes);
        this.saveOperationService.saveItemWithoutUpdatingSolr(item, null, false, StringUtils.EMPTY, Collections.EMPTY_MAP);
        this.productAndItemSolrIndexerService.updateSolrOnPriceChange(Arrays.asList(item));
        return true;
      }
    }
    throw new Exception(ItemServiceImpl.CHANNEL_NOT_FOUND);
  }

  @Override
  public List<Item> deleteItemByStoreIdAndProductSkus(String storeId, Set<String> productSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(productSkus), ErrorMessages.PRODUCT_SKU_SET_MUST_NOT_BE_EMPTY);
    return itemRepository.deleteByStoreIdAndProductSkuIn(storeId, productSkus);
  }

  @Override
  public Item findByStoreIdAndItemSkuAndMarkForDeleteFalse(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku),
        ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    return this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
  }

  @Override
  public Item findByStoreIdAndItemSkuAndMarkForDeleteFalseReadFromPrimary(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku),
        ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    return this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, true);
  }

  @Override
  public Item findByStoreIdAndItemSku(String storeId, String itemSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku), ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    return this.itemRepository.findItemByStoreIdAndItemSku(storeId, itemSku, false);
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId,
      String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ItemServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    return this.itemRepository
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSku(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku), ItemServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    return this.itemRepository.findItemsByStoreIdAndProductSku(storeId, productSku);
  }

  @Override
  public List<Item> getItemPriceAndViewConfigsAndPromoDetails(String storeId, List<String> itemSkus) {
    return this.itemRepository.getItemPriceAndViewConfigsAndPromoDetails(storeId, itemSkus);
  }

  @Override
  public Item getItem(String storeId, String requestId, String username, String itemSku, boolean needMasterDataDetail,
      boolean fullFetch, boolean combineOthersBundlings, boolean instantPickup, String pickupPointCode, boolean off2On,
      boolean fetchMfdTrueItem)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ItemServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku),
        ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    Item item;
    if (fetchMfdTrueItem) {
      item =
          this.itemCacheHelperService.findItemByStoreIdAndItemSku(storeId, itemSku, fullFetch, combineOthersBundlings,
              instantPickup, pickupPointCode, off2On);
      if (Objects.isNull(item)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ApiErrorCode.ITEM_NOT_FOUND.getCode());
      } else if (item.isMarkForDelete()) {
        Product product =
            productCacheableService.findProductByStoreIdAndProductSku(item.getStoreId(), item.getProductSku());
        if (product.isMarkForDelete() && product.isSuspended()) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.ITEM_IS_SUSPENDED.getCode());
        }
      }
    } else {
      item =
          this.itemCacheHelperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, fullFetch,
              combineOthersBundlings, instantPickup, pickupPointCode, off2On, false);
    }
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    if (Objects.nonNull(itemPickupPoint)) {
      objectConverterService.overrideL4DetailsFromL5(Arrays.asList(item), Arrays.asList(itemPickupPoint));
      if (item != null && fullFetch) {
        item = getItemsWithDiscount(storeId, username, requestId, Arrays.asList(item), false).get(0);
      }
    }
    return item;
  }

  @Override
  public ItemAndItemPickupPointVo getItemDetails(String storeId, String itemSku, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku),
        ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    ItemAndItemPickupPointVo itemAndItemPickupPointVo = this.itemCacheHelperService
        .findItemByItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSku, pickupPointCode);
    return itemAndItemPickupPointVo;
  }

  @Override
  public ItemAndItemPickupPointVo getItemDetailsFromDB(String storeId, String itemSku, String pickupPointCode) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku), ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    Item item = itemRepository.findItemByStoreIdAndItemSku(storeId, itemSku, false);
    if (Objects.isNull(item) || (item.isMarkForDelete() && !item.isForceReview())) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.INVALID_PRODUCT.getErrorMessage(),
          ApiErrorCodes.INVALID_PRODUCT.getErrorCode());
    } else if (item.isForceReview()) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.TAKEN_DOWN.getErrorMessage(),
          ApiErrorCodes.TAKEN_DOWN.getErrorCode());
    }
    ItemPickupPoint itemPickupPoint =
        this.itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(storeId, itemSku, pickupPointCode);
    if (Objects.isNull(itemPickupPoint) || itemPickupPoint.isMarkForDelete()) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.INVALID_PRODUCT.getErrorMessage(),
          ApiErrorCodes.INVALID_PRODUCT.getErrorCode());
    }

    setActivePromoBundlingsByPristineOrItemCode(storeId, item, itemPickupPoint);
    return new ItemAndItemPickupPointVo(Arrays.asList(item), Arrays.asList(itemPickupPoint), 1);

  }

  private void setActivePromoBundlingsByPristineOrItemCode(String storeId, Item item, ItemPickupPoint itemPickupPoint) {
    if (item.isSynchronized()) {
      if (Objects.nonNull(item.getPristineDataItem())) {
        itemPickupPoint.setActivePromoBundlings(itemHelperService
            .findActivePromoBundlingsByStoreIdAndPristineAndPickupPointCode(storeId, item.getPristineDataItem(),
                itemPickupPoint.getPickupPointCode()));
      } else if (StringUtils.isNotBlank(item.getItemCode())) {
        itemPickupPoint.setActivePromoBundlings(itemHelperService
            .findActivePromoBundlingsByStoreIdAndItemCodeAndPickupPointCode(storeId, item.getItemCode(),
                itemPickupPoint.getPickupPointCode()));
      }
    }
  }

  @Override
  public ItemAndItemPickupPointVo getItemAndPickupPointDetails(String storeId, List<String> productSkus
      , List<String> itemSku, boolean showDeleted, int page, int pageSize) {
    return this.itemCacheHelperService
        .findItemAndItemPickPointByproductSkus(storeId, productSkus, itemSku, showDeleted, page, pageSize);
  }

  @Override
  public Item getDetailsForActiveOrSuspendedItem(String storeId, String requestId, String username, String itemSku,
      boolean needMasterDataDetail, boolean fullFetch, boolean combineOthersBundlings, boolean instantPickup,
      String pickupPointCode, boolean off2On) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ItemServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSku), ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    Item item = this.itemCacheHelperService
        .findItemByStoreIdAndItemSku(storeId, itemSku, fullFetch, combineOthersBundlings, instantPickup,
            pickupPointCode, off2On);
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    if (skipOverrideL5Data && Objects.nonNull(itemPickupPoint)) {
      objectConverterService.overrideL4DetailsFromL5(Arrays.asList(item), Arrays.asList(itemPickupPoint));
    }
    if (Objects.nonNull(item) && fullFetch) {
      item = getItemsWithDiscount(storeId, username, requestId, Arrays.asList(item), false).get(0);
    }
    return item;
  }

  @Override
  public Item getItem(String storeId, String requestId, String username, String itemSku,
      boolean needMasterDataDetail) throws Exception {
    return getItem(storeId, requestId, username, itemSku, needMasterDataDetail, true, false, false, null, false, false);
  }

  @Override
  public Map<String, List<Item>> getItemAvailability(String storeId, Set<String> productSkus) {
    return this.itemRepository.getItemAvailability(storeId, productSkus).stream()
        .map(item -> {
          item.setUniqueId(item.getItemSku());
          return item;
        })
        .collect(groupingBy(Item::getProductSku, mapping(Function.identity(), toList())));
  }

  @Override
  public List<Item> getItemPriceAndOff2OnChannelActive(String storeId, List<String> itemSkus) {
    return this.itemRepository.getPriceAndOff2OnChannelActive(storeId, itemSkus);
  }

  @Override
  public List<Item> getItemPriceAndViewConfigs(String storeId, List<String> itemSkus) {
    return this.itemRepository.getPriceAndViewConfigs(storeId, itemSkus);
  }

  @Override
  public List<Item> getItemsByMerchantSkuAndMerchantCode(String storeId, String merchantSku,
      String merchantCode) {
    List<Item> items = this.itemRepository
        .findItemsByStoreIdAndMerchantSkuAndMarkForDeleteFalse(storeId, merchantSku);
    List<Product> products = this.productService.getProductsByMerchantCode(storeId, merchantCode);
    List<String> productSkus = new ArrayList<>();
    for (Product product : products) {
      productSkus.add(product.getProductSku());
    }

    List<Item> itemsWithSpecifiedMerchantCode = new ArrayList<>();
    for (Item item : items) {
      if (productSkus.contains(item.getProductSku())) {
        itemsWithSpecifiedMerchantCode.add(item);
      }
    }
    return itemsWithSpecifiedMerchantCode;
  }

  @Override
  public List<Item> getItemsByProductSku(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ItemServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);

    return this.itemCacheHelperService
        .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false, false,
          false);
  }

  @Override
  public List<Item> getItemsByProductSkuAndCncActivated(String storeId, String username,
      String requestId, String productSku, boolean cncActivated) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ItemServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);

    List<Item> items = this.itemCacheHelperService
        .findItemsByStoreIdAndProductSkuAndCncActivatedAndMarkForDeleteFalse(storeId, productSku, cncActivated);
    return this.getItemsWithDiscount(storeId, username, requestId, items, false);
  }

  @Override
  public List<Item> getItemsForViewByProductSku(String storeId, String requestId, String username, String productSku,
      boolean isSynchronized, String level2MerchantCode, boolean showDeleted, boolean combineOthersBundlings,
      boolean off2On, boolean isMigrateAndSyncProduct)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ItemServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    List<Item> items = null;
    if ((Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED).getValue()))) {
      if (showDeleted) {
        items = this.itemCacheHelperService.findItemsByStoreIdAndProductSku(storeId, productSku, combineOthersBundlings,
            off2On, isMigrateAndSyncProduct);
      } else {
        items = this.itemCacheHelperService
            .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, combineOthersBundlings, off2On,
              false);
      }
    } else {
      items = this.itemCacheHelperService
          .findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, combineOthersBundlings, off2On,
            false);
    }
    checkArgument(!items.isEmpty(),
        ItemServiceImpl.ITEM_NOT_FOUND_FOR_PRODUCT_WITH_PRODUCT_SKU + productSku);
    return this.getItemsWithDiscount(storeId, username, requestId, items, isMigrateAndSyncProduct);
  }

  private List<Item> getItemsWithDiscount(String storeId, String username, String requestId, List<Item> items,
      boolean isMigrateAndSyncProduct) {
    if (CollectionUtils.isEmpty(items)) {
      return new ArrayList<>();
    }
    List<String> itemSkus = items.stream().map(Item::getItemSku).collect(toList());
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    if (fetchItemPickupPointWithoutDelivery && isMigrateAndSyncProduct) {
      itemPickupPoints = itemPickupPointService.findOneForEachItemSkuIn(storeId, itemSkus);
    } else {
      itemPickupPoints = itemPickupPointService.findByItemSkusAndDelivery(storeId, itemSkus, true);
    }
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
    Map<String, Set<Price>> prices = this.itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
    for (Item item : items) {
      CommonUtil.setItemPickupPointDetailsInItem(item, itemPickupPointMap.get(item.getItemSku()), false);
      item.setPrice(prices.get(item.getItemSku()));
    }
    return items;
  }

  @Override
  public List<Item> getItemsForViewByProductSkuAndPickUpPoint(String storeId, String productSku, boolean showDeleted,
      boolean combineOthersBundlings, boolean off2On, Map<String, ItemPickupPoint> itemPickupPointMap) throws Exception {
    showDeleted = Boolean.valueOf(systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.FETCH_ALL_ITEMS_BY_PRODUCT_SKU_ENABLED).getValue()) && showDeleted;
    List<Item> items = this.itemCacheHelperService.findItemsByStoreIdAndProductSkuAndShowDeletedFlag(storeId,
            productSku, combineOthersBundlings, off2On, showDeleted, itemPickupPointMap);
    checkArgument(CollectionUtils.isNotEmpty(items),
        ItemServiceImpl.ITEM_NOT_FOUND_FOR_PRODUCT_WITH_PRODUCT_SKU + productSku);
    return items;
  }

  @Override
  public List<Item> getItemsWithDiscountAndPickUpPointDetails(String storeId,
      String productSku, String pickupPointCode, List<Item> items, List<ItemPickupPoint> itemPickupPoints,
      Map<String, ItemPickupPoint> itemPickupPointMap, boolean validateViewConfig) {
    List<String> itemSkus = itemPickupPoints.stream().map(ItemPickupPoint::getItemSku).collect(toList());
    List<Item> updatedItems = items.stream().filter(item -> itemSkus.contains(item.getItemSku())).collect(toList());
    Map<String, Set<Price>> prices = this.itemPriceService.getDiscountItemPickupPoint(itemPickupPoints);
    for (Item item : updatedItems) {
      CommonUtil.setItemPickupPointDetailsInItemWithCncFlagUpdate(item, itemPickupPointMap.get(item.getItemSku()), validateViewConfig);
      item.setPrice(prices.get(item.getItemSku()));
    }
    return updatedItems;
  }

  @Override
  public List<Item> getItemsWithDiscountPriceByProductSkus(String storeId, String username, String requestId,
      Set<String> productSkus, boolean combineOthersBundlings, boolean off2On) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(productSkus != null, ItemServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    List<Item> resultItems = new ArrayList<>();
    Iterables.partition(productSkus, this.size).forEach(productSkuList -> {
      resultItems
          .addAll(this.itemCacheHelperService.getCacheableItemsByProductSkus(storeId, new HashSet(productSkuList)));
    });
    Set<String> cacheableProductSkus = resultItems.stream().map(Item::getProductSku).collect(Collectors.toSet());
    productSkus.removeAll(cacheableProductSkus);
    List<Item> items;
    if (off2On) {
      List<Item> itemsWithOff2OnChannelInactive =
          resultItems.stream().filter(item -> !item.isOff2OnChannelActive()).collect(toList());
      resultItems.removeAll(itemsWithOff2OnChannelInactive);
      if (CollectionUtils.isNotEmpty(productSkus)) {
        items = this.itemRepository
            .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalseAndOff2OnChannelActiveTrue(storeId,
                productSkus);
        resultItems.addAll(items);
      }
    } else {
      if (CollectionUtils.isNotEmpty(productSkus)) {
        items = this.itemRepository
            .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, productSkus);
        resultItems.addAll(items);
      }
    }
    if (combineOthersBundlings) {
      resultItems.forEach(item -> {
        this.itemCacheHelperService.setActivePromoBundlingsByPristineOrItemCode(storeId, item);
      });
    }
    return this.getItemsWithDiscount(storeId, username, requestId, resultItems, false);
  }

  @Override
  public EditItemResponse toggleArchiveItem(String storeId, String itemSku, String username, boolean doArchive)
      throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemSku), ItemServiceImpl.ITEM_SKU_MUST_NOT_BE_BLANK);
    Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSku(storeId, itemSku);
    checkState(item != null, ItemServiceImpl.ITEM_NOT_FOUND + itemSku);
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.findByStoreIdAndItemSku(storeId, item.getItemSku());
    if (CollectionUtils.isEmpty(itemPickupPointList)) {
      editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
      return editItemResponse;
    }
    editItemResponse = doArchivalAction(item, storeId, username, doArchive, itemPickupPointList, false);
    if (Objects.isNull(editItemResponse.getApiErrorCode())) {
      Product product = this.productService.getProduct(storeId, item.getProductSku());
      if (!doArchive) {
        item.setOff2OnChannelActive(product.isOff2OnChannelActive());
      }
      Item updatedItem = this.saveOperationService.saveItemWithoutUpdatingSolr(item,
          itemPickupPointList.stream().filter(ItemPickupPoint::isDelivery).collect(Collectors.toList()), false, StringUtils.EMPTY, Collections.EMPTY_MAP);
      updateCncAndArchiveAtL3(storeId, updatedItem, product);
      editItemResponse.setIsArchived(updatedItem.isArchived());
      editItemResponse.setUpdatedItems(Arrays.asList(updatedItem));
      editItemResponse.setUpdatedItemPickupPoints(itemPickupPointList);
      productAndItemSolrIndexerService.updateProductDetailsInSolr(Arrays.asList(product));
    } else {
      editItemResponse.setApiErrorCode(ApiErrorCode.TOGGLE_ARCHIVE_FAILED_FOR_SAME_FLAG);
      editItemResponse.setIsArchived(item.isArchived());
    }
    return editItemResponse;
  }

  private Item doArchivalFalseAction(Item item, String storeId) {
    item.setArchived(false);
    item.setMarkForDelete(false);
    item.setUpdatedDate(Calendar.getInstance().getTime());
    item.setUpdatedBy(username);
    List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
    itemChangeEventTypes.add(ItemChangeEventType.ARCHIVED_FLAG_CHANGE);
    item.setItemChangeEventTypes(itemChangeEventTypes);
    return this.saveOperationService.saveItemsWithoutUpdatingSolr(Collections.singletonList(item))
      .get(0);
  }

  private void updateCncAndArchiveAtL3(String storeId, Item updatedItem, Product product) {
    boolean cncActive = false;
    boolean archive = true;
    List<Item> items = itemRepository.findItemsByStoreIdAndProductSku(storeId, product.getProductSku());
    for (Item item : items) {
      if (item.getItemSku().equals(updatedItem.getItemSku())) {
        item = updatedItem;
      }
      if (item.isCncActivated()) {
        cncActive = true;
      }
      if (!item.isArchived()) {
        archive = false;
      }
    }
    product.setArchived(archive);
    product.setCncActivated(cncActive);
    if (archive) {
      product.setOnline(false);
    }
    saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
  }

  @Override
  public EditItemResponse doArchivalAction(Item item, String storeId, String username, boolean doArchive,
      List<ItemPickupPoint> itemPickupPointList, boolean isRejected) {
    EditItemResponse editItemResponse = new EditItemResponse();
    if (doArchive != item.isArchived()) {
      item.setArchived(doArchive);
      if (doArchive) {
        item.setOff2OnChannelActive(false);
        item.setCncActivated(false);
        if (Objects.nonNull(item.getMasterDataItem())) {
          item.getMasterDataItem().setViewable(false);
        }
      }
    } else {
      editItemResponse.setApiErrorCode((doArchive) ?
        ApiErrorCode.TOGGLE_ARCHIVE_FAILED_FOR_SAME_FLAG :
        ApiErrorCode.TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG);
      return editItemResponse;
    }
    item.setUpdatedDate(Calendar.getInstance().getTime());
    item.setUpdatedBy(username);
    List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    itemChangeEventTypes.add(ItemChangeEventType.ARCHIVED_FLAG_CHANGE);
    item.setItemChangeEventTypes(itemChangeEventTypes);
    this.itemPickupPointService.updateItemViewConfigByItemSku(auditTrailDtoList, storeId, username,
        doArchive, item, itemPickupPointList, isRejected);
    this.saveAndPublishService.publishMerchantVoucherViewConfigChange(itemPickupPointList, Arrays.asList(item));
    itemPickupPointSummaryService.updateExternalHistoryInPBP(auditTrailDtoList);
    editItemResponse.setIsArchived(item.isArchived());
    return editItemResponse;
  }

  @Override
  public EditItemResponse toggleArchiveByProductSku(String storeId, String username, String productSku,
    boolean doArchive, String source) throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ItemServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    List<Item> items = this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    editItemResponse.setCncActivated(items.get(0).isCncActivated());
    List<ItemPickupPoint> itemPickupPointList =
      itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(storeId,
        productSku);
    List<Item> updatedItemList = new ArrayList<>();
    checkState(CollectionUtils.isNotEmpty(items), ItemServiceImpl.PRODUCT_NOT_FOUND + productSku);
    if (CollectionUtils.isEmpty(itemPickupPointList) && !doArchive) {
      editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
      return editItemResponse;
    }
    Map<String, List<ItemPickupPoint>> itemPickupPointMap =
        itemPickupPointList.stream().collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
    for (Item item : items) {
      editItemResponse = doArchivalAction(item, storeId, username, doArchive, itemPickupPointList, false);
      if (Objects.isNull(editItemResponse.getApiErrorCode())) {
        Item updatedItem = this.saveOperationService.saveItemWithoutUpdatingSolr(item,
            itemPickupPointMap.get(item.getItemSku()), false, source, Collections.EMPTY_MAP);
        if (updatedItem.isCncActivated()) {
          editItemResponse.setCncActivated(true);
        }
        updatedItemList.add(updatedItem);
      }
    }
    editItemResponse.setUpdatedItems(updatedItemList);
    editItemResponse.setUpdatedItemPickupPoints(itemPickupPointList);
    return editItemResponse;
  }

  @Override
  public Page<Item> getItemsByProductSkuPaginated(String storeId, String productSku, int page, int size) {
    Page<Item> items =
      itemRepository.findItemsByStoreIdAndProductSkuIn(storeId, ImmutableSet.of(productSku),
        PageRequest.of(page, size));
    checkArgument(CollectionUtils.isNotEmpty(items.getContent()),
        ItemServiceImpl.ITEM_NOT_FOUND_FOR_PRODUCT_WITH_PRODUCT_SKU);
    return items;
  }

  @Override
  public void updateDangerousGoodsLevel(String storeId, Map<Integer, Set<String>> itemSku)
      throws Exception {
    for (Entry<Integer, Set<String>> entry : itemSku.entrySet()) {
      updateDangerousGoodsLevel(storeId, entry.getValue(), entry.getKey());
    }
  }

  @Override
  public void updateDangerousGoodsLevel(String storeId, Set<String> itemSkus,
      Integer dangerousLevel) throws Exception {
    Set<String> oldItem = null;
    try {
      List<ProductAndItemSolr> searchByItemSku =
          productSearchService.getItemSkuAndCodeByStoreIdAndItemSkus(storeId, itemSkus);
      Map<Boolean, Set<String>> mapOfOldAndNewProduct = searchByItemSku.stream().collect(Collectors
          .groupingBy(e -> e.getItemCode() == null,
              Collectors.mapping(ProductAndItemSolr::getItemSku, Collectors.toSet())));
      oldItem = mapOfOldAndNewProduct.get(true);
      LOG.warn("#updateDangerousGoodsLevel not updating dangerous goods on new item = {}",
          mapOfOldAndNewProduct.get(false));
      saveOperationService.updateItemDGLevel(storeId, oldItem, dangerousLevel);
    } catch (Exception e) {
      LOG.warn(
          "#updateDangerousGoodsLevel failed update dangerous goods on item = {} with error = {}",
          oldItem, e.getMessage(), e);
    }
  }

  @Override
  public boolean updateEtdNote(String storeId, String itemSku, String etdNote) {
    return false;
  }

  @Override
  public Item updateItem(String storeId, Item item, String username, boolean isOnlyExternal,
      boolean isProductTypeChanged, boolean isPreOrderChanged) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(item != null, ItemServiceImpl.ITEM_MUST_NOT_BE_NULL);
    Set<ItemChangeEventType> itemChangeEventTypes = new LinkedHashSet<>();
    boolean isShippingDetailChange = false;
    boolean isPickupPointChange = false;
    Map<String, Boolean> pickupPointToPureCNCStatusChangeMap = new HashMap<>();
    Item savedItem = this.itemRepository
        .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, item.getItemSku(), false);
    ItemPickupPoint itemPickupPoint = this.itemPickupPointService.findByItemSkuAndDelivery(storeId, item.getItemSku());
    checkState(savedItem != null, ItemServiceImpl.ITEM_NOT_FOUND);
    checkState(!savedItem.isForceReview(), CommonUtil.ITEM_NOT_EDITABLE);
    if(Objects.nonNull(itemPickupPoint)) {
      this.objectConverterService.overrideL4DetailsFromL5(Arrays.asList(savedItem), Arrays.asList(itemPickupPoint));
    }
    String existingMerchantSku = savedItem.getMerchantSku();
    Product product = this.productService.getProduct(storeId, savedItem.getProductSku());
    checkState(Objects.nonNull(product), ItemServiceImpl.PRODUCT_NOT_FOUND);
    boolean isItemDataChange = false;
    boolean viewConfigChange = false;
    boolean isItemChanged = false;
    boolean isDiscoverableChange = false;
    boolean isCncDiscoverableChange = false;
    String oldPickupPointCode = StringUtils.EMPTY;
    Map<String, ItemPickupPoint> evictItemPickupPointCacheMap = new HashMap<>();
    if (item.isFreeSample()) {
      if (cncForWarehouseFeatureSwitch) {
        ItemViewConfig cncItemViewConfig = new ItemViewConfig();
        cncItemViewConfig.setChannel(Constants.CNC);
        cncItemViewConfig.setDiscoverable(false);
        cncItemViewConfig.setBuyable(false);
        isCncDiscoverableChange = true;
        item = this.productHelperService.updateItemViewConfigForExistingChannel(item, cncItemViewConfig);
      }
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(false);
      isDiscoverableChange = true;
      item = this.productHelperService.updateItemViewConfigForExistingChannel(item, itemViewConfig);
    }
    if(Objects.nonNull(itemPickupPoint)) {
    if(itemViewConfigService.isItemViewConfigChangeForExistingChannelChange(savedItem, item.getItemViewConfigs())) {
      if (!cncForWarehouseFeatureSwitch) {
        pickupPointToPureCNCStatusChangeMap.put(item.getPickupPointCode(),
            CommonUtil.isPureCNCStatusChange(item.getItemViewConfigs().iterator().next(),
                savedItem.getItemViewConfigs().iterator().next(), itemPickupPoint.isCncActive(),
                itemPickupPoint.isCncActive()));
      }
      isDiscoverableChange =
          CommonUtil.isDiscoverableChanged(item.getSingleItemViewConfigByChannel(Constants.DEFAULT),
              savedItem.getSingleItemViewConfigByChannel(Constants.DEFAULT));

      if (cncForWarehouseFeatureSwitch) {
        isCncDiscoverableChange =
            CommonUtil.isDiscoverableChanged(item.getSingleItemViewConfigByChannel(Constants.CNC),
                savedItem.getSingleItemViewConfigByChannel(Constants.CNC));
      }

      this.productHelperService
          .updateItemViewConfigForExistingChannel(savedItem, item.getItemViewConfigs());
      isItemDataChange = true;
      viewConfigChange = true;
    }
    }
    if((Objects.nonNull(savedItem.getIsLateFulfillment())
        && !savedItem.getIsLateFulfillment().equals(item.getIsLateFulfillment()))) {
      savedItem.setLateFulfillment(item.isLateFulfillment());
      isItemDataChange = true;
    }
    if(!StringUtils.equals(savedItem.getMerchantSku(), item.getMerchantSku())) {
      savedItem.setMerchantSku(item.getMerchantSku());
      isItemDataChange = true;
    }
    if (isItemDataChange) {
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
    }
    MasterDataItem masterDataItem = item.getMasterDataItem();
    SystemParameter systemParameter = new SystemParameter();
    if (!savedItem.isSynchronized() && masterDataItem != null) {
      try {
        systemParameter = this.systemParameterService
            .findValueByStoreIdAndVariable(product.getStoreId(), Constants.CATEGORY_CODE_VARIABLE);
      } catch (ApplicationRuntimeException e) {
        systemParameter.setValue(StringUtils.EMPTY);
      }
      boolean disableUnSyncUpdate = false;
      if (Objects.nonNull(product.getMasterDataProduct())) {
        if (Objects.nonNull(product.getMasterDataProduct().getMasterCatalog())) {
          if (Objects.nonNull(product.getMasterDataProduct().getMasterCatalog().getCategory())) {
            disableUnSyncUpdate = CommonUtil.checkForDisableUnSyncUpdate(systemParameter.getValue(),
                product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode());
          }
        }
      }
      isShippingDetailChange =
           isShippingDetailChange(savedItem.getMasterDataItem(), masterDataItem, disableUnSyncUpdate, product,
              isOnlyExternal);
      setUnsyncMasterData(masterDataItem, savedItem.getMasterDataItem(), isShippingDetailChange, product);
      isItemChanged = true;
    }
    if (Objects.nonNull(item.getWholesalePriceActivated())) {
      savedItem.setWholesalePriceExists(true);
      if (Boolean.TRUE.equals(item.getWholesalePriceActivated())) {
        if (CollectionUtils.isEmpty(savedItem.getActivePromoBundlings())) {
          Set<String> activePromoBundlingSet = new HashSet();
          activePromoBundlingSet.add(Constants.WHOLESALE_PRICE);
          savedItem.setActivePromoBundlings(activePromoBundlingSet);
          isItemChanged = true;
        } else if (!savedItem.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE)) {
          savedItem.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
          isItemChanged = true;
        }
      } else {
        if (Optional.ofNullable(savedItem.getActivePromoBundlings()).orElse(new HashSet<>())
            .contains(Constants.WHOLESALE_PRICE))
          savedItem.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
          isItemChanged = true;
      }
    }
    if(Objects.nonNull(itemPickupPoint)) {
    if (StringUtils.isNotEmpty(savedItem.getPickupPointCode())
        && !savedItem.getPickupPointCode().equals(item.getPickupPointCode())) {
      oldPickupPointCode = savedItem.getPickupPointCode();
      savedItem.setPickupPointCode(item.getPickupPointCode());
      evictItemPickupPointCacheMap =
          updatePickupPointInItemPickupPoints(item.getPickupPointCode(), itemPickupPoint, savedItem,
              pickupPointToPureCNCStatusChangeMap, false, null, false);
      isPickupPointChange = true;
    }
    }
    if(isShippingDetailChange || isPickupPointChange) {
      itemChangeEventTypes.add(ItemChangeEventType.SHIPPING_CHANGE);
    }
    if (isPreOrderChanged) {
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
    }
    boolean isPriceChange = validatePriceChangeAndSet(savedItem, item.getPrice(), username);
    if(isPriceChange){
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
      itemPriceService.publishItemPriceChangeEvent(username, StringUtils.EMPTY, product, savedItem);
    }
    isItemChanged = isItemChanged || isItemDataChange || isShippingDetailChange || isPriceChange || isPickupPointChange || isPreOrderChanged;
    if(item.isOff2OnChannelActive() != savedItem.isOff2OnChannelActive()) {
      savedItem.setOff2OnChannelActive(item.isOff2OnChannelActive());
      itemChangeEventTypes.add(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE);
      isItemChanged = true;
      if(item.isOff2OnChannelActive()) {
        saveOperationService.updateAndEvictOff2OnItemCountByProductSku(storeId, item.getProductSku(), true, 1);
      } else {
        saveOperationService.updateAndEvictOff2OnItemCountByProductSku(storeId, item.getProductSku(), false, -1);
      }
    }
    boolean contentChanged = false;
    if (savedItem.isContentChanged() != item.isContentChanged()) {
      contentChanged = true;
      isItemChanged = true;
    }
    savedItem.setContentChanged(item.isContentChanged());
    if(item.isContentChanged()) {
      if (Objects.nonNull(savedItem.getPristineDataItem())) {
        itemChangeEventTypes.add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
        savedItem.setPristineDataItem(null);
      }
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
      isItemChanged = true;
    }
    savedItem.setItemChangeEventTypes(new ArrayList<>(itemChangeEventTypes));
    Item updatedItem;
    savedItem.setUpdatedFields(Set.of(UpdatedFields.NAME_UPDATE.name(), UpdatedFields.DESCRIPTION_UPDATE.name()));
    if(isItemChanged) {
      if (MapUtils.isEmpty(evictItemPickupPointCacheMap)) {
        if(Objects.nonNull(itemPickupPoint)) {
        evictItemPickupPointCacheMap.put(
            CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()),
            itemPickupPoint);
        }
      }
      List<ItemPickupPoint> result =
          evictItemPickupPointCacheMap.values().stream().filter(itemPickupPoint1 -> !itemPickupPoint1.isMarkForDelete())
              .collect(Collectors.toList());
      this.objectConverterService.overrideL5DetailsFromL4(Collections.singletonList(savedItem), result);
      List<ItemPickupPoint> updatedItemPickupPoints =
          itemPickupPointService.saveItemPickupPoint(new ArrayList<>(evictItemPickupPointCacheMap.values()));
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();
      itemPickupPointDataChangeEventModelList.addAll(updatedItemPickupPoints.stream().map(
              itemPickupPoint1 -> objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint1,
                  pickupPointToPureCNCStatusChangeMap.get(itemPickupPoint1.getPickupPointCode())))
          .collect(Collectors.toList()));
      setItemPickupPointChangeEventType(isPriceChange, result, itemPickupPointDataChangeEventModelList, isDiscoverableChange, isCncDiscoverableChange);
      updatedItem = this.saveOperationService.saveItem(savedItem, updatedItemPickupPoints,
          itemPickupPointDataChangeEventModelList);
      if (isProductTypeChanged || isShippingDetailChange || contentChanged || isPreOrderChanged) {
        updateOtherItemsForL3Product(product, savedItem, isProductTypeChanged, isShippingDetailChange, isPreOrderChanged);
      }
    } else {
      updatedItem = savedItem;
    }
    if(Objects.nonNull(itemPickupPoint)) {
      updateItemPickupPointIfMerchantSkuChanged(storeId, existingMerchantSku, savedItem, itemPickupPoint);
    }
    if(viewConfigChange) {
      saveAndPublishService.publishMerchantVoucherViewConfigChange(Arrays.asList(itemPickupPoint), Arrays.asList(savedItem));
    }
    if (isPickupPointChange) {
      if (CollectionUtils.isEmpty(product.getPickupPointCodes())) {
        product.setPickupPointCodes(new HashSet<>());
      }
      product.getPickupPointCodes().add(updatedItem.getPickupPointCode());
      List<ItemPickupPoint> itemPickupPointList =
        this.itemPickupPointService.findItemPickupPointsByProductSkuAndPPcode(storeId,
          savedItem.getProductSku(), oldPickupPointCode);
      itemPickupPointList.removeIf(
        itemPickupPoint1 -> savedItem.getItemSku().equals(itemPickupPoint1.getItemSku()));
      if (CollectionUtils.isEmpty(itemPickupPointList)) {
        product.getPickupPointCodes().remove(oldPickupPointCode);
      }
      this.saveOperationService.saveProduct(product);
    }
    item.setLateFulfillment(
        CommonUtil.getLateFulfillmentFromProductType(product.getProductType(), overrideLateFulfillmentByProductType,
            item.isLateFulfillment()));
    if (updatedItem.isSynchronized() && !ProductType.BOPIS.equals(product.getProductType())) {
      return updateSyncItemMasterData(updatedItem, product.getProductCode());
    }
    return updatedItem;
  }

  private static void setItemPickupPointChangeEventType(boolean isPriceChange, List<ItemPickupPoint> result,
      List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList, boolean isDiscoverableChange, boolean isCncDiscoverableChange) {
    if (isPriceChange || isDiscoverableChange || isCncDiscoverableChange) {
      for (ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel : itemPickupPointDataChangeEventModelList) {
        if (itemPickupPointDataChangeEventModel.getPickupPointCode()
            .equals(result.stream().findFirst().get().getPickupPointCode())) {
          List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes =
              itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes();
          if (isPriceChange) {
            itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.PRICE_CHANGE);
          }
          if (isDiscoverableChange) {
            itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
          }
          if (isCncDiscoverableChange) {
            itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
          }
          itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
        }
      }
    }
  }

  private void updateOtherItemsForL3Product(Product product, Item item,
      boolean isProductTypeChanged, boolean isShippingDetailChange, boolean isPreOrderChanged) {
    List<Item> remainingItems = null;
    List<Item> items;
    items = cacheItemHelperService
        .findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, product.getProductSku());
    if (CollectionUtils.isNotEmpty(items)) {
      remainingItems = items.stream().filter(item1 -> !item.getItemSku().equals(item1.getItemSku())).collect(toList());
    }
    if (CollectionUtils.isNotEmpty(remainingItems)) {
      remainingItems.stream().forEach(
          item1 -> updateItemAndPublishItemChange(item1, item, isProductTypeChanged,
              isShippingDetailChange, isPreOrderChanged));
    }
  }

  private void updateItemAndPublishItemChange(Item item, Item savedItem,
      boolean isProductTypeChanged, boolean isShippingDetailChange, boolean isPreOrderChanged) {
    Set<ItemChangeEventType> itemChangeEventTypes = new LinkedHashSet<>();
    if (isProductTypeChanged) {
      item.setLateFulfillment(savedItem.isLateFulfillment());
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
      item.setLateFullfillmentUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
      item.setLateFullfillmentUpdatedDate(new Date());
      log.info("IsLateFulfillment changed for itemSku because of product type changed: {}", item.getItemSku());
    }
    if (isShippingDetailChange) {
      if (Objects.nonNull(item.getMasterDataItem())) {
        setMasterDataItemShippingValues(item.getMasterDataItem(), savedItem.getMasterDataItem());
        itemChangeEventTypes.add(ItemChangeEventType.SHIPPING_CHANGE);
        log.info(
            "Shipping dimensions changed for itemSku {} because of change of shipping dimensions for item: {}",
            item.getItemSku(), savedItem.getItemSku());
      }
    }
    if (isPreOrderChanged) {
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
    }
    item.setContentChanged(savedItem.isContentChanged());
    if (savedItem.isContentChanged()) {
      if (Objects.nonNull(item.getPristineDataItem())) {
        item.setPristineDataItem(null);
        itemChangeEventTypes.add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
        log.info("Removing the pristine mapping for itemSku {} because of content change for item: {}",
            item.getItemSku(), savedItem.getItemSku());
      }
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
    }
    item.setItemChangeEventTypes(new ArrayList<>(itemChangeEventTypes));
    item.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
    item.setUpdatedDate(new Date());
    this.saveOperationService.saveItem(item, null, new ArrayList<>());
  }

  private void setMasterDataItemShippingValues(MasterDataItem updatedMasterDataItem,
      MasterDataItem savedMasterDataItem) {
    if (Objects.nonNull(savedMasterDataItem)) {
      updatedMasterDataItem.setItemLength(savedMasterDataItem.getItemLength());
      updatedMasterDataItem.setItemHeight(savedMasterDataItem.getItemHeight());
      updatedMasterDataItem.setItemWidth(savedMasterDataItem.getItemWidth());
      updatedMasterDataItem.setItemWeight(savedMasterDataItem.getItemWeight());
      updatedMasterDataItem.setItemDeliveryWeight(savedMasterDataItem.getItemDeliveryWeight());
    }
  }

  private Item updateSyncItemMasterData(Item updatedItem, String productCode) throws Exception {
    MasterDataProductAndItemsVO masterDataProductAndItemsVO = masterDataCacheService
        .getMasterDataProductAndItems(requestId, username, productCode, false);
    setMasterDataProductForSyncItem(updatedItem, masterDataProductAndItemsVO.getMasterDataProduct(),
        masterDataProductAndItemsVO.getMasterDataItems());
    return updatedItem;
  }

  private void setMasterDataProductForSyncItem(Item updatedItem, MasterDataProduct masterDataProduct,
      Map<String, MasterDataItem> masterDataItemMap) {
    MasterDataItem updatedMasterDataItem = new MasterDataItem();
    updatedMasterDataItem.setItemLength(masterDataProduct.getLength());
    updatedMasterDataItem.setItemHeight(masterDataProduct.getHeight());
    updatedMasterDataItem.setItemWidth(masterDataProduct.getWidth());
    updatedMasterDataItem.setItemWeight(masterDataProduct.getWeight());
    updatedMasterDataItem.setItemDeliveryWeight(masterDataProduct.getShippingWeight());
    if (Objects.nonNull(updatedItem.getItemCode()) && Objects.nonNull(masterDataItemMap)) {
      updatedMasterDataItem.setDangerousLevel(masterDataItemMap.get(updatedItem.getItemCode()).getDangerousLevel());
    }
    updatedItem.setMasterDataItem(updatedMasterDataItem);
  }

  @Override
  public boolean validatePriceChangeAndSet(Item itemToSet, Set<Price> updatedPrices, String username) {
    boolean isPriceChanged = false;
    if (CollectionUtils.isNotEmpty(updatedPrices)) {
      isPriceChanged = CommonUtil.isItemPriceChange(itemToSet, updatedPrices);
      if(isPriceChanged) {
        if(isPriceEditDisabled(itemToSet)) {
          LOG.error("Price change disabled for itemSku: {}", itemToSet.getItemSku());
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ErrorMessages.PRICE_CANNOT_BE_EDITED_FOR_ITEM);
        }
        Item returnedItem = this.itemHelperService.setItemPriceByChannel(itemToSet, updatedPrices, username);
        itemToSet.setPrice(returnedItem.getPrice());
        LOG.info("Price changed for itemSku: {}", itemToSet.getItemSku());
      }
    }
    return isPriceChanged;
  }

  private void setUnsyncMasterData(MasterDataItem masterDataItem, MasterDataItem savedMasterDataItem,
      boolean isShippingDetailChange, Product currentProduct) {
    if (isShippingDetailChange) {
      savedMasterDataItem.setInventoryType(masterDataItem.getInventoryType());
      savedMasterDataItem.setItemHeightIfNotNullOrZero(masterDataItem.getItemHeight());
      savedMasterDataItem.setItemLengthIfNotNullOrZero(masterDataItem.getItemLength());
      savedMasterDataItem.setItemWeightIfNotNullOrZero(masterDataItem.getItemWeight());
      savedMasterDataItem.setItemWidthIfNotNullOrZero(masterDataItem.getItemWidth());
      setItemDeliveryWeight(masterDataItem, savedMasterDataItem, currentProduct);
    }
    savedMasterDataItem.setActivated(masterDataItem.isActivated());
    savedMasterDataItem.setViewable(masterDataItem.isViewable());
    savedMasterDataItem.setMasterDataItemImages(masterDataItem.getMasterDataItemImages());
  }

  private void setItemDeliveryWeight(MasterDataItem masterDataItem, MasterDataItem savedMasterDataItem,
      Product currentProduct) {
    if (generateShippingWeight && Objects.nonNull(currentProduct.getMasterDataProduct()) && Objects
        .nonNull(currentProduct.getMasterDataProduct().getMasterCatalog()) && Objects
        .nonNull(currentProduct.getMasterDataProduct().getMasterCatalog().getCategory())) {
        log.info("Call to generate shipping weight for SKU : {} ", currentProduct.getProductSku());
      double shippingWeight = productService.generateShippingWeight(Constants.DEFAULT_STORE_ID,
          currentProduct.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode(),
          masterDataItem.getItemLength(), masterDataItem.getItemWidth(),
          masterDataItem.getItemHeight(), masterDataItem.getItemWeight());
        savedMasterDataItem.setItemDeliveryWeightIfNotNullOrZero(shippingWeight);
      if (ProductType.REGULAR.equals(currentProduct.getProductType()) && shippingWeight > maxShippingWeight) {
        log.error("cannot update regular product because weight > 50! for gdnSku : {} ",
            currentProduct.getProductSku());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, CommonConstants.WEIGHT_EXCEED_ERROR);
      }
      } else {
        savedMasterDataItem.setItemDeliveryWeightIfNotNullOrZero(masterDataItem.getItemDeliveryWeight());
    }
  }

  private boolean isShippingDetailChange(MasterDataItem savedMasterDataItem, MasterDataItem newMasterDataItem,
      boolean disableUnSyncUpdate, Product currentProduct, boolean isOnlyExternal) {
    boolean isShippingDetailChange =
        ((Objects.nonNull(newMasterDataItem.getInventoryType()) && changeInItemDeliveryValue(
            savedMasterDataItem.getInventoryType(), newMasterDataItem.getInventoryType(), currentProduct.getProductType())) || (
            Objects.nonNull(newMasterDataItem.getItemHeight()) && changeInItemDeliveryValue(
                savedMasterDataItem.getItemHeight(), newMasterDataItem.getItemHeight(), currentProduct.getProductType())) || (
            Objects.nonNull(newMasterDataItem.getItemWeight()) && changeInItemDeliveryValue(
                savedMasterDataItem.getItemWeight(), newMasterDataItem.getItemWeight(), currentProduct.getProductType())) || (
            Objects.nonNull(newMasterDataItem.getItemLength()) && changeInItemDeliveryValue(
                savedMasterDataItem.getItemLength(), newMasterDataItem.getItemLength(), currentProduct.getProductType())) || (
            Objects.nonNull(newMasterDataItem.getItemWidth()) && changeInItemDeliveryValue(
                savedMasterDataItem.getItemWidth(), newMasterDataItem.getItemWidth(), currentProduct.getProductType())));
    if(isShippingDetailChange && disableUnSyncUpdate && isOnlyExternal) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.DISABLED_MASTER_DATA_UPDATE);
    }
    boolean isCurrentValueNull = currentValueIsNull(savedMasterDataItem, currentProduct.getProductType());
    if(isCurrentValueNull && disableUnSyncUpdate && isOnlyExternal) {
      setMasterDataItemShippingValues(newMasterDataItem, currentProduct);
    }
    return (isShippingDetailChange || isCurrentValueNull);
  }

  private void setMasterDataItemShippingValues(MasterDataItem updatedMasterDataItem, Product currentProduct) {
    if (Objects.nonNull(currentProduct.getMasterDataProduct())) {
      updatedMasterDataItem.setItemLength(currentProduct.getMasterDataProduct().getLength());
      updatedMasterDataItem.setItemHeight(currentProduct.getMasterDataProduct().getHeight());
      updatedMasterDataItem.setItemWidth(currentProduct.getMasterDataProduct().getWidth());
      updatedMasterDataItem.setItemWeight(currentProduct.getMasterDataProduct().getWeight());
      updatedMasterDataItem.setItemDeliveryWeight(currentProduct.getMasterDataProduct().getShippingWeight());
    }
  }

  private boolean currentValueIsNull(MasterDataItem currentMasterDataItem, ProductType productType) {
    if (!ProductType.BOPIS.equals(productType) && Objects.nonNull(currentMasterDataItem) && (
        Objects.isNull(currentMasterDataItem.getItemDeliveryWeight()) || Objects
            .isNull(currentMasterDataItem.getItemLength()) || Objects.isNull(currentMasterDataItem.getItemHeight())
            || Objects.isNull(currentMasterDataItem.getItemWidth()) || Objects
            .isNull(currentMasterDataItem.getItemWeight()))) {
      return true;
    }
    return false;
  }

  private <T> boolean changeInItemDeliveryValue(T savedItemValue, T newItemValue, ProductType currentProductType) {
    boolean changeInValue = false;
    if(!newItemValue.equals(savedItemValue)) {
      changeInValue = true;
    }
    if(Objects.isNull(savedItemValue) && !currentProductType.equals(ProductType.BOPIS)) {
      changeInValue = false;
    }
    return changeInValue;
  }

  @Override
  public boolean validatePriceForDeliveryTrueItemPickupPoint(String storeId, ItemPickupPoint itemPickupPoint,
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest) {
    String[] includedField =
        {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.CATEGORY_CODE};
    try {
      List<Product> productsByProductSkus =
          productService.getProductsByProductSkus(storeId, Collections.singleton(itemPickupPoint.getProductSku()),
              includedField, false);
      if (CollectionUtils.isNotEmpty(productsByProductSkus)) {
        String categoryCode = StringUtils.EMPTY;
        if (StringUtils.isNotEmpty(productsByProductSkus.get(0).getCategoryCode())) {
          categoryCode = productsByProductSkus.get(0).getCategoryCode();
        } else {
          ProductDetailResponse result = this.masterDataService.getProductDetailFromMasterData(username, requestId,
              productsByProductSkus.get(0).getProductCode());
          if (Objects.nonNull(result) && CollectionUtils.isNotEmpty(result.getProductCategoryResponses())) {
            categoryCode = result.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
          }
        }
        validatePromoPriceAndUpdateCampaign(updateOfflineItemPriceRequest, categoryCode, itemPickupPoint);
      } else {
        return false;
      }
    } catch (ApplicationRuntimeException ex){
      log.error("Exception caught from campaign while updating price for itemPickupPoint with itemSku = {} ",
          itemPickupPoint.getItemSku(), ex);
      throw ex;
    }
    catch (Exception e) {
      log.error("Exception caught while updating price for itemPickupPoint with itemSku = {} ",
          itemPickupPoint.getItemSku(), e);
      return false;
    }
    return true;
  }

  private void validatePromoPriceAndUpdateCampaign(UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest,
      String categoryCode, ItemPickupPoint itemPickupPoint) throws Exception {
    if (this.isPriceEditDisabled(itemPickupPoint)) {
      log.error("Price update disabled for this sku : {}", updateOfflineItemPriceRequest.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION);
    }
    UpdateDiscountDTO updateDiscountDTO =
        UpdateDiscountDTO.builder().itemSku(updateOfflineItemPriceRequest.getItemSku())
            .sellingPrice(updateOfflineItemPriceRequest.getOfferPrice()).categoryCode(categoryCode)
            .pickUpPointCode(itemPickupPoint.getPickupPointCode()).build();
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest =
        CampaignUpdateDiscountRequest.builder().discountDTOList(Collections.singletonList(updateDiscountDTO)).build();
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
        xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
    if (Objects.nonNull(campaignUpdateDiscountResponse) && Objects.nonNull(
        campaignUpdateDiscountResponse.getItemSkuStatusMap()) && campaignUpdateDiscountResponse.getItemSkuStatusMap()
        .containsKey(updateOfflineItemPriceRequest.getItemSku())) {
      log.error("Campaign price validation failed for itemSku : {} with Error : {} ",
          updateOfflineItemPriceRequest.getItemSku(),
          campaignUpdateDiscountResponse.getItemSkuStatusMap().get(updateOfflineItemPriceRequest.getItemSku()));
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, campaignUpdateDiscountResponse.getItemSkuStatusMap()
          .get(updateOfflineItemPriceRequest.getItemSku()));
    }
  }

  @Override
  public Map<String, Item> fetchItemMapByItemSkus(String storeId, Set<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    List<Item> itemList = getItemsByStoreIdAndItemSkus(storeId, itemSkus);
    return itemList.stream().collect(Collectors.toMap(Item::getItemSku, Function.identity()));
  }

  @Override
  public List<ItemPickupPoint> updateItemPickupPointIfMerchantSkuChanged(String storeId, String existingMerchantSku, Item newItem,
      ItemPickupPoint itemPickupPointWithDeliveryTrue) {
    if (!StringUtils.equals(existingMerchantSku, newItem.getMerchantSku())) {
      List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
      itemPickupPointWithDeliveryTrue.setMerchantSku(newItem.getMerchantSku());
      itemPickupPointWithDeliveryTrue.setCncActive(false);
      updatedItemPickupPointList.add(itemPickupPointWithDeliveryTrue);
      return updateItemPickupPointDeliveryFalse(storeId, newItem, updatedItemPickupPointList);
    }
    return new ArrayList<>();
  }

  @Override
  public List<ItemPickupPoint> updateItemPickupPointDeliveryFalse(String storeId, Item newItem,
      List<ItemPickupPoint> updatedItemPickupPointList) {
    List<ItemPickupPoint> itemPickupPointList = this.itemPickupPointService
        .findByStoreIdAndItemSkuAndCncActiveAndMarkForDelete(storeId, newItem.getItemSku(), Boolean.TRUE,
            Boolean.FALSE);
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      if (!itemPickupPoint.isDelivery()) {
        itemPickupPoint.setMarkForDelete(true);
        updatedItemPickupPointList.add(itemPickupPoint);
      }
    }
    return itemPickupPointService.saveItemPickupPoint(updatedItemPickupPointList);
  }

  @Override
  public boolean updateItemPrice(String username, String storeId, Price price, String itemSku,
      Boolean wholeSalePriceActivated) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isItemSku(itemSku),
        ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
    checkArgument(price != null, ItemServiceImpl.PRICE_MUST_NOT_BE_NULL);

    Item item =
        this.itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku, false);
    boolean isPriceChanged = false;
    checkState(item != null, ItemServiceImpl.ITEM_NOT_FOUND);
    checkState(!item.isForceReview(), CommonUtil.ITEM_NOT_EDITABLE);
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    checkState(itemPickupPoint != null, ItemServiceImpl.ITEM_PICKUPOINT_NOT_FOUND);
    objectConverterService
        .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    Set<Price> prices = new HashSet<>();
    if (Objects.nonNull(wholeSalePriceActivated)) {
      item.setWholesalePriceExists(true);
    }
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
      if ((itemPickupPoint.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE) && Boolean.TRUE.equals(
          wholeSalePriceActivated)) || (!itemPickupPoint.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE)
          && Boolean.FALSE.equals(wholeSalePriceActivated))) {
        wholeSalePriceActivated = null;
      }
    }
    prices.add(price);
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getPrice())) {
      isPriceChanged = validatePriceChangeAndSet(item, prices, username);
      CommonUtil.settingPriceUpdatedDate(item, itemPickupPoint, channelService.getDefaultChannel());
      itemPickupPoint.setPrice(item.getPrice());
    }
    if (Objects.nonNull(wholeSalePriceActivated)) {
      if (Boolean.TRUE.equals(wholeSalePriceActivated)) {
        if (CollectionUtils.isEmpty(itemPickupPoint.getActivePromoBundlings())) {
          Set<String> activePromoBundlingSet = new HashSet();
          activePromoBundlingSet.add(Constants.WHOLESALE_PRICE);
          item.setActivePromoBundlings(activePromoBundlingSet);
          itemPickupPoint.setActivePromoBundlings(activePromoBundlingSet);
        } else {
          item.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
          itemPickupPoint.getActivePromoBundlings().add(Constants.WHOLESALE_PRICE);
        }
      } else {
        if (Optional.ofNullable(itemPickupPoint.getActivePromoBundlings()).orElse(new HashSet<>())
            .contains(Constants.WHOLESALE_PRICE))
          itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      }
    }
    item.setContentChanged(false);
    List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
    itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
    item.setItemChangeEventTypes(itemChangeEventTypes);
    ItemPickupPoint updatedItemPickupPoint = this.itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterService.convertToItemPickupPointChangeEventModel(updatedItemPickupPoint, false);
    if (isPriceChanged) {
      List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes =
          itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes();
      itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.PRICE_CHANGE);
      itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
    }
    cacheEvictHelperService.evictItemPickupPointData(storeId, itemPickupPoint, itemPickupPoint.getPickupPointCode());
    cacheEvictHelperService.evictItemCache(storeId, item);
    this.productAndItemSolrIndexerService.updateSolrOnPriceAndWholesalePriceFlagChange(item, wholeSalePriceActivated);
    saveAndPublishService.publishListOfItems(Collections.singletonList(item),
        Collections.singletonList(updatedItemPickupPoint),
        Collections.singletonList(itemPickupPointDataChangeEventModel), false);
    return true;
  }

  @Override
  public boolean updateItemPriceByMerchantSkuAndMerchantCode(String storeId, String requestId,
      String username, String merchantSku, String merchantCode, Price newPrice) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode),
        ItemServiceImpl.MERCHANT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(newPrice != null, ItemServiceImpl.PRICE_MUST_NOT_BE_NULL);

    List<Item> items =
        this.getItemsByMerchantSkuAndMerchantCode(storeId, merchantSku, merchantCode);
    for (Item item : items) {
      for (Price price : item.getPrice()) {
        if (!item.isMerchantPromoDiscount() && newPrice.getChannel().equals(price.getChannel())) {
          this.priceHistoryService.savePriceHistory(
              this.objectConverterService.convertToPriceHistory(price, item.getItemSku()));
          BeanUtils.copyProperties(newPrice, price);
          price.setLastUpdatedBy(username);
          price.setLastUpdatedDate(new Date());
          break;
        }
      }
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.ITEM_PRICE_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
    }
    this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(items, null,StringUtils.EMPTY);
    this.productAndItemSolrIndexerService.updateSolrOnPriceChange(items);
    return true;
  }

  @Override
  public boolean updateSalesCatalogForPristineProducts(List<String> pristineIds) {
    try {
      if(CollectionUtils.isEmpty(pristineIds)) {
        pristineIds = pristineItemRepository.getAllPristineIds();
      }
      return updateSalesCatalogForAllPristineProducts(pristineIds);
    } catch (Exception e) {
      LOG.error("failed to update salesCatalog for pristine products", e);
      return false;
    }
  }

  @Override
  public boolean updateSalesCatalogForAllPristineProducts(List<String> pristineIds) {
    List<SalesCatalogUpdateForPristineTask> taskList = new ArrayList<>();
    List<List<String>> batchPristineIds = Lists.partition(pristineIds, size);
    for (List<String> batchList : batchPristineIds) {
      SalesCatalogUpdateForPristineTask task =
          new SalesCatalogUpdateForPristineTask(batchList, this, pristineItemRepository, storeId,
              this.cacheEvictHelperService, this.saveAndPublishService);
      taskList.add(task);
    }
    try {
      List<Future<Boolean>> results = this.executorService.invokeAll(taskList);
      for (Future<Boolean> result : results) {
        if (result.get() == null || !result.get()) {
          return false;
        }
      }
    } catch (Exception e) {
      LOG.error("failed to update salesCatalog for pristine products", e);
      return false;
    }
    return true;
  }

  @Override
  public List<Item> getItemsByPristineIds(String storeId, Set<String> pristineIds)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pristineIds),
        ItemServiceImpl.LIST_OF_PRISTINEIDS_MUST_NOT_BE_BLANK);
    List<PristineDataItem> pristineItems = pristineItemRepository.findByPristineIdIn(pristineIds);
    return this.itemRepository
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, pristineItems);
  }

  @Override
  public List<Item> getAllItemsByPristineIds(String storeId, Set<String> pristineIds)
      throws Exception {
    checkArgument(CollectionUtils.isNotEmpty(pristineIds), ItemServiceImpl.LIST_OF_PRISTINEIDS_MUST_NOT_BE_BLANK);
    List<PristineDataItem> pristineItems = pristineItemRepository.findByPristineIdIn(pristineIds);
    return this.itemRepository.findByStoreIdAndPristineDataItemIn(storeId, pristineItems);
  }

  @Override
  public List<Item> getItemSkusByPristineIdsAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(String storeId, Set<String> pristineIds)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pristineIds), ItemServiceImpl.LIST_OF_PRISTINEIDS_MUST_NOT_BE_BLANK);
    List<PristineDataItem> pristineItems = pristineItemRepository.findByPristineIdIn(pristineIds);
    return this.itemRepository.findItemSkusByStoreIdAndPristineDataItemInAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalse(
        storeId, pristineItems);
  }

  @Override
  public List<Item> getItemsByProductSkus(String storeId, Set<String> productSkus)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(productSkus),
        ItemServiceImpl.LIST_OF_PRODUCT_SKUS_MUST_NOT_BE_BLANK);
    return this.itemRepository
        .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, productSkus);
  }

  @Override
  public Map<String, String> getMapForPristineCategoryAttribute() {
    return attributeMap;
  }

  @Override
  public void publishAllItems(String storeId){
    LOG.info("Start publishAllItems");
    try(Stream<Item> itemStream = this.itemRepository.streamAllByStoreId(storeId)) {
      this.saveAndPublishService.publishStreamOfItems(itemStream);
      LOG.info("Finish publishAllItems");
    } catch (Exception e){
      LOG.error("Failed publishAllItems with error {} ", e);
    }
  }

  @Override
  public void republishItemsToAgp(String storeId, List<String> itemSkus){
    LOG.info("Start republishItemsToAgp");
    try(Stream<Item> itemStream = this.itemRepository.streamAllByStoreIdAndItemSkuIn(storeId,itemSkus)) {
      List<Item> itemList = itemStream.collect(Collectors.toCollection(ArrayList::new));
        saveAndPublishService.publishItemDataChangeEvent(itemList);
      LOG.info("Finish republishItemsToAgp");
    } catch (Exception e){
      LOG.error("Failed republishItemsToAgp with error {} ", e);
    }
  }

  @Override
  public void publishItemSkus(String storeId, Map<String, Boolean> itemsChanged,
      Set<String> skuCodeNeedToPublish, ProductDomainEventModel productDomainEventModel) {

    String id = productDomainEventModel.getProductCode();
    Boolean pristineCategory = productDomainEventModel.getPristineCategory();
    boolean migratedProduct = productDomainEventModel.isMigratedProduct();
    boolean isItemUpdatePublish = productDomainEventModel.isItemUpdatePublish();
    Set<String> productPublishEventTypes = productDomainEventModel.getEventTypes();

    log.info("Saving items using map : {} and pristine category flag : {} and id : {} and migratedProduct : {}",
        itemsChanged, pristineCategory, id, migratedProduct);

    if (migratedProduct) {
      publishEventsForMigration(storeId, id, itemsChanged);
      return;
    }
    List<Product> productList = new ArrayList<>();
    Set<String> productSkus = validateAndGetProductSkus(storeId, id, productList);

    if (CollectionUtils.isNotEmpty(productSkus)) {
      List<Item> items = this.itemRepository
          .findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, productSkus);
      if (isItemUpdatePublish || Objects.isNull(pristineCategory) || Boolean.FALSE.equals(pristineCategory)) {
        clearItemCacheAndSetItemChangeEventType(items);
        List<Item> publishItems = filterItemsForPublishingItemDataChangeEvent(skuCodeNeedToPublish, productPublishEventTypes, items);
        updateUpdatedFieldsInItem(publishItems, productDomainEventModel.getUpdatedFields());
        Product product = Optional.of(productList).orElse(Collections.singletonList(new Product())).stream()
            .filter(Product::isSynchronized).findFirst().orElse(null);

        if (skipL4EventPublishOnMasterDataChange) {
          boolean L4EventPublishSkip = checkIfL4EventPublishCanBeSkipped(productDomainEventModel, product);
          if (L4EventPublishSkip) {
            return;
          }
        }
        publishItemDataChangeEvent(productPublishEventTypes, publishItems);
      } else {
        updateItemContentChangeFlagBaseOnPristineCategory(itemsChanged, pristineCategory, items);
        updateItems(items);
      }
    }
  }

  private static boolean checkIfL4EventPublishCanBeSkipped(ProductDomainEventModel productDomainEventModel,
      Product product) {
    if (Objects.nonNull(product)) {
      String categoryCode = StringUtils.EMPTY;
      if (CollectionUtils.isNotEmpty(productDomainEventModel.getProductCategories()) && Objects.nonNull(
          productDomainEventModel.getProductCategories().get(0).getCategory())) {
        categoryCode = productDomainEventModel.getProductCategories().get(0).getCategory().getCategoryCode();
      }
      if (!Optional.ofNullable(productDomainEventModel.getBrand()).orElse(StringUtils.EMPTY).equals(product.getBrand())
          || !Optional.ofNullable(categoryCode).orElse(StringUtils.EMPTY).equals(product.getCategoryCode())
          || !Optional.ofNullable(productDomainEventModel.getName()).orElse(StringUtils.EMPTY)
          .equals(product.getProductName())) {
        log.info("Skip publishing L4 because there is masterData for productCode : {}",
            productDomainEventModel.getProductCode());
        return true;
      }
    }
    return false;
  }

  private void updateItems(List<Item> items) {
    if (masterDataChangeSolrReindexEnabled) {
      saveOperationService.saveItems(items, null);
    } else {
      saveOperationService.saveItemsWithoutUpdatingSolr(items);
    }
  }

  private void updateItemContentChangeFlagBaseOnPristineCategory(Map<String, Boolean> itemsChanged,
      Boolean pristineCategory, List<Item> items) {
    for (Item item : items) {
      boolean contentChanged = itemsChanged.get(item.getItemCode());
      if (pristineCategory && contentChanged) {
        item.setContentChanged(true);
        if (Objects.nonNull(item.getPristineDataItem())) {
          log.info("Removing pristine mapping on update for pristine mapped categories for item sku : {}",
              item.getItemSku());
          item.setPristineDataItem(null);
          List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
          itemChangeEventTypes.add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
          itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
          item.setItemChangeEventTypes(itemChangeEventTypes);
        }
      } else {
        item.setContentChanged(false);
      }
    }
  }

  private void publishItemDataChangeEvent(Set<String> productPublishEventTypes, List<Item> publishItems) {
    if (publishItemPickupPointChangeEventOnProductPublish && !Optional.ofNullable(productPublishEventTypes)
        .orElse(new HashSet<>()).contains(ProductPublishEventType.PUBLISH_ITEM_PICKUP_POINT_DATA_CHANGE_EVENT.name())) {
      saveAndPublishService.publishItemDataChangeEvent(publishItems, Constants.SOURCE_PRODUCT_PUBLISH, false);
    } else {
      saveAndPublishService.publishListOfItems(publishItems, new ArrayList<>(), new ArrayList<>(),
          Constants.SOURCE_PRODUCT_PUBLISH, false, Collections.EMPTY_MAP);
    }
  }

  private List<Item> filterItemsForPublishingItemDataChangeEvent(Set<String> skuCodeNeedToPublish,
      Set<String> productPublishEventTypes, List<Item> items) {
    List<Item> publishItems;
    if (publishSpecificItemDataDataChangeEvent && Optional.ofNullable(productPublishEventTypes).orElse(new HashSet<>())
        .contains(ProductPublishEventType.PUBLISH_SPECIFIC_ITEM_DATA_CHANGE_EVENT.name())) {
      publishItems = items.stream().filter(item -> skuCodeNeedToPublish.contains(item.getItemCode())).collect(toList());
    } else {
      publishItems = items;
    }
    return publishItems;
  }

  private void clearItemCacheAndSetItemChangeEventType(List<Item> items) {
    for (Item item : items) {
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      item.setItemChangeEventTypes(new ArrayList<>());
      item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_DATA_CHANGE);
    }
  }

  private Set<String> validateAndGetProductSkus(String storeId, String id, List<Product> productList) {
    Set<String> productSkus = new HashSet<>();
    if (skuValidator.isProductSku(id)) {
      productSkus.add(id);
    } else {
      List<Product> products = this.productService.getProductsByProductCode(storeId, id);
      if (CollectionUtils.isNotEmpty(products)) {
      productList.addAll(products);
        for (Product product : products) {
          productSkus.add(product.getProductSku());
        }
      }
    }
    return productSkus;
  }

  private void updateUpdatedFieldsInItem(List<Item> publishItems, Set<String> updatedFields) {
    Optional.ofNullable(publishItems).orElse(new ArrayList<>()).stream()
        .forEach(item -> item.setUpdatedFields(updatedFields));
  }

  private void publishEventsForMigration(String storeId, String id, Map<String, Boolean> itemsChanged) {
    List<Product> products = this.productService.findByStoreIdAndProductCode(storeId, id);
    Set<String> productSkus = new HashSet<>();
    if (CollectionUtils.isNotEmpty(products)) {
      for (Product product : products) {
        productSkus.add(product.getProductSku());
        product.setSynchronized(true);
        saveOperationService.saveProduct(product);
      }
    }

    if (CollectionUtils.isNotEmpty(productSkus)) {
      List<Item> items = this.itemRepository.findItemsByStoreIdAndProductSkuIn(storeId, productSkus);
      for (Item item : items) {
        boolean contentChanged = itemsChanged.get(item.getItemCode());
        item.setContentChanged(contentChanged);
        if (!item.isSynchronized()) {
          item.setSynchronized(true);
          List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
          itemChangeEventTypes.add(ItemChangeEventType.SYNC_UNSYNC_FLAG_CHANGE);
          itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
          item.setItemChangeEventTypes(itemChangeEventTypes);
        }
      }
      saveOperationService.saveItems(items, null);
    }
  }

  @Override
  public void archiveAndDeleteActiveProduct(ProductDomainEventModel productDomainEventModel,
    Map<String, Set<String>> eventToBlackListedSellersMap) {
    Set<String> productSkus = new HashSet<>();
    int pageNumber = 0;
    Page<Item> items;
    List<Product> products =
        this.productService.findByStoreIdAndProductCode(storeId, productDomainEventModel.getProductCode());
    List<Item> itemList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(products)) {
      CommonUtil.setSharedProduct(products);
      for (Product product : products) {
        productSkus.add(product.getProductSku());
        product.setMarkForDelete(true);
        product.setArchived(true);
        product.setForceReview(false);
        product.setSuspended(false);
        saveOperationService.saveProductWithoutUpdatingSolr(product,
            Collections.singletonList(ProductChangeEventType.PRODUCT_REJECTED), StringUtils.EMPTY, eventToBlackListedSellersMap);
      }
    }
    if (CollectionUtils.isNotEmpty(productSkus)) {
      do {
        items = this.itemRepository
            .findItemsByStoreIdAndProductSkuIn(storeId, productSkus, PageRequest.of(pageNumber++,DEFAULT_PAGE_SIZE));
        for (Item item : items) {
          checkState(Objects.nonNull(item), ItemServiceImpl.ITEM_NOT_FOUND + item.getItemSku());
          itemList.add(item);
          item.setForceReview(false);
          item.setMarkForDelete(true);
          List<ItemPickupPoint> itemPickupPointList =
              itemPickupPointService.findByStoreIdAndItemSku(item.getStoreId(), item.getItemSku());
          EditItemResponse editItemResponse =
              doArchivalAction(item, storeId, username, true, itemPickupPointList, true);
          itemPickupPointService.deleteItemPickupPoints(item.getStoreId(), item.getItemSku());
          this.saveOperationService.saveItemWithoutUpdatingSolr(item, new ArrayList<>(), true, Constants.SOURCE_PRODUCT_REJECTION, eventToBlackListedSellersMap);
        }
      } while ((pageNumber * DEFAULT_PAGE_SIZE) < items.getTotalElements());
      publishSolrUpdatedEvent(products, itemList);
    }
  }

  private void publishSolrUpdatedEvent(List<Product> products, List<Item> items) {
    if (CollectionUtils.isNotEmpty(products)) {
      if (CollectionUtils.isNotEmpty(items)) {
        List<ProductAndItemEventModel> productAndItemEventModels = new ArrayList<>();
        Map<String, List<Item>> itemGroup = items.stream().collect(Collectors.groupingBy(Item::getProductSku));
        for (Product product : products) {
          List<Item> productItems = itemGroup.get(product.getProductSku());
          if (CollectionUtils.isNotEmpty(productItems)) {
            productAndItemEventModels.add(
                objectConverterService.convertToProductAndItemEventModel(new ProductAndItemsVO(product, items)));
          }
        }
        if (isDeleteL3AndL4Enabled) {
          productAndItemEventModels.forEach(
            productAndItemEventModel -> productAndItemEventModel.setRejected(true));
        }
        saveAndPublishService.publishSolrUpdateEvent(productAndItemEventModels);
      }
    }
  }

  @Override
  public List<Item> findBuyableDiscoverableItemsByPristineId(String storeId, String pristineId) {
    List<Item> itemList = getItemsByPristineId(storeId, pristineId);
    return itemList.stream().filter(this::isItemBuyableAndDiscoverable)
        .collect(Collectors.toList());
  }

  @Override
  public List<Item> getItemsByPristineId(String storeId, String pristineId) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(pristineId),
        CommonConstants.PRISTINE_ID_MUST_NOT_BE_BLANK);
    PristineDataItem pristineItem = this.pristineItemRepository.findByPristineId(pristineId);
    if(Objects.isNull(pristineItem)){
      return new ArrayList<>();
    }
    return this.itemRepository
        .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse(storeId, pristineItem);
  }

  @Override
  public Boolean isItemBuyableAndDiscoverable(Item item) {
    if (Objects.nonNull(item)) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
      if (Objects.nonNull(itemPickupPoint)) {
        ItemViewConfig itemViewConfig =
            itemPickupPoint.getItemViewConfig().stream().findFirst().orElseGet(ItemViewConfig::new);
        return (CommonUtil.getBuyableFromConfig(itemViewConfig) && CommonUtil.getDiscoverableFromConfig(itemViewConfig));
      }
    }
    return Boolean.FALSE;
  }

  @Override
  public DefaultItemSkuVO findFirstBuyableDiscoverableItemSkuByPristineId(String storeId,
      LinkedHashSet<String> itemSkus, String pristineId) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), CommonConstants.ITEM_SKUS_MUST_NOT_BE_EMPTY);
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pristineId), CommonConstants.PRISTINE_ID_MUST_NOT_BE_BLANK);
    List<Item> itemList = itemRepository.getItemViewConfigsByItemSkus(storeId, itemSkus);
    Map<String, Item> itemMap = new HashMap<>();
    itemList.forEach(item -> itemMap.put(item.getItemSku(), item));
    return new DefaultItemSkuVO(getFirstBuyableDiscoverableItemSku(itemSkus, itemMap, pristineId));
  }

  /**
   * To get first buyable and discoverable item sku
   *
   * @param itemSkuSet sorted list of itemSkus as per buyBoxScores
   * @param itemMap    i.e. map of item sku with item response
   * @param pristineId i.e. pristine id to which the item should be mapped
   * @return i.e. first buyable and discoverable item sku, if null return empty string
   */
  private String getFirstBuyableDiscoverableItemSku(LinkedHashSet<String> itemSkuSet, Map<String, Item> itemMap,
      String pristineId) {
    return Optional.ofNullable(
        itemSkuSet.stream().map(itemMap::get).filter(item -> checkForBuyableDiscoverableAndPristineId(item, pristineId))
            .findFirst().orElse(new Item()).getItemSku()).orElse(StringUtils.EMPTY);
  }

  /**
   * To check whether item is buyable, discoverable and mapped to pristine id
   * @param item i.e. item to be checked
   * @param pristineId i.e. pristine id to which item should be mapped
   * @return i.e. whether the item is buyable, discoverable and mapped to pristine id or not
   */
  private Boolean checkForBuyableDiscoverableAndPristineId(Item item, String pristineId) {
    if (Objects.nonNull(item) && Objects.nonNull(item.getPristineDataItem())) {
      return pristineId.equals(item.getPristineDataItem().getPristineId()) && isItemBuyableAndDiscoverable(item);
    }
    return Boolean.FALSE;
  }

  @Override
  public List<Item> getItemsWithDiscountPriceByPristineMasterIds(String storeId, String username,
      String requestId, Set<String> pristineMasterIds) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(pristineMasterIds),
        ItemServiceImpl.PRISTINE_MASTER_IDS_SET_MUST_NOT_BE_EMPTY);
    List<PristineDataItem> pristineItemList =
        pristineItemRepository.findByPristineMasterIdIn(pristineMasterIds);
    List<Item> items = this.itemRepository
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, pristineItemList);
    return this.getItemsWithDiscount(storeId, username, requestId, items, false);

  }

  @Override
  public List<ItemCatalogVO> getItemCatalogsByPristineId(String pristineId) throws Exception {

    Set<String> productSkus = new HashSet();
    Set<String> categoryCodes = new HashSet<>();
    Set<String> pristineIdSet = new HashSet<>();
    pristineIdSet.add(pristineId);
    List<PristineDataItem> pristineItemList = pristineItemRepository.findByPristineIdIn(pristineIdSet);
    List<Item> currentItemsByPristineId = itemRepository
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, pristineItemList);
    if (CollectionUtils.isNotEmpty(currentItemsByPristineId)) {
      for (Item item : currentItemsByPristineId) {
        productSkus.add(item.getProductSku());
      }
      List<Product> productResultBySkus = this.productService.getProducts(storeId, productSkus);
      for (Product product : productResultBySkus) {
        for (SalesCatalog salesCatalog : product.getSalesCatalogs()) {
          for (Category salesCategory : salesCatalog.getListOfCategories()) {
            categoryCodes.add(salesCategory.getCategoryCode());
          }
        }
      }
    }
    return catalogService.getItemCatalogsWithCategoryHierarchyExistsInPCB(username, requestId,
        categoryCodes.stream().collect(toList()));
  }

  @Override
  public List<SalesCategorySequence> getSalesCategorySequenceListFromCategoryHierarchy(
      List<ItemCatalogVO> itemCatalogVOs) throws Exception {
    Map<String, SalesCategorySequence> salesCategorySequenceList = new HashMap<>();
    if (CollectionUtils.isNotEmpty(itemCatalogVOs)) {
      for (ItemCatalogVO itemCatalogVO : itemCatalogVOs) {
        if (CollectionUtils.isNotEmpty(itemCatalogVO.getItemCategories())) {
          for (ItemCategoryVO itemCategoryVO : itemCatalogVO.getItemCategories()) {
            if (!salesCategorySequenceList.containsKey(itemCategoryVO.getProductCategoryCode())) {
              salesCategorySequenceList.put(itemCategoryVO.getProductCategoryCode(),
                  new SalesCategorySequence(itemCategoryVO.getProductCategoryCode(),
                      itemCategoryVO.getLevel()));
            } else {
              int sequence = salesCategorySequenceList.get(itemCategoryVO.getProductCategoryCode())
                  .getSequence();
              salesCategorySequenceList.put(itemCategoryVO.getProductCategoryCode(),
                  new SalesCategorySequence(itemCategoryVO.getProductCategoryCode(),
                      Math.max(sequence, itemCategoryVO.getLevel())));
            }
          }
        }
      }
    }
    return new ArrayList<>(salesCategorySequenceList.values());
  }

  @Override
  public void updateItemsPristineDataByItemCode(PristineDataItem pristineDataItem) throws Exception {
    List<Item> currentItems;
    String itemId = pristineDataItem.getPcbProductItemId();
    Map<String, Item> changedItemsMap = new HashMap<>();
    if (StringUtils.isNotEmpty(pristineDataItem.getPristineId())) {
      currentItems = saveAndGetItems(pristineDataItem, itemId, changedItemsMap);
    } else {
      //For delete event when defining attributes change
      currentItems = deletePristineDetailAndGetItems(pristineDataItem, itemId, changedItemsMap);
    }
    Map<String, Item> currentItemsMap =
        currentItems.stream().collect(toMap(Item::getItemSku, Function.identity()));
    LOG.info("currentItems : {}", currentItemsMap.keySet());
    for (Map.Entry<String, Item> changedItem : changedItemsMap.entrySet()) {
      if (!currentItemsMap.containsKey(changedItem.getKey())) {
        currentItemsMap.put(changedItem.getKey(), changedItem.getValue());
        currentItems.add(changedItem.getValue());
      }
    }
    LOG.info("currentItems after adding changedItems : {}", currentItemsMap.keySet());
    if (CollectionUtils.isNotEmpty(currentItems)) {
      Set<PristineDataItem> pristineItems =
          updateAndGetPristineCategoryDetails(pristineDataItem, currentItems, changedItemsMap);
      pristineItemRepository.updateSalesCategorySequencesAndDPC(pristineItems);
      for (Item item : changedItemsMap.values()) {
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
        this.saveAndPublishService.publishPristineItem(item.getStoreId(), item);
      }
      productAndItemSolrIndexerService.updateSolrOnPristineChanges(currentItems);
    }
  }

  private List<Item> getItems(String itemId) {
    List<Item> itemList = new ArrayList<>();
    if (skuValidator.isItemSku(itemId)) {
      itemList
          .add(itemRepository.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemId, false));
    } else {
      itemList = itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemId);
    }
    return itemList;
  }

  @Override
  public void updateItemsPristineData(String storeId,
      Map<String, PristineDataItem> itemSkuPristineDataItemsMap) throws Exception {
    if (isProductVisibilityEnabled) {
      String itemId =
          itemSkuPristineDataItemsMap.keySet().stream().findFirst().orElse(StringUtils.EMPTY);
      Set<String> pristineIdList = itemSkuPristineDataItemsMap.values().stream()
          .map(PristineDataItem::getPristineId).collect(Collectors.toSet());
      List<PristineDataItem> existingPristineList =
          pristineItemRepository.findByPristineIdIn(pristineIdList);
      Map<String, PristineDataItem> existingPristineMap =
          generatePristineIdToModelMap(existingPristineList);
      List<Item> resultItemList = new ArrayList<>();
      if (skuValidator.isItemSku(itemId)) {
        List<Item> items = itemRepository
            .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId,
                itemSkuPristineDataItemsMap.keySet());
        for (Item item : items) {
          if (itemSkuPristineDataItemsMap.containsKey(item.getItemSku())) {
            PristineDataItem pristineItem =
                getPristineItemFromData(item.getItemSku(), itemSkuPristineDataItemsMap,
                    existingPristineMap);
            item.setPristineDataItem(pristineItem);
            resultItemList.add(item);
          }
        }
      } else {
        List<Item> items = itemRepository
            .findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(storeId,
                itemSkuPristineDataItemsMap.keySet());
        for (Item item : items) {
          if (itemSkuPristineDataItemsMap.containsKey(item.getItemCode())) {
            PristineDataItem pristineItem =
                getPristineItemFromData(item.getItemCode(), itemSkuPristineDataItemsMap,
                    existingPristineMap);
            item.setPristineDataItem(pristineItem);
            resultItemList.add(item);
          }
        }
      }
      if (CollectionUtils.isNotEmpty(resultItemList)) {
        itemRepository.saveAll(resultItemList);
        productAndItemSolrIndexerService.updateSolrOnPristineChanges(resultItemList);
      }
      resultItemList.forEach(item ->
        this.cacheEvictHelperService.evictItemData(storeId, item));
    }
  }

  private PristineDataItem getPristineItemFromData(String itemId,
      Map<String, PristineDataItem> itemSkuPristineDataItemsMap,
      Map<String, PristineDataItem> existingPristineMap) {

    PristineDataItem pristineDataItem = itemSkuPristineDataItemsMap.get(itemId);
    pristineDataItem.setPristineProductName(getPristineProductName(pristineDataItem));
    if (existingPristineMap.containsKey(pristineDataItem.getPristineId())) {
      pristineDataItem = existingPristineMap.get(pristineDataItem.getPristineId());
    } else {
      if (StringUtils.isNotBlank(pristineDataItem.getPristineProductName())) {
        PristineDataItem pristineDataItemByProductName =
            pristineItemRepository.findByPristineProductNameIgnoreCase(pristineDataItem.getPristineProductName());
        if (pristineDataItemByProductName != null) {
          existingPristineMap.put(pristineDataItem.getPristineId(), pristineDataItemByProductName);
          pristineDataItem = pristineDataItemByProductName;
        } else {
          pristineDataItem = pristineItemRepository.save(pristineDataItem);
          existingPristineMap.put(pristineDataItem.getPristineId(), pristineDataItem);
        }
      } else {
        pristineDataItem = pristineItemRepository.save(pristineDataItem);
        existingPristineMap.put(pristineDataItem.getPristineId(), pristineDataItem);
      }
    }
    return pristineDataItem;
  }

  private Map<String, PristineDataItem> generatePristineIdToModelMap(
      List<PristineDataItem> existingPristineList) {
    Map<String, PristineDataItem> pristineItemMap = new HashMap<>();
    for (PristineDataItem item : existingPristineList) {
      pristineItemMap.put(item.getPristineId(), item);
    }
    return pristineItemMap;
  }

  private String getPristineProductName(PristineDataItem pristineDataItem) {

    if (PristineCategory.HANDPHONE.name().equals(pristineDataItem.getPristineCategory())
        || PristineCategory.CAMERA.name().equals(pristineDataItem.getPristineCategory()))
      return createPristineProductName(pristineDataItem.getPristineBrand(),
          pristineDataItem.getPristineModel(), pristineDataItem.getPristineListingAttributes(),
          pristineDataItem.getProductCondition());
    else if (PristineCategory.COMPUTER.name().equals(pristineDataItem.getPristineCategory())) {

      return createComputerPristineProductName(pristineDataItem.getPristineBrand(),
          pristineDataItem.getPristineModel(), pristineDataItem.getPristineListingAttributes(),
          pristineDataItem.getProductCondition());
    }
    return StringUtils.EMPTY;
  }

  private String createComputerPristineProductName(String brand, String model,
      Map<String, String> listingAttributesValues, String productCondition) {

    StringBuilder pristineProductName = new StringBuilder(StringUtils.EMPTY);
    pristineProductName.append(brand).append(StringUtils.SPACE).append(model)
        .append(StringUtils.SPACE).append(HYPHEN).append(StringUtils.SPACE);
    for (Map.Entry<String, String> entry : listingAttributesValues.entrySet()) {
      if (FieldNames.COLOR.equals(entry.getKey())) {
        pristineProductName.append(entry.getValue()).append(StringUtils.SPACE)
            .append(ROUND_BRACKET_OPEN);
      } else {
        pristineProductName.append(entry.getValue()).append(FORWARD_SLASH);
      }
    }
    String productName =
        pristineProductName.substring(0, pristineProductName.length() - 2) + ROUND_BRACKET_CLOSE;

    return productName + getConditionName(productCondition);
  }

  private String createPristineProductName(String brand, String model,
      Map<String, String> listingAttributesValues, String productCondition) {
    StringBuilder pristineProductName = new StringBuilder(StringUtils.EMPTY);
    pristineProductName.append(brand).append(StringUtils.SPACE).append(model)
        .append(StringUtils.SPACE).append(ROUND_BRACKET_OPEN);
    for (Map.Entry<String, String> entry : listingAttributesValues.entrySet()) {
      pristineProductName.append(entry.getValue() + COMMA + StringUtils.SPACE);
    }
    String productName =
        pristineProductName.substring(0, pristineProductName.length() - 2) + ROUND_BRACKET_CLOSE;

    return productName + getConditionName(productCondition);
  }

  private String getConditionName(String productCondition) {

    StringBuilder conditionName = new StringBuilder(StringUtils.EMPTY);
    if (StringUtils.isNotEmpty(productCondition) && !(productCondition.equals(PRODUCT_CONDITION_NEW)
        || productCondition.equals(PRODUCT_CONDITION_BARU))) {
      conditionName.append(StringUtils.SPACE).append(ROUND_BRACKET_OPEN).append(productCondition)
          .append(ROUND_BRACKET_CLOSE).toString();
    }
    return conditionName.toString();
  }

  @Override
  public void updateResignMerchantItemsByMerchantCode(String storeId, String requestId, String username,
      String merchantCode) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), ItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    int page = 0;
    int size = DEFAULT_PAGE_SIZE;
    Page<Item> itemPage;
    List<List<Item>> updatedItemsList = new ArrayList<>();
    do {
      itemPage = this.itemRepository
          .findItemsByStoreIdAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(storeId, merchantCode,
              PageRequest.of(page,size));
      for (Item item : itemPage.getContent()) {
        item.setArchived(true);
        for (ItemViewConfig itemViewConfig : item.getItemViewConfigs()) {
          itemViewConfig.setBuyable(false);
          itemViewConfig.setDiscoverable(false);
        }
      }
      updatedItemsList.add(itemPage.getContent());
      page++;
    } while (page * size < itemPage.getTotalElements());
    for (List<Item> items : updatedItemsList) {
      try {
        this.saveOperationService.saveItems(items, null);
      } catch (ApplicationRuntimeException e) {
        log.error("failed to save current batch of items = {} ", itemPage.getContent(), e);
      }
    }
  }

  @Override
  public List<Item> getItemsByMerchantCodeAndMerchantSkus(String storeId, String requestId, String username,
      String merchantCode, List<String> merchantSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), ItemServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(merchantSkus), ItemServiceImpl.LIST_OF_MERCHANT_SKU_MUST_NOT_BE_BLANK);

    return itemRepository.findByStoreIdAndMerchantCodeAndMerchantSkuInAndMarkForDeleteFalseAndIsArchivedFalse(
        storeId, merchantCode, merchantSkus);
  }

  @Override
  public void updatePromoBundlingByItemSkus(String storeId, Set<String> itemSkus, boolean promoBundling) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), CommonConstants.ITEM_SKUS_MUST_NOT_BE_EMPTY);
    Set<String> itemSkusNeedToBeUpdated = this.itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, itemSkus).stream()
        .filter(item -> item.isPromoBundling() != promoBundling).map(Item::getItemSku).collect(Collectors.toSet());
    if (CollectionUtils.isNotEmpty(itemSkusNeedToBeUpdated)) {
      List<Item> items = this.saveOperationService
          .updateItemFieldByItemSkus(storeId, itemSkusNeedToBeUpdated, ProductFieldNames.PROMO_BUNDLING, promoBundling);
      productAndItemSolrIndexerService.updateSolrOnPromoBundlingFlagChange(items, promoBundling);
      productL3SolrService.updatePromoOrWholesaleItemSkus(
          items.stream().map(item -> setPromoBundling(item, promoBundling)).collect(toList()), true);
    }
  }

  @Override
  public void updatePromoBundlingByItemSkusInItemPickupPoint(String storeId, Set<String> itemSkus,
      boolean promoBundling) {
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), CommonConstants.ITEM_SKUS_MUST_NOT_BE_EMPTY);
    List<Item> items = this.itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, itemSkus);
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.updateFieldByItemSkusAndDelivery(storeId, itemSkus,
            ProductFieldNames.PROMO_BUNDLING, true, promoBundling);
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(toMap(ItemPickupPoint::getItemSku, Function.identity()));
    List<Item> itemsUpdated = items.stream().map(item -> setPromoBundling(itemPickupPointMap, item)).collect(toList());
    productAndItemSolrIndexerService.updateSolrOnPromoBundlingFlagChange(itemsUpdated, promoBundling);
    productL3SolrService.updatePromoOrWholesaleItemSkus(
        itemsUpdated.stream().map(item -> setPromoBundling(item, promoBundling)).collect(toList()), true);
  }

  @Override
  public void updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(String storeId, List<ItemInfo> itemInfos,
      boolean promoBundling) {
    checkArgument(CollectionUtils.isNotEmpty(itemInfos), CommonConstants.ITEM_INFOS_MUST_NOT_BE_EMPTY);
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.updateFieldByItemSkusAndPPCode(storeId, itemInfos, ProductFieldNames.PROMO_BUNDLING,
            promoBundling);
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      Map<String, List<String>> productSkuMap = itemPickupPoints.stream().collect(
        Collectors.groupingBy(ItemPickupPoint::getProductSku, Collectors.mapping(ItemPickupPoint::getItemSku,
            Collectors.collectingAndThen(Collectors.toSet(), itemSkus -> new ArrayList<>(itemSkus)))));
      Map<String, String> productSkuToMerchantCodeMap = itemPickupPoints.stream()
          .collect(toMap(ItemPickupPoint::getProductSku, ItemPickupPoint::getMerchantCode, (a, b) -> b));
      productAndItemSolrIndexerService.updateSolrOnPromoFlagChangeByItemSkus(productSkuMap,
        promoBundling, SolrFieldNames.PROMO_BUNDLING, productSkuToMerchantCodeMap);
    }
  }

  private Item setPromoBundling(Map<String, ItemPickupPoint> itemPickupPointMap, Item item) {
    if (Objects.nonNull(itemPickupPointMap.get(item.getItemSku()))) {
      item.setPromoBundling(itemPickupPointMap.get(item.getItemSku()).isPromoBundling());
    }
    return item;
  }

  private Item setPromoBundling(Item item, boolean promoBundling) {
    item.setPromoBundling(promoBundling);
    return item;
  }

  @Override
  public List<Item> getItemsByPristineMasterId(String storeId, String username, String requestId,
      String pristineMasterId) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotEmpty(pristineMasterId),
        CommonConstants.PRISTINE_ID_MUST_NOT_BE_BLANK);
    List<PristineDataItem> pristineItems = this.pristineItemRepository.findByPristineMasterId(pristineMasterId);
    if(CollectionUtils.isEmpty(pristineItems)){
      return new ArrayList<>();
    }
    List<Item> itemList = this.itemRepository
        .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(storeId,
            pristineItems);
    return itemList;
  }

  /**
   * Add/update pristine mappings for pristine products
   * @param pristineDataItem
   * @param itemId
   * @param changedItemsMap
   * @return
   */
  private List<Item> saveAndGetItems(PristineDataItem pristineDataItem, String itemId, Map<String, Item> changedItemsMap) {
    pristineDataItem.setPristineProductName(getPristineProductName(pristineDataItem));
    List<Item> currentItems = getItems(itemId);
    if (CollectionUtils.isNotEmpty(currentItems)) {
      PristineDataItem existingPristineItem = pristineItemRepository.findByPristineId(pristineDataItem.getPristineId());
      if(existingPristineItem == null){
        pristineDataItem = pristineItemRepository.save(pristineDataItem);
      }else{
        BeanUtils.copyProperties(existingPristineItem, pristineDataItem);
      }
      for (Item item : currentItems) {
        item.setPristineDataItem(pristineDataItem);
        itemRepository.updatePristineDataItem(storeId, item);
        changedItemsMap.put(item.getItemSku(), item);
      }
      currentItems = itemRepository
          .findByStoreIdAndPristineDataItemAndMarkForDeleteFalseAndIsArchivedFalse(storeId, pristineDataItem);
    }
    return currentItems;
  }

  /**
   * Delete Pristine mapping for pristine products
   * @param pristineDataItem
   * @param itemId
   * @param changedItemsMap
   * @return
   */
  private List<Item> deletePristineDetailAndGetItems(PristineDataItem pristineDataItem,
      String itemId, Map<String, Item> changedItemsMap) {
    List<Item> currentItems = new ArrayList<>();
    List<Item> existingItemsByPristineMasterId = new ArrayList<>();
    currentItems = getItems(itemId);
    boolean isItemSku = skuValidator.isItemSku(itemId);
    PristineDataItem existingPristineItem = null;
    if (CollectionUtils.isNotEmpty(currentItems)) {
      for (Item item : currentItems) {
        if (item.getPristineDataItem() != null) {
          existingPristineItem = item.getPristineDataItem();
          BeanUtils.copyProperties(existingPristineItem, pristineDataItem, "defaultProductCode");
        }
        boolean itemUpdated = false;
        if (isItemSku) {
          item.setPristineDataItem(null);
          itemUpdated = true;
        } else {
          if (item.isSynchronized()) {
            item.setPristineDataItem(null);
            itemUpdated = true;
          }
        }
        if (itemUpdated) {
          itemRepository.updatePristineDataItem(storeId, item);
          changedItemsMap.put(item.getItemSku(), item);
        }
      }
      if (StringUtils.isNotEmpty(pristineDataItem.getDefaultProductCode()) && pristineDataItem.getDefaultProductCode()
          .equals(existingPristineItem.getDefaultProductCode())) {
        existingItemsByPristineMasterId = getItemsByPristineId(storeId, existingPristineItem.getPristineId());
      } else {
        if (StringUtils.isNotEmpty(pristineDataItem.getPristineMasterId())) {
          existingItemsByPristineMasterId =
              getItemsByPristineMasterId(storeId, username, requestId, pristineDataItem.getPristineMasterId());
        }
      }
    }
    return existingItemsByPristineMasterId;
  }

  /**
   * Update Sales category for pristine products
   * @param pristineDataItem
   * @param items
   * @param changedItemsMap
   * @return
   * @throws Exception
   */
  private Set<PristineDataItem> updateAndGetPristineCategoryDetails(PristineDataItem pristineDataItem,
      List<Item> items, Map<String, Item> changedItemsMap) throws Exception {
    List<ItemCatalogVO> itemCatalogVOList = getItemCatalogsByPristineId(pristineDataItem.getPristineId());
    List<SalesCategorySequence> salesCategorySequenceList =
        getSalesCategorySequenceListFromCategoryHierarchy(itemCatalogVOList);
    List<SalesCategorySequence> oldSalesCategorySequenceList = pristineDataItem.getSalesCategorySequences();
    boolean isSalesCategoryChanged =
        isItemCatalogsChange(itemCatalogVOList, pristineDataItem.getPristineCategoriesHierarchy());

    Set<PristineDataItem> pristineItems = new HashSet<>();
    for (Item item : items) {
      if (item.getPristineDataItem() != null) {
        if (pristineDataItem.getPristineId().equals(item.getPristineDataItem().getPristineId())
            && isSalesCategoryChanged) {
          item.getPristineDataItem()
              .setOldSalesCategorySequences(oldSalesCategorySequenceList);
          item.getPristineDataItem().setSalesCategorySequences(salesCategorySequenceList);
          item.getPristineDataItem().setPristineCategoriesHierarchy(itemCatalogVOList);
          if(!changedItemsMap.containsKey(item.getItemSku())){
            changedItemsMap.put(item.getItemSku(), item);
          }
        }
        item.getPristineDataItem().setDefaultProductCode(pristineDataItem.getDefaultProductCode());
        pristineItems.add(item.getPristineDataItem());
      }
    }
    return pristineItems;
  }

  /**
   *
   * @param newItemCatalogVOs
   * @param oldItemCatalogVOs
   * @return
   */
  private  boolean isItemCatalogsChange(List<ItemCatalogVO> newItemCatalogVOs,
      List<ItemCatalogVO> oldItemCatalogVOs) {
    Map<String, ItemCategoryVO> oldItemCategoryVoMap =
        Optional.ofNullable(oldItemCatalogVOs)
            .orElseGet(Collections::emptyList)
        .stream().map(ItemCatalogVO::getItemCategories)
            .flatMap(List::stream)
            .collect(toMap(ItemCategoryVO::getCategoryId, Function.identity(),
                (oldValue, newValue) -> newValue));
    Map<String, ItemCategoryVO> newItemCategoryVoMap =
        Optional.ofNullable(newItemCatalogVOs)
            .orElseGet(Collections::emptyList).stream().map(ItemCatalogVO::getItemCategories)
            .flatMap(List::stream)
            .collect(toMap(ItemCategoryVO::getCategoryId, Function.identity(),
                (oldValue, newValue) -> newValue));
   return oldItemCategoryVoMap.keySet().size() != newItemCategoryVoMap.keySet().size()
       || newItemCategoryVoMap.keySet().stream()
       .anyMatch(newCategoryCode -> !oldItemCategoryVoMap.containsKey(newCategoryCode));
  }

  @Override
  public List<String> getPristineIdsByProductSku(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ItemServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    Set<String> pristineIds = new HashSet<>();
    List<Item> items =
        itemRepository.getByProductSkuAndPristineDataItemExist(storeId, productSku);
    for(Item item : items){
      pristineIds.add(item.getPristineDataItem().getPristineId());
    }
    return new ArrayList<>(pristineIds);
  }

  @Override
  public Map<String, String> getItemCodesByItemSkuIn(String storeId, Set<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus), ItemServiceImpl.ITEM_SKUS_MUST_NOT_BE_EMPTY);

    final List<Item> items =
        itemRepository.findItemCodesByStoreIdAndItemSkuIn(storeId, itemSkus);

    return items.stream().filter(item -> Objects.nonNull(item.getItemCode()))
        .collect(toMap(Item::getItemSku, Item::getItemCode));
  }

  @Override
  public Map<String, Item> getItemsByItemSkuIn(String storeId, Set<String> itemSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(itemSkus),
        ItemServiceImpl.ITEM_SKUS_MUST_NOT_BE_EMPTY);

    final List<Item> items =
        itemRepository.findItemCodeAndPristineDataItemByStoreIdAndItemSkuIn(storeId, itemSkus);

    return items.stream().collect(toMap(Item::getItemSku, Function.identity()));
  }

  @Override
  public Item updateCncActivated(String storeId, String itemSku, boolean cncActivated, String username) {
    Item item = this.itemRepository.updateCncActivated(storeId, itemSku, cncActivated, username);
    this.cacheEvictHelperService.evictItemCache(storeId, item);
    return item;
  }

  @Override
  public void updateCategoryCodeByItemSkuList(String storeId, List<String> itemSkus, String categoryCode) {
    List<Item> items = this.itemRepository.updateCategoryCodeByItemSkus(storeId, itemSkus, categoryCode);
    items.forEach(item -> cacheEvictHelperService.evictItemCache(storeId, item));
  }

  @Override
  public void updateCncActivatedByMerchantCode(String storeId, String merchantCode, boolean cncActivated, String username) {
    this.itemRepository.updateCncActivatedByMerchantCode(storeId, merchantCode, cncActivated, username);
    List<Item> items =
        this.itemRepository.findSpecificFieldsByStoreIdAndMarkForDeleteFalseAndIsArchivedFalseAndMerchantCode(
            storeId, merchantCode);
    items.forEach(item -> cacheEvictHelperService.evictItemCache(storeId, item));
  }

  @Override
  public void updateCncActivatedByItemSkusAndPublish(String storeId, Set<String> itemSkuSet, boolean cncActivated,
      String username) throws Exception {
    this.itemRepository.updateCncActivatedByItemSkuInMarkForDeleteFalse(storeId, itemSkuSet, cncActivated, username);
    List<Item> items = this.itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, itemSkuSet);
    items.forEach(item -> item.setCncActivated(cncActivated));
    this.saveAndPublishService.publishItemDataChangeEvent(items);
    items.forEach(item -> cacheEvictHelperService.evictItemCache(storeId, item));
  }

  @Override
  public Item getItemByItemCode(String storeId, String itemCode){
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode), ItemServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);
    Set<String> itemSkus = itemCacheHelperService
        .findAllItemSkusByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      return itemCacheHelperService
          .findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSkus.iterator().next(), false, false,
            false);
    } else {
      return null;
    }

  }

  @Override
  public List<Item> findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
      String storeId, String itemCode) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(itemCode), ItemServiceImpl.ITEM_CODE_MUST_NOT_BE_BLANK);

    return this.itemRepository.findItemSkusByStoreIdAndItemCodeAndCncActivatedTrueAndMarkForDeleteFalseAndIsArchivedFalseAndSynchronizedTrue(
        storeId, itemCode);
  }

  @Override
  public Set<String> getItemCodesByPristine(String storeId, PristineDataItem pristineDataItem){
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(pristineDataItem != null, ItemServiceImpl.PRISTINE_DATA_ITEM_MUST_NOT_BE_NULL);
    return itemCacheHelperService.findItemCodesByPristine(storeId, pristineDataItem);
  }

  @Override
  public Set<String> getItemCodesByPristineId(String storeId, String pristineId) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(pristineId),
        CommonConstants.PRISTINE_ID_MUST_NOT_BE_BLANK);
    return this.itemCacheHelperService
        .findCacheableItemCodesByStoreIdAndPristineAndMarkForDeleteFalse(storeId, pristineId);
  }

  @Override
  public ItemAndBundlingInfoVO getItemsAndBundlingInfo(String storeId, String channelId, String clientId,
      String requestId, Set<String> itemSkus, Set<String> promoBundlingIds, String username)
      throws Exception {
    checkArgument(itemSkus.size() < Integer.valueOf(systemParameterService
            .findValueByStoreIdAndVariable(storeId, SystemParameterNames.MAX_ITEM_COUNT_TRANSACTION_API_SWITCH).getValue()),
        ErrorMessages.ITEM_COUNT_LIMIT_FOR_TRANSACTION_API);
    MandatoryRequestParam param = MandatoryRequestParam
        .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    List<ProductAndItemsVO> productAndItemsVOS =
        productSearchService.getProductAndItemsInfoForAllItems(param, itemSkus, false, true, true, false, false);
    Map<String, Product> productsByProductSku =
        productAndItemsVOS.stream().map(ProductAndItemsVO::getProduct).collect(
            toMap(Product::getProductSku, Function.identity(), (newValue, oldValue) -> newValue));

    Map<String, Item> itemsByItemSku = productAndItemsVOS.stream()
        .flatMap(productAndItemsVO -> productAndItemsVO.getItems().stream()).collect(
            toMap(Item::getItemSku, Function.identity(), (newValue, oldValue) -> newValue));
    List<PromoBundlingDetailResponseVO> promoBundlingDetails =
        this.promoBundlingService.getByPromoBundlingIds(param, promoBundlingIds, itemSkus);
    ItemAndBundlingInfoVO itemAndBundlingInfoVO = new ItemAndBundlingInfoVO();
    Map<String, Price> priceByItemSku = new HashMap<>();
    itemAndBundlingInfoVO.setItems(
        constructItemInfoVOList(itemsByItemSku, productsByProductSku, priceByItemSku));
    itemAndBundlingInfoVO
        .setPromoBundlings(constructPromoBundlingVOList(promoBundlingDetails, priceByItemSku));
    return itemAndBundlingInfoVO;
  }

  private List<ItemInfoVO> constructItemInfoVOList(Map<String, Item> itemsByItemSku,
      Map<String, Product> productsByProductSku, Map<String, Price> priceByItemSku){
    List<ItemInfoVO> itemInfoVOList = new ArrayList<>();
    itemsByItemSku.forEach((itemSku, item) -> {
      try {
        ItemPickupPoint itemPickupPoint =
            itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
        this.objectConverterService.overrideL4DetailsFromL5(Arrays.asList(item), Arrays.asList(itemPickupPoint));
        ItemInfoVO itemInfoVO = new ItemInfoVO();
        Product product = productsByProductSku.get(item.getProductSku());
        priceByItemSku.put(item.getItemSku(), item.getPrice().stream().findFirst().orElse(new Price()));

        objectConverterService.convertItemAndProductToItemInfoVO(itemInfoVO, item, product);
        objectConverterService.convertProductToItemInfoVO(itemInfoVO, product);
        itemInfoVOList.add(itemInfoVO);
      } catch (Exception e) {
        LOG.error("failed to convert ProductAndItemVO to itemInfoVO with itemSku, {}", itemSku, e);
      }
    });

    return itemInfoVOList;
  }

  private List<PromoBundlingVO> constructPromoBundlingVOList(
      List<PromoBundlingDetailResponseVO> promoBundlingDetails, Map<String, Price> priceByItemSku) {
    List<PromoBundlingVO> promoBundlingVOList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(promoBundlingDetails)) {
      promoBundlingDetails.stream().forEach(promoBundlingDetailResponseVO -> {
        if(CollectionUtils.isNotEmpty(promoBundlingDetailResponseVO.getComboRules())){
          promoBundlingVOList.add(objectConverterService
              .convertPromoBundlingDetailResponseVOToPromoBundlingVO(promoBundlingDetailResponseVO,
                  getPriceMapFromComboRuleVO(promoBundlingDetailResponseVO.getComboRules(), priceByItemSku)));
        }
      });
    }

    return promoBundlingVOList;
  }

  public Map<String, Price> getPriceMapFromComboRuleVO(List<ComboRuleVO> comboRuleVOList,
      Map<String, Price> priceByItemSku) {
    comboRuleVOList.stream().forEach(comboRuleVO -> {
      String itemSku = comboRuleVO.getItemSku();
      if (!priceByItemSku.containsKey(itemSku)) {
        try {
          Price price =
              this.getItem(storeId, requestId, username, itemSku, false, false, false, false, null, false, false).getPrice().stream()
                  .findFirst().orElse(new Price());
          priceByItemSku.put(itemSku, price);
        } catch (Exception e) {
          LOG.error("failed to get Price from item", e);
        }
      }
    });
    return priceByItemSku;
  }

  @Override
  public List<ItemPriceVO> getAllItemSkuByItemCode(String storeId, String username, String requestId, String itemCode,
      boolean fetchAll) {
    GdnPreconditions.checkArgument(itemCode != null, ITEM_CODE_MUST_NOT_BE_BLANK);
    List<Item> itemList = itemCacheHelperService
        .findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
    List<ItemPriceVO> itemPriceVOS = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemList) && Objects
        .nonNull(itemList.get(0).getPristineDataItem())) {
      List<Item> items = this.itemCacheHelperService
          .findItemsByPristineId(storeId, username, requestId,
              itemList.get(0).getPristineDataItem().getPristineId(), false);
      itemPriceVOS = (fetchAll ?
          addItemToItemWithoutFilteringBuyableAndDiscoverablePriceVOList(items) :
          addItemToItemPriceVOList(items));
    } else {
      itemPriceVOS = (fetchAll ?
          addItemToItemWithoutFilteringBuyableAndDiscoverablePriceVOList(itemList) :
          addItemToItemPriceVOList(itemList));
    }
    return itemPriceVOS;
  }

  /**
   * Add items to ItemPriceVo list which are buyable as well as discoverable
   *
   * @param items list of items to add in itemPriceVOS
   * @return Item Price VO list
   */
  private List<ItemPriceVO> addItemToItemPriceVOList(List<Item> items) {
    List<ItemPriceVO> itemPriceVOS = new ArrayList<>();
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
      checkArgument(Objects.nonNull(itemPickupPoint),
          String.format(ErrorMessages.CANNOT_FIND_ITEM_PICKUP_POINT_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
              item.getItemSku(), item.getPickupPointCode()));
      if (isBuyableDiscoverable(itemPickupPoint)) {
        ItemPriceVO itemPriceVO = new ItemPriceVO.ItemPriceBuilder().setItemSku(item.getItemSku()).setBuyable(true)
            .setMerchantCode(item.getMerchantCode()).build();
        this.setDefaultPrice(itemPickupPoint, itemPriceVO);
        itemPriceVOS.add(itemPriceVO);
      }
    }
    return itemPriceVOS;
  }

  /**
   * Add items to ItemPriceVo list without filtering which are buyable as well as discoverable
   *
   * @param items list of items to add in itemPriceVOS
   * @return Item Price VO list
   */
  private List<ItemPriceVO> addItemToItemWithoutFilteringBuyableAndDiscoverablePriceVOList(List<Item> items) {
    return items.stream().map(item -> {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
      ItemPriceVO itemPriceVO =
          new ItemPriceVO.ItemPriceBuilder().setItemSku(item.getItemSku()).setBuyable(isBuyable(itemPickupPoint))
              .setMerchantCode(item.getMerchantCode()).build();
      this.setDefaultPrice(itemPickupPoint, itemPriceVO);
      return itemPriceVO;
    }).collect(Collectors.toList());
  }

  /**
   * set prive of Item whose Channel is Default
   *
   * @param itemPickupPoint item of which defalut price is copied
   * @param itemPriceVO object in which price will be copied
   */
  private void setDefaultPrice(ItemPickupPoint itemPickupPoint, ItemPriceVO itemPriceVO) {
    Price price = itemPickupPoint.getPrice().stream().filter(p -> p.getChannel().equalsIgnoreCase(DEFAULT)).findFirst()
        .orElse(null);
    if (price != null) {
      itemPriceVO.setOfferPrice(price.getOfferPrice());
      itemPriceVO.setListPrice(price.getListPrice());
    }
  }

  /**
   * check if item is buyable and discoverable
   *
   * @param itemPickupPoint Item to check if buyable and discoverable
   * @return whether is discoverable or not
   */
  private boolean isBuyableDiscoverable(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.getItemViewConfig().stream().filter(
            config -> config.getChannel().equalsIgnoreCase(DEFAULT) && config.isBuyable() && config.isDiscoverable())
        .count() > 0;
  }

  private boolean isBuyable(ItemPickupPoint itemPickupPoint) {
    return itemPickupPoint.getItemViewConfig().stream()
        .anyMatch(config -> config.getChannel().equalsIgnoreCase(DEFAULT) && config.isBuyable());
  }

  @Override
  public void updatePromotionPriceForItemSkuList(String storeId, Set<String> itemSkus) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkus), "empty Skus list");
    List<Item> items =
        findByStoreIdAndItemSkus(storeId, itemSkus).stream().map(this::setItemDiscountPriceEmpty).collect(toList());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(items), "Items not present");
    this.itemPriceService.getAndSetPromotionPrice(requestId, username, items);
    items.forEach(item -> {
      ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
      itemPickupPoint.setItemSku(item.getItemSku());
      itemPickupPoint.setPrice(item.getPrice());
      itemPickupPoint.setProductSku(item.getProductSku());
      this.itemPickupPointService.updateDiscountPriceInItemPickupPoint(itemPickupPoint);
      this.cacheEvictHelperService.evictItemData(storeId, item);
    });
  }

  /**
   * initially set discount price as Empty before updating values from x_promo
   *
   * @param item item for which we need to set discount price as empty
   * @return
   */
  private Item setItemDiscountPriceEmpty(Item item) {
    item.getPrice().forEach(price -> price.setListOfDiscountPrices(Collections.emptyList()));
    return item;
  }

  @Override
  public List<Item> findByStoreIdAndItemSkus(String storeId, Set<String> itemSkus) {
    Map<String, Item> cachedItems =
        itemCacheHelperService.findItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(storeId, itemSkus);
    return itemSkus.stream().map(itemSku -> cachedItems.containsKey(itemSku) ?
        cachedItems.get(itemSku) :
        itemCacheHelperService
            .findItemByStoreIdAndItemSku(storeId, itemSku, Boolean.TRUE, Boolean.FALSE, false, null, false))
        .collect(Collectors.toList());
  }

  @Override
  public List<Item> findByStoreIdAndItemSkusReadFromPrimary(String storeId, Set<String> itemSkus) {
    return itemRepository.findItemByStoreIdAndItemSkuIn(storeId, itemSkus, true);
  }

  @Override
  public List<Item> findByStoreIdAndItemSkusNotCached(String storeId, Set<String> itemSkus) {
    return itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus);
  }

  public boolean updateItemFlashSaleActiveFlag(String storeId, List<String> itemSkus,
      boolean isFlashSaleActive) {
    try {
      if(CollectionUtils.isNotEmpty(itemSkus)) {
        List<Item> items = itemRepository
            .findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, new HashSet(itemSkus));
        items.forEach(item -> item.setFlashSaleActive(isFlashSaleActive));
        if (CollectionUtils.isNotEmpty(items)) {
          this.saveOperationService.saveItems(items, null);
        }
        for(Item item : items){
          cacheEvictHelperService.evictItemData(storeId, item);
        }
      }
    } catch (Exception ex) {
      LOG.error("Error while Updating isFlashSaleActive:{} for itemskus:{}", isFlashSaleActive,
          itemSkus, ex);
      return false;
    }
    return true;
  }


  @Override
  public List<Item> findByStoreIdAndMarkForDeleteFalseAndProductSkus(String storeId,
      String username, String requestId, Set<String> productSkus, boolean combineOthersBundlings) {
    List<Item> items = itemRepository
        .findByStoreIdAndMarkForDeleteFalseAndProductSkus(storeId, productSkus, CommonConstants.itemFields);
    fetchViewConfigsAndPriceFromL5(storeId, items);
    if (combineOthersBundlings) {
      items.forEach(item -> {
        this.itemCacheHelperService.setActivePromoBundlingsByPristineOrItemCode(storeId, item);
      });
    }
    return this.getItemsWithDiscount(storeId, username, requestId, items, false);
  }

  private void fetchViewConfigsAndPriceFromL5(String storeId, List<Item> items) {
    List<String> itemSkus = items.stream().map(Item::getItemSku).collect(Collectors.toList());
    List<ItemPickupPoint> itemPickupPoints =
        itemSkus.stream().map(itemSku -> itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku))
            .filter(Objects::nonNull).collect(Collectors.toList());
    objectConverterService.overrideL4DetailsFromL5(items, itemPickupPoints);
  }

  @Async
  @Override
  public void sendItemChangeEventByUpdatedBy(String storeId, String updatedBy) {
    LOG.info("Starting to publish item change for storeID : {} ,updatedBy : {} ",
        storeId, updatedBy);
    Slice<Item> items;
    Pageable pageRequest = PageRequest.of(0, ITEM_CHANGE_DEFAULT_PAGE_SIZE);
    int total =0;
    try {
      do {
        items = itemRepository.findByStoreIdAndUpdatedByAndMarkForDeleteFalse(storeId,
            updatedBy, pageRequest);
        for (Item item : items) {
          masterDataCacheService.evictMasterDataItem(item.getItemSku(), null);
        }
        saveAndPublishService.publishListOfItems(items.getContent());
        total += items.getNumberOfElements();
        pageRequest = pageRequest.next();
      } while (items.hasNext());
    } catch (Exception ex) {
      LOG.error("Exception while sending item change events ", ex);
    } finally {
      LOG.info("Publish Item Change completed for updatedBy {} : Total = {}",updatedBy, total);
    }
  }

  @Override
  public void updateItemMerchantDiscountPrice(String storeId, Item item) throws Exception {
    Item savedItem = this.itemRepository.updateItemMerchantDiscountPrice(storeId, item);
    this.cacheEvictHelperService.evictItemData(savedItem.getStoreId(), savedItem);
    this.saveAndPublishService.publishMerchantPromoDiscountEventChange(savedItem);
    this.productAndItemSolrIndexerService.indexMerchantPromoDiscountItem(savedItem, false);
  }

  @Override
  public void updateItemMerchantDiscountPriceWithoutUpdatingL4Db(String storeId, String itemSku, DiscountPrice discountPrice)
      throws Exception {
    Item item =
        itemCacheHelperService.findItemByStoreIdAndItemSku(storeId, itemSku, Boolean.TRUE, Boolean.FALSE, false, null,
            false);
    GdnPreconditions.checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, itemSku);
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    item.getPrice().forEach(price -> price.setMerchantPromoDiscountPrice(discountPrice));
    itemPickupPoint.getPrice().forEach(price -> price.setMerchantPromoDiscountPrice(discountPrice));
    this.itemPickupPointService.updateDiscountPriceInItemPickupPoint(itemPickupPoint);
    this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
    this.saveAndPublishService.publishMerchantPromoDiscountEventChange(itemPickupPoint);
    this.productAndItemSolrIndexerService.indexMerchantPromoDiscountItem(item, false);
  }

  @Override
  public void updateItemMerchantDiscountPriceByItemSkuAndPPCode(String storeId, String itemSku,
      DiscountPrice discountPrice, String ppCode, boolean activated) throws Exception {
    ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndPickupPointCode(storeId, itemSku, ppCode);
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    itemPickupPoint.setMerchantPromoDiscount(activated);
    itemPickupPoint.getPrice().forEach(price -> price.setMerchantPromoDiscountPrice(discountPrice));
    this.itemPickupPointService.updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPoint);
    productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(itemPickupPoint);
    this.productAndItemSolrIndexerService.indexMerchantPromoDiscountItemPickupPoint(itemPickupPoint, activated);
    this.saveAndPublishService.publishMerchantPromoDiscountEventChange(itemPickupPoint);
  }

  @Override
  public void updateMerchantPromoDiscountFlagInItemPickupPoint(String storeId, String itemSku, boolean isPromoActive)
      throws Exception {
    if (this.itemPickupPointService.updateMerchantPromoDiscountFlag(storeId, itemSku, isPromoActive)) {
      Item item = itemCacheHelperService
          .findItemByStoreIdAndItemSku(storeId, itemSku, Boolean.TRUE, Boolean.FALSE, false, null, false);
      item.setMerchantPromoDiscount(isPromoActive);
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      this.productAndItemSolrIndexerService.indexMerchantPromoDiscountItem(item, true);
      this.productL3SolrService.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    }
  }

  @Override
  public void updateMerchantPromoDiscountFlagByItemSkuAndPPCode(String storeId, String itemSku, boolean isPromoActive,
      String ppCode) {
    ItemInfo itemInfo = new ItemInfo(itemSku, ppCode, null);
    List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.updateFieldByItemSkusAndPPCode(storeId, Arrays.asList(itemInfo),
            ProductFieldNames.MERCHANT_PROMO_DISCOUNT, isPromoActive);
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      Map<String, List<String>> productSkuMap = itemPickupPoints.stream().collect(
          Collectors.groupingBy(ItemPickupPoint::getProductSku, Collectors.mapping(ItemPickupPoint::getItemSku,
              Collectors.collectingAndThen(Collectors.toSet(), itemSkus -> new ArrayList<>(itemSkus)))));
      Map<String, String> productSkuToMerchantCodeMap = itemPickupPoints.stream()
          .collect(toMap(ItemPickupPoint::getProductSku, ItemPickupPoint::getMerchantCode, (a, b) -> b));
      productAndItemSolrIndexerService.updateSolrOnPromoFlagChangeByItemSkus(productSkuMap, isPromoActive,
          SolrFieldNames.MERCHANT_PROMO_DISCOUNT, productSkuToMerchantCodeMap);
    }
  }

  @Override
  public void updateMerchantPromoDiscountFlag(String storeId, String itemSku, boolean isPromoActive) throws Exception {
    if (this.itemRepository.updateMerchantPromoDiscountFlag(storeId, itemSku, isPromoActive)) {
      Item item = itemCacheHelperService
          .findItemByStoreIdAndItemSku(storeId, itemSku, Boolean.TRUE, Boolean.FALSE, false, null, false);
      item.setMerchantPromoDiscount(isPromoActive);
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      this.productAndItemSolrIndexerService.indexMerchantPromoDiscountItem(item, true);
      this.productL3SolrService.updatePromoOrWholesaleItemSkus(Arrays.asList(item), true);
    }
  }

  @Override
  public Page<Item> findByStoreIdAndUpdatedDateGreaterThan(String storeId, Date lastIndexTime, Pageable pageable) {
    return itemRepository.findByStoreIdAndUpdatedDateGreaterThan(storeId, lastIndexTime, pageable);
  }

  @Override
  public void publishItemsByMerchantCodeToVoucher(VoucherItemSkusEventModel voucherItemSkusEventModel) {
    SystemParameter systemParameter = systemParameterService.findValueByStoreIdAndVariable(
        voucherItemSkusEventModel.getStoreId(),
        SystemParameterNames.SOLR_MERCHANT_VOUCHER_ITEMS_PAGE_SIZE);
    Pageable pageRequest = PageRequest.of(0,
        Objects.nonNull(systemParameter) ? Integer.valueOf(systemParameter.getValue()) : 500);
    Page<ProductAndItemSolr> itemSkuPage = null;
    do {
      itemSkuPage = productSearchService.getItemsByMerchantCode(
          voucherItemSkusEventModel.getStoreId(),
          voucherItemSkusEventModel.getMerchantCode(),
          voucherItemSkusEventModel.getVoucherCreatedDate(),
          pageRequest);
      if (CollectionUtils.isNotEmpty(itemSkuPage.getContent())) {
        voucherItemSkusEventModel.setItemSkus(itemSkuPage.getContent()
            .stream()
            .map(ProductAndItemSolr::getId)
            .collect(Collectors.toSet()));
        saveAndPublishService.publishItemSkuListForVoucher(voucherItemSkusEventModel);
      }
      pageRequest = itemSkuPage.nextPageable();
    } while (itemSkuPage.hasNext());
  }

  @Override
  public Pair<Map<String, Boolean>, List<Item>> suspendItems(String storeId, String productSku, boolean suspendItem) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ItemServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    LOG.info("Suspend item : {} for product with productSku", suspendItem, productSku);
    Map<String, Boolean> itemsMap = new HashMap<>();
    List<Item> items = itemRepository.findItemsByStoreIdAndProductSku(storeId, productSku);
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    List<Item> newItems = new ArrayList<>();
    String source = StringUtils.EMPTY;
    if (suspendItem) {
      source = Constants.SOURCE_SUSPENSION_FLOW;
      for (Item item : items) {
        List<ItemPickupPoint> itemPickupPoints =
            itemPickupPointService.findByStoreIdAndItemSku(item.getStoreId(), item.getItemSku());
        if (item.isArchived()) {
          item.setArchivedBeforeSuspension(true);
        } else {
          doArchivalAction(item, storeId, username,true, itemPickupPoints, false);
          item.setArchivedBeforeSuspension(false);
        }
        itemPickupPointList.addAll(itemPickupPoints);
        itemsMap.put(item.getItemSku(), true);
        item.setMarkForDelete(true);
        newItems.add(item);
      }
    } else {
      items = items.stream().filter(not(Item::isPermanentDelete)).collect(Collectors.toList());
      for (Item item : items) {
        List<ItemPickupPoint> itemPickupPoints =
            itemPickupPointService.findByStoreIdAndItemSku(item.getStoreId(), item.getItemSku());
        if (!item.getArchivedBeforeSuspension()) {
          doArchivalAction(item, storeId, username,false, itemPickupPoints, false);
          itemsMap.put(item.getItemSku(), false);
        } else {
          itemsMap.put(item.getItemSku(), true);
        }
        itemPickupPointList.addAll(itemPickupPoints);
        item.setArchivedBeforeSuspension(false);
        item.setMarkForDelete(false);
        newItems.add(item);
      }
    }
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointList;
    if (suspensionPublishOnlyDeliveryTrueL5) {
      itemPickupPoints = itemPickupPointList.stream().filter(ItemPickupPoint::isDelivery).collect(toList());
    }
    this.saveOperationService.saveItemsAndClearCacheWithoutUpdatingSolr(newItems, itemPickupPoints, source);
    return Pair.of(itemsMap, items);
  }

  @Override
  public boolean isPriceEditDisabled(Item item) {
    // Price edit will be disabled if its part of any active promo discount or any active campaigns (blibli subsidy is
    // excluded)
    Date now = new Date();
    return item.isMerchantPromoDiscount() || item.getPrice().stream().anyMatch(
        price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices().stream()
            .anyMatch(discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode()) && discountPrice
                .getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))));
  }

  @Override
  public boolean isPriceEditDisabled(ItemPickupPoint itemPickupPoint) {
    Date now = new Date();
    return itemPickupPoint.isMerchantPromoDiscount() || itemPickupPoint.getPrice().stream().anyMatch(
        price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price.getListOfDiscountPrices().stream()
            .anyMatch(discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode())
                && discountPrice.getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))));
  }

  @Override
  public void updateSubscriptionFlagByItemSku(String storeId, String itemSku,
    boolean subscriptionFlag, Set<String> preferredSubscriptionType) {
    try {
      checkArgument(this.skuValidator.isItemSku(itemSku), ItemServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSku);
      Item savedItem = this.itemRepository.findItemByStoreIdAndItemSku(storeId, itemSku, false);
      Set<String> existingSubscription = savedItem.getPreferredSubscriptionType();
      GdnPreconditions.checkArgument(Objects.nonNull(savedItem), ITEM_MUST_NOT_BE_NULL);
      savedItem.setSubscribable(subscriptionFlag);
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.SUBSCRIPTION_FLAG_CHANGE);
      savedItem.setItemChangeEventTypes(itemChangeEventTypes);
      setSubscriptionType(existingSubscription, preferredSubscriptionType, savedItem);
      saveAndPublishService.saveItems(Collections.singletonList(savedItem));
      this.cacheEvictHelperService.evictItemCache(savedItem.getStoreId(), savedItem);
    } catch (Exception e) {
      LOG.error("Exception caught while updating subscription flag for : {}, to : {}", itemSku, subscriptionFlag, e);
    }
  }

  private void setSubscriptionType(Set<String> existingSubscriptionType,
    Set<String> newPreferredSubscription, Item savedItem) {
    savedItem.setPreferredSubscriptionType(
      Objects.nonNull(newPreferredSubscription) ? newPreferredSubscription : new HashSet<>());
    savedItem.setOldPreferredSubscriptionType(
      Objects.nonNull(existingSubscriptionType) ? existingSubscriptionType : new HashSet<>());
  }

  @Override
  public List<Item> getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(String storeId, String itemCode) {
    return itemRepository.findByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);
  }

  @Override
  public List<Item> getItemsWithDiscountPrice(String storeId, String username, String requestId, List<Item> items) {
    return this.getItemsWithDiscount(storeId, username, requestId, items, false);
  }

  @Override
  public void updatePristineDPC(String itemSku, String pristineMasterId, String productCode) {
    if (Objects.isNull(itemSku)) {
      List<PristineDataItem> pristineItems = pristineItemRepository.findByPristineMasterId(pristineMasterId);
      pristineItems.forEach(pristineDataItem -> pristineDataItem.setDefaultProductCode(productCode));
      pristineItemRepository.updatePristineDPC(pristineItems);
      LOG.info("Updating item pristine data for pristine event : {}", pristineItems);
      List<Item> items = itemRepository
          .findByStoreIdAndPristineDataItemInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, pristineItems);
      for (Item item : items) {
        LOG.info("Evicting item cache for pristine event : {}", item.getItemSku());
        this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      }
    } else {
      PristineDataItem pristineItem;
      Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
      pristineItem = item.getPristineDataItem();
      pristineItem.setPristineMasterId(pristineMasterId);
      pristineItem.setDefaultProductCode(productCode);
      item.setPristineDataItem(pristineItem);
      pristineItemRepository.updatePristineMasterDPC(pristineItem);
      itemRepository.updatePristineDataItem(storeId, item);
      LOG.info("Updating item pristine data for itemSku : {} , {}", item.getItemSku(), item.getPristineDataItem());
      this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
      this.saveAndPublishService.publishPristineItem(item.getStoreId(), item);
    }
  }

  @Override
  public Map<String, ProductAndItemsVO> getProductAndItemsMap(String storeId,
      List<String> productSkuList) {
    Map<String, ProductAndItemsVO> result = new HashMap<>();
    for(String productSku : productSkuList) {
      try {
        Product product = productService.getProductDeletedOrUndeleted(storeId, productSku);
        List<Item> itemList = this.getItemsByProductSkuFromCacheOrElseDB(storeId, productSku);
        result.put(productSku, new ProductAndItemsVO(product, itemList));
      } catch (Exception e) {
        log.error("Error fetching product and items for productSku : {} ", productSku, e);
      }
    }
    return result;
  }

  private void overrideL4DetailsFromL5(String storeId, List<Item> itemList) {
    for (Item item : itemList) {
      ItemPickupPoint itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(storeId, item.getItemSku());
      objectConverterService
          .overrideL4DetailsFromL5(Collections.singletonList(item), Collections.singletonList(itemPickupPoint));
    }
  }

  @Override
  public List<Item> getItemsByProductSkuFromCacheOrElseDB(String storeId, String productSku) {
    List<Item> itemList = itemCacheHelperService.findItemsByStoreIdAndProductSku(storeId, productSku, false, false,
        false);
    if (CollectionUtils.isEmpty(itemList)) {
      itemList = itemRepository.findItemsByStoreIdAndProductSku(storeId, productSku);
    }
    overrideL4DetailsFromL5(storeId, itemList);
    return itemList;
  }

  @Override
  public ProductAndItemPickupPointDTO updatePickupPoints(String storeId,
      PickupPointUpdateRequest pickupPointUpdateRequest, boolean readFromPrimary) throws Exception {
    log.info("Updating the pickup point for product : {} different Location : {}",
        pickupPointUpdateRequest.getProductSku(), pickupPointUpdateRequest.isDifferentLocation());
    List<Item> updatedItems = new ArrayList<>();
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    Product product = null;
    boolean fbbActive = Objects.nonNull(pickupPointUpdateRequest.getFbbActivated()) ?
    pickupPointUpdateRequest.getFbbActivated() : false;
    Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointsMap = new HashMap<>();
    if (pickupPointUpdateRequest.isDifferentLocation()) {
      for (PickupPointUpdateItemRequest pickupPointUpdateItemRequest : pickupPointUpdateRequest
          .getPickupPointUpdateItemRequestList()) {
        Item item =
          dataSourceWrapperService.findItemByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId,
            pickupPointUpdateItemRequest.getItemSku(), fbbActive, readFromPrimary);
        if (Objects.nonNull(item)) {
          List<ItemPickupPoint> updatedItemPickupPoint = new ArrayList<>();
          updatedItemPickupPoint =
            updatePickupPoint(storeId, item, pickupPointUpdateItemRequest.getPickupPointCode(),
              readFromPrimary);
          updatedItemPickupPoints.addAll(updatedItemPickupPoint);
          updatedItems.add(item);
        }
      }
      if (CollectionUtils.isNotEmpty(updatedItems)) {
        product =
          productAndItemSolrIndexerService.pickupPointCodesUpdateAndSolrPublish(updatedItems, true);
      }
    } else {
      List<Item> items =
        dataSourceWrapperService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
          pickupPointUpdateRequest.getProductSku(), false, fbbActive, readFromPrimary);
      if (CollectionUtils.isNotEmpty(items)) {
        String pickupPoint = pickupPointUpdateRequest.getPickupPointUpdateItemRequestList().get(0).getPickupPointCode();
        for (Item item : items) {
          List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
          updatedItemPickupPointList =
            updatePickupPoint(storeId, item, pickupPoint, readFromPrimary);
          updatedItemPickupPoints.addAll(updatedItemPickupPointList);
          updatedItems.add(item);
        }
        product =
          productAndItemSolrIndexerService.pickupPointCodesUpdateAndSolrPublish(items, false);
      }
    }
      Set<String> distinctProductSkus =
          updatedItems.stream().map(Item::getProductSku).collect(Collectors.toSet());
      if (CollectionUtils.isNotEmpty(distinctProductSkus)) {
        productL3SolrReindexStatusService.insertProductSkusToReindexStatusCollection(storeId,
            CommonUtil.toProductL3SolrReindexStatuses(distinctProductSkus, storeId,
                ProductReindexStatus.REINDEX_PENDING_L3_SOLR_DB));
      }
    return new ProductAndItemPickupPointDTO(product, updatedItemPickupPoints);
  }

  private Item getItemFromItemSku(EditProductDetailDTO editProductDetailDTO,
      PickupPointUpdateItemRequest pickupPointUpdateItemRequest,
      Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointsMap) {
    Item item = CommonUtil.getItemFromDTO(pickupPointUpdateItemRequest.getItemSku(), editProductDetailDTO);
    if (Objects.nonNull(item)) {
      Optional<ItemPickupPoint> itemPickupPointOptional =
          itemSkuAndItemPickupPointsMap.get(item.getItemSku()).stream().filter(not(ItemPickupPoint::isMarkForDelete))
              .findFirst();
      itemPickupPointOptional.ifPresent(itemPickupPoint -> item.setMerchantSku(itemPickupPoint.getMerchantSku()));
    }
    return item;
  }

  public Product pickupPointCodesUpdateForCombineEditRequest(List<Item> currentItems, boolean isDifferentLocation,
      EditProductDetailDTO editProductDetailDTO) {
    List<Item> items = new ArrayList<>();
    if (isDifferentLocation) {
      items =
          editProductDetailDTO.getAllItemMap().values().stream().filter(not(Item::isMarkForDelete)).collect(toList());
      items.addAll(currentItems);
    } else {
      items.addAll(currentItems);
    }
    Product product = editProductDetailDTO.getProduct();
    product.setPickupPointCodes(items.stream().filter(item -> !item.isPermanentDelete()).map(Item::getPickupPointCode)
        .collect(Collectors.toSet()));
    editProductDetailDTO.setProduct(product);
    editProductDetailDTO.setProductUpdated(true);
    return product;
  }

  private List<ItemPickupPoint> updatePickupPointForCombinedRequest(String storeId, Item item, String pickupPointCode,
      List<ItemPickupPoint> itemPickupPointList, EditProductDetailDTO editProductDetailDTO, boolean readFromPrimary)
      throws Exception {
    log.info("Updating pickup point {} for item sku : {}", pickupPointCode, item.getItemSku());
    Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointsMap =
        itemPickupPointList.stream().collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
    checkArgument(StringUtils.isNotBlank(pickupPointCode), ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    ItemPickupPoint itemPickupPoint;
    itemPickupPoint =
        itemSkuAndItemPickupPointsMap.get(item.getItemSku()).stream().filter(not(ItemPickupPoint::isMarkForDelete))
            .findFirst().orElse(null);
    List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>(
        updatePickupPointInItemPickupPoints(pickupPointCode, itemPickupPoint, item, new HashMap<>(), true,
            editProductDetailDTO, readFromPrimary).values());
    updatedItemPickupPointList.forEach(updatedItemPickupPoint -> updatedItemPickupPoint.setNewData(true));
    CommonUtil.setOfflineItemIdToItemPickupPointMap(updatedItemPickupPointList, editProductDetailDTO);
    return updatedItemPickupPointList;
  }

  private List<ItemPickupPoint> updatePickupPoint(String storeId, Item item, String pickupPoint,
      boolean readFromPrimary) throws Exception {
    log.info("Updating pickup point {} for item sku : {}", pickupPoint, item.getItemSku());
    checkArgument(Objects.nonNull(item), ITEM_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(pickupPoint), ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    ItemPickupPoint itemPickupPoint;
    item.setPickupPointCode(pickupPoint);
    itemPickupPoint =
        dataSourceWrapperService.findItemPickupPointByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, item.getItemSku(),
            readFromPrimary).get(0);
    Map<String, ItemPickupPoint> evictItemPickupPointCacheMap =
        updatePickupPointInItemPickupPoints(pickupPoint, itemPickupPoint, item, new HashMap<>(), false, null,
            readFromPrimary);
    List<ItemPickupPoint> savedItemPickupPoints =
        this.itemPickupPointService.saveItemPickupPoint(new ArrayList<>(evictItemPickupPointCacheMap.values()));
    savedItemPickupPoints.forEach(savedItemPickupPoint -> savedItemPickupPoint.setNewData(true));
    saveAndPublishService.publishItemPickupPointDataChangeEvent(savedItemPickupPoints, new ArrayList<>(),
      Collections.EMPTY_MAP);
    evictItemPickupPointCache(evictItemPickupPointCacheMap);
    return savedItemPickupPoints;
  }

  @Override
  public Map<String, ItemPickupPoint> updatePickupPointInItemPickupPoints(String pickupPoint,
      ItemPickupPoint itemPickupPoint, Item item, Map<String, Boolean> pickupPointCodeToPureCNCStatusChangeMap,
      boolean isCombinedEditRequest, EditProductDetailDTO editProductDetailDTO, boolean readFromPrimary) throws Exception {
    ItemPickupPoint newItemPickupPoint = null;
    if (isCombinedEditRequest) {
      newItemPickupPoint = CommonUtil.getItemPickupPointFromDTO(editProductDetailDTO, item.getItemSku(), pickupPoint)
          .orElseGet(
              () -> dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(item.getStoreId(), item.getItemSku(),
                  pickupPoint, readFromPrimary));
    } else {
      newItemPickupPoint =
          dataSourceWrapperService.findItemPickupPointByItemSkuAndPickupPointCode(item.getStoreId(), item.getItemSku(),
              pickupPoint, readFromPrimary);
    }
    Map<String, ItemPickupPoint> evictItemPickupPointCacheMap = new HashMap<>();
    if (Objects.nonNull(newItemPickupPoint) && newItemPickupPoint.isMarkForDelete()) {
      BeanUtils.copyProperties(itemPickupPoint, newItemPickupPoint, ID, VERSION, PRICE, PICKUP_POINT_CODE, CNC_ACTIVE,
          OFFLINE_ITEM_ID, WHOLESALE_PRICE_EXISTS);
      newItemPickupPoint.setPrice(setHigherPrice(itemPickupPoint.getPrice(), newItemPickupPoint.getPrice()));
      String categoryCode = StringUtils.EMPTY;
      if (isCombinedEditRequest) {
        categoryCode = getCategoryCodeForCombinedEditRequest(editProductDetailDTO);
      } else {
        categoryCode = getCategoryCode(itemPickupPoint);
      }
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
      updateOfflineItemPriceRequest.setItemSku(newItemPickupPoint.getItemSku());
      updateOfflineItemPriceRequest.setOfferPrice(
          newItemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
      CommonUtil.deletePickupPointsPromoAndDiscountDetails(Collections.singletonList(newItemPickupPoint));
      if (isPriceChanged(itemPickupPoint.getPrice().stream().findFirst().get(),
          newItemPickupPoint.getPrice().stream().findFirst().get())) {
        validatePromoPriceAndUpdateCampaign(updateOfflineItemPriceRequest, categoryCode, newItemPickupPoint);
      }
      setMarkForDelete(newItemPickupPoint, false);
      setDelivery(newItemPickupPoint, itemPickupPoint.isDelivery());
      newItemPickupPoint.setCncActive(false);
      setFbbFlag(itemPickupPoint, newItemPickupPoint);
      newItemPickupPoint.setItemViewConfig(itemPickupPoint.getAllItemViewConfigs());
      pickupPointCodeToPureCNCStatusChangeMap.put(newItemPickupPoint.getPickupPointCode(), false);
    } else if (Objects.nonNull(newItemPickupPoint)) {
      BeanUtils.copyProperties(itemPickupPoint, newItemPickupPoint, ID, VERSION, PRICE, PICKUP_POINT_CODE, CNC_ACTIVE,
          OFFLINE_ITEM_ID, WHOLESALE_PRICE_EXISTS);
      newItemPickupPoint.setPrice(setHigherPrice(itemPickupPoint.getPrice(), newItemPickupPoint.getPrice()));
      String categoryCode = StringUtils.EMPTY;
      if (isCombinedEditRequest) {
        categoryCode = getCategoryCodeForCombinedEditRequest(editProductDetailDTO);
      } else {
        categoryCode = getCategoryCode(itemPickupPoint);
      }
      UpdateOfflineItemPriceRequest updateOfflineItemPriceRequest = new UpdateOfflineItemPriceRequest();
      updateOfflineItemPriceRequest.setItemSku(newItemPickupPoint.getItemSku());
      updateOfflineItemPriceRequest.setOfferPrice(
          newItemPickupPoint.getPrice().stream().findFirst().get().getOfferPrice());
      CommonUtil.deletePickupPointsPromoAndDiscountDetails(Collections.singletonList(newItemPickupPoint));
      if (isPriceChanged(itemPickupPoint.getPrice().stream().findFirst().get(),
          newItemPickupPoint.getPrice().stream().findFirst().get())) {
        validatePromoPriceAndUpdateCampaign(updateOfflineItemPriceRequest, categoryCode, newItemPickupPoint);
      }
      setDelivery(newItemPickupPoint, itemPickupPoint.isDelivery());
      newItemPickupPoint.setItemViewConfig(itemPickupPoint.getAllItemViewConfigs());
      setFbbFlag(itemPickupPoint, newItemPickupPoint);
      if (item.getItemViewConfigs().isEmpty()) {
        item.setItemViewConfigs(itemPickupPoint.getAllItemViewConfigs());
      }
      pickupPointCodeToPureCNCStatusChangeMap.put(newItemPickupPoint.getPickupPointCode(),
          CommonUtil.isPureCNCStatusChange(item.getItemViewConfigs().iterator().next(),
              newItemPickupPoint.getItemViewConfig().iterator().next(), newItemPickupPoint.isCncActive(),
              newItemPickupPoint.isCncActive()));
    } else {
      newItemPickupPoint = setPickupPoint(itemPickupPoint, pickupPoint);
      setDelivery(newItemPickupPoint, itemPickupPoint.isDelivery());
      newItemPickupPoint.setCncActive(false);
    }
    pickupPointCodeToPureCNCStatusChangeMap.put(itemPickupPoint.getPickupPointCode(), false);
    setDelivery(itemPickupPoint, false);
    if (CommonUtil.isCncActivatedFalse(itemPickupPoint)) {
      setMarkForDelete(itemPickupPoint, true);
      itemPickupPoint.setFbbActivated(false);
    }
    if (Objects.nonNull(newItemPickupPoint)) {
      evictItemPickupPointCacheMap.put(
          CommonUtil.generatePickupPointKey(newItemPickupPoint.getItemSku(), newItemPickupPoint.getPickupPointCode()),
          newItemPickupPoint);
    }
    evictItemPickupPointCacheMap.putIfAbsent(
        CommonUtil.generatePickupPointKey(itemPickupPoint.getItemSku(), itemPickupPoint.getPickupPointCode()),
        itemPickupPoint);
    return evictItemPickupPointCacheMap;
  }

  private void setFbbFlag(ItemPickupPoint itemPickupPoint, ItemPickupPoint newItemPickupPoint) {
    newItemPickupPoint.setFbbActivated(itemPickupPoint.isFbbActivated());
  }

  private boolean isPriceChanged(Price existingPriceSet, Price priceToSet) {
    return !(Double.compare(existingPriceSet.getListPrice(), priceToSet.getListPrice()) == 0);
  }

  private String getCategoryCode(ItemPickupPoint itemPickupPoint) throws Exception {
    String categoryCode = StringUtils.EMPTY;
    Product product =
        productService.findByStoreIdAndProductSku(itemPickupPoint.getStoreId(), itemPickupPoint.getProductSku());
    if (StringUtils.isNotBlank(product.getCategoryCode())) {
      categoryCode = product.getCategoryCode();
    } else {
      ProductDetailResponse result =
          this.masterDataService.getProductDetailFromMasterData(username, requestId, product.getProductCode());
      if (Objects.nonNull(result) && CollectionUtils.isNotEmpty(result.getProductCategoryResponses())) {
        categoryCode = result.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
      }
    }
    return categoryCode;
  }

  public String getCategoryCodeForCombinedEditRequest(EditProductDetailDTO editProductDetailDTO) {
    String categoryCode = StringUtils.EMPTY;
    Product product = editProductDetailDTO.getProduct();
    if (StringUtils.isNotBlank(product.getCategoryCode())) {
      categoryCode = product.getCategoryCode();
    } else {
      ProductDetailResponse productDetailResponse = editProductDetailDTO.getProductDetailResponse();
      if (Objects.nonNull(productDetailResponse) && CollectionUtils.isNotEmpty(
          productDetailResponse.getProductCategoryResponses())) {
        categoryCode = productDetailResponse.getProductCategoryResponses().stream()
            .map(response -> response.getCategory().getCategoryCode()).filter(StringUtils::isNotBlank).findFirst()
            .orElse(StringUtils.EMPTY);
      }
    }
    return categoryCode;
  }

  private void evictItemPickupPointCache(Map<String, ItemPickupPoint> evictItemPickupPointCache) {
    evictItemPickupPointCache.values().forEach(itemPickupPoint -> cacheEvictHelperService
      .evictItemPickupPointData(itemPickupPoint.getStoreId(), itemPickupPoint,
        itemPickupPoint.getPickupPointCode()));
  }

  private void setDelivery(ItemPickupPoint itemPickupPoint, boolean deliveryFlag) {
    itemPickupPoint.setDelivery(deliveryFlag);
  }

  private void setMarkForDelete(ItemPickupPoint itemPickupPoint, boolean mfdFlag) {
    itemPickupPoint.setMarkForDelete(mfdFlag);
  }

  private ItemPickupPoint setPickupPoint(ItemPickupPoint itemPickupPoint, String pickupPoint) {
    ItemPickupPoint newItemPickupPoint = new ItemPickupPoint();
    BeanUtils.copyProperties(itemPickupPoint, newItemPickupPoint, ID, VERSION, CREATED_DATE, CREATED_BY, UPDATED_DATE,
        UPDATED_BY, PICKUP_POINT_CODE, OFFLINE_ITEM_ID, CNC_ACTIVE, WHOLESALE_PRICE_EXISTS);
    newItemPickupPoint.setPrice(itemPickupPoint.getPrice());
    newItemPickupPoint.setItemViewConfig(itemPickupPoint.getAllItemViewConfigs());
    newItemPickupPoint.setMarkForDelete(false);
    newItemPickupPoint.setPickupPointCode(pickupPoint);
    newItemPickupPoint.setFbbActivated(itemPickupPoint.isFbbActivated());
    newItemPickupPoint.setOfflineItemId(CommonUtil.generateOfflineItemId(newItemPickupPoint.getItemSku(), pickupPoint));
    CommonUtil.deletePickupPointsPromoAndDiscountDetails(Collections.singletonList(newItemPickupPoint));
    return newItemPickupPoint;
  }

  private Set<Price> setHigherPrice(Set<Price> existingPriceSet, Set<Price> priceToSet) {
    Price highestPrice = Stream.concat(existingPriceSet.stream(), priceToSet.stream())
        .max(Comparator.comparingDouble(Price::getListPrice)).get();
    return ImmutableSet.of(highestPrice);
  }

  @Override
  public void updateContentChange(String storeId, String productSku, boolean contentChange, boolean publishItems) {
    Set<String> productSkus = new HashSet<>();
    productSkus.add(productSku);
    List<Item> items = itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalseAndIsArchivedFalse(storeId, productSkus);
    checkState(CollectionUtils.isNotEmpty(items), ItemServiceImpl.PRODUCT_NOT_FOUND + productSku);
    List<Item> solrUpdateNeededItems = new ArrayList<>();
    setItemData(contentChange, items, solrUpdateNeededItems);
    if (publishItems) {
      this.saveOperationService.saveItemsWithoutUpdatingSolr(items);
      if (CollectionUtils.isNotEmpty(solrUpdateNeededItems)) {
        this.productAndItemSolrIndexerService.updateSolrOnContentChange(solrUpdateNeededItems);
      }
    } else {
      this.saveOperationService.saveProductAndItemsWithoutPublishingItemChange(new ProductAndItemsVO(null, items));
    }
  }

  private void setItemData(boolean contentChange, List<Item> items, List<Item> solrUpdateNeededItems) {
    for (Item item : items) {
      Set<ItemChangeEventType> itemChangeEventTypes = new LinkedHashSet<>();
      item.setContentChanged(contentChange);
      if (contentChange) {
        if (Objects.nonNull(item.getPristineDataItem())) {
          item.setPristineDataItem(null);
          itemChangeEventTypes.add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
          solrUpdateNeededItems.add(item);
          log.info("Removing the pristine mapping for itemSku {} because of content change");
        }
        itemChangeEventTypes.add(ItemChangeEventType.ITEM_DATA_CHANGE);
      }
      item.setItemChangeEventTypes(new ArrayList<>(itemChangeEventTypes));
      item.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
      item.setUpdatedDate(new Date());
    }
  }

  @Override
  public Map<String, String> findListOfItemCodesByItemSkus(String storeId, Set<String> itemSkus) {
    return itemSkus.stream()
        .map(itemSku -> cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku))
        .filter(Objects::nonNull).collect(toMap(Item::getItemCode, Item::getItemSku, (a, b) -> a));
  }

  @Override
  public Page<Item> getItemsByProductSkusPaginated(String storeId, String productSku, PageRequest pageRequest) {
    List<Item> results = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    List<Item> pageItems = results.stream().sorted(Comparator.comparing(Item::getItemSku))
        .skip(pageRequest.getPageNumber() * pageRequest.getPageSize())
        .limit(pageRequest.getPageSize()).collect(toList());
    Page<Item> items = new PageImpl<>(pageItems,
      PageRequest.of(pageRequest.getPageNumber(), pageRequest.getPageSize()), results.size());
    checkArgument(CollectionUtils.isNotEmpty(items.getContent()),
        ItemServiceImpl.ITEM_NOT_FOUND_FOR_PRODUCT_WITH_PRODUCT_SKU);
    return items;
  }

  @Override
  public Item findByStoreIdAndItemSkuFromCache(String storeId, String itemSku) {
    return itemCacheHelperService
        .findItemByStoreIdAndItemSku(storeId, itemSku, Boolean.TRUE, Boolean.TRUE, false, null, false);
  }

  public List<Item> activateItemsOnNeedCorrection(String storeId, String productSku,
      List<NeedCorrectionItemActivationRequest> itemActivationRequests) throws Exception {
    List<Item> listOfItems = this.getItemsByProductSkuFromCacheOrElseDB(storeId, productSku);
    Map<String, NeedCorrectionItemActivationRequest> itemActivationRequestMap = new HashMap<>(
        itemActivationRequests.stream()
            .collect(toMap(NeedCorrectionItemActivationRequest::getItemSku, Function.identity())));
    for (Item item : listOfItems) {
      NeedCorrectionItemActivationRequest itemActivationRequest = itemActivationRequestMap.get(item.getItemSku());
      if (Objects.nonNull(itemActivationRequest)) {
        Map<String, ItemPickupPoint> updateItemPickupPoint = new HashMap<>();
        ItemPickupPoint itemPickupPoint =
            itemPickupPointService.findByItemSkuAndDelivery(item.getStoreId(), item.getItemSku());
        boolean isShippingChanged =
            objectConverterService.getItemForActivation(item, itemActivationRequest, itemPickupPoint);
        if (isShippingChanged) {
          updateItemPickupPoint =
              updatePickupPointInItemPickupPoints(item.getPickupPointCode(), itemPickupPoint, item, new HashMap<>(),
                  false, null, false);
        }
        List<ItemPickupPoint> updatedItemPickupPoints = MapUtils.isEmpty(updateItemPickupPoint) ?
            Collections.singletonList(itemPickupPoint) :
            new ArrayList<>(updateItemPickupPoint.values());
        updatedItemPickupPoints.forEach(
            updatedItemPickupPoint -> updatedItemPickupPoint.setMerchantSku(item.getMerchantSku()));
        this.itemPickupPointService.saveItemPickupPoint(updatedItemPickupPoints);
      }
    }
    return listOfItems;
  }

  @Override
  public EditItemResponse activateItemsOnNeedCorrectionWithMpp(String storeId, String productSku, String merchantCode,
      List<NeedCorrectionItemActivationRequest> itemActivationRequests, boolean isFreeSampleOrOfflineProduct) throws Exception {
    List<ItemPickupPoint> newlyAddedL5s = new ArrayList<>();
    boolean addOrDeletePPPerformed = false;
    Map<String, Map<String, NeedCorrectionItemActivationRequest>> itemSkuPickPointRequestMap =
        getItemSkuPickPointNeedCorrectionItemActivationRequestMap(itemActivationRequests);
    List<Item> listOfItems = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    listOfItems = listOfItems.stream().filter(not(Item::isPermanentDelete)).collect(toList());
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    boolean pureCNCStatusChange = false;
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList = new ArrayList<>();
    List<Pair<String, BundleRecipeRequest>> bundleRecipeRequestPairList = new ArrayList<>();
    List<Item> bundleItems = new ArrayList<>();
    Map<String, BusinessPartnerPickupPoint> pickupPointMap = new HashMap<>();
    List<String> pickupPointCodeRequests =
        itemActivationRequests.stream().map(NeedCorrectionItemActivationRequest::getPickupPointCode)
            .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(pickupPointCodeRequests)) {
      List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
          businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
              pickupPointCodeRequests);
      if (CollectionUtils.isNotEmpty(businessPartnerPickupPoints)) {
        pickupPointMap.putAll(businessPartnerPickupPoints.stream()
            .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, Function.identity())));
      }
    }
    for (Item item : listOfItems) {
      List<ItemPickupPoint> itemPickupPoints = itemPickupPointService.findByStoreIdAndItemSku(storeId, item.getItemSku());
      ItemPickupPoint deliveryPickupPoint =
          itemPickupPoints.stream().filter(ItemPickupPoint::isDelivery).findFirst().orElse(null);
      if (Objects.nonNull(deliveryPickupPoint)) {
        objectConverterService.overrideL4DetailsFromL5(Collections.singletonList(item),
            Collections.singletonList(deliveryPickupPoint));
      }
      Set<String> pickupPointCodes =
          itemPickupPoints.stream().map(ItemPickupPoint::getPickupPointCode).collect(Collectors.toSet());
      pickupPointCodes.removeAll(pickupPointMap.keySet());
      if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
        List<BusinessPartnerPickupPoint> businessPartnerPickupPoints =
            businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId,
                new ArrayList<>(pickupPointCodes));
        if (CollectionUtils.isNotEmpty(businessPartnerPickupPoints)) {
          pickupPointMap.putAll(businessPartnerPickupPoints.stream()
              .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, Function.identity())));
        }
      }
      boolean cncActivatedAtL4 = false;
      for (ItemPickupPoint itemPickupPoint : itemPickupPoints) {
        ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel = null;
        boolean isPriceChanged = false;
        boolean isDiscoverableChanged = false;
        boolean isCncDiscoverableChanged = false;
        if(itemSkuPickPointRequestMap.containsKey(itemPickupPoint.getItemSku())) {
          NeedCorrectionItemActivationRequest request = itemSkuPickPointRequestMap.get(itemPickupPoint.getItemSku())
                  .getOrDefault(itemPickupPoint.getPickupPointCode(), null);
          if (Objects.nonNull(request)) {
            if (!cncForWarehouseFeatureSwitch) {
              pureCNCStatusChange = CommonUtil.isPureCNCStatusChange(
                  ItemViewConfig.builder().isDiscoverable(request.isDiscoverable()).isBuyable(request.isBuyable())
                      .build(), itemPickupPoint.getItemViewConfig().iterator().next(), request.isCncActive(),
                  itemPickupPoint.isCncActive());
            }
            isPriceChanged = CommonUtil.isPriceChanged(request.getListPrice(),
                itemPickupPoint.getPrice().iterator().next().getListPrice(), request.getOfferPrice(),
                itemPickupPoint.getPrice().iterator().next().getOfferPrice());

            isDiscoverableChanged = CommonUtil.isDiscoverableChanged(
                ItemViewConfig.builder().isDiscoverable(request.isDiscoverable())
                    .isBuyable(request.isBuyable()).build(),
                itemPickupPoint.getSingleItemViewConfigByChannel(Constants.DEFAULT));

            if (cncForWarehouseFeatureSwitch) {
              isCncDiscoverableChanged = CommonUtil.isDiscoverableChanged(
                  ItemViewConfig.builder().isDiscoverable(request.isCncDiscoverable())
                      .isBuyable(request.isCncBuyable()).build(),
                  itemPickupPoint.getSingleItemViewConfigByChannel(Constants.CNC));
            }

            itemActivationRequests.remove(request);
            itemPickupPoint =
                objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(request, itemPickupPoint,
                    pickupPointMap.getOrDefault(request.getPickupPointCode(), new BusinessPartnerPickupPoint()));
            itemPickupPoint.setNewData(Boolean.TRUE);
            itemPickupPointDataChangeEventModel =
                objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, pureCNCStatusChange);
            List<ItemPickupPointChangeEventType> itemPickupPointChangeEventTypes =
                itemPickupPointDataChangeEventModel.getItemPickupPointChangeEventTypes();
            boolean isOffline = !request.isBuyable() && !request.isDiscoverable();
            if ((isFreeSampleOrOfflineProduct || !isOffline)
              && clearScheduleForNeedRevision) {
              CommonUtil.clearSchedules(itemPickupPoint, itemPickupPointChangeEventTypes);
            }
            if (isDiscoverableChanged) {
              itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE);
            }
            if (isCncDiscoverableChanged) {
              itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.CNC_DISCOVERABLE_FLAG_CHANGE);
            }
            if (isPriceChanged) {
              itemPickupPoint.setPriceUpdatedDate(new Date());
              itemPickupPointChangeEventTypes.add(ItemPickupPointChangeEventType.PRICE_CHANGE);
            }
            itemPickupPointDataChangeEventModel.setItemPickupPointChangeEventTypes(itemPickupPointChangeEventTypes);
            if (Objects.nonNull(request.getBundleRecipe()))
              setBundleRecipeAndAddItemToBundleList(item, request, bundleRecipeRequestPairList, bundleItems);
            item.setMerchantSku(request.getMerchantSku());
          } else {
            addOrDeletePPPerformed = true;
            itemPickupPoint.setMarkForDelete(true);
            itemPickupPointDataChangeEventModel =
                objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
          }
        } else {
          itemPickupPoint.setMarkForDelete(true);
          itemPickupPointDataChangeEventModel =
              objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
        }
        itemPickupPointDataChangeEventModelList.add(itemPickupPointDataChangeEventModel);
        if (!cncActivatedAtL4) {
          cncActivatedAtL4 = !itemPickupPoint.isMarkForDelete() && isCncActive(itemPickupPoint);
        }
        updatedItemPickupPoints.add(itemPickupPoint);
        item.setMarkForDelete(false);
        item.setForceReview(false);
        item.setArchived(item.isArchivedBeforeEdit());
      }
      item.setCncActivated(cncActivatedAtL4);
    }

    updateRecipeForSharedProducts(storeId, bundleItems);

    for (NeedCorrectionItemActivationRequest request : itemActivationRequests) {
      updateL5ForNeedCorrectionActivation(storeId, productSku, merchantCode, request,
        itemSkuPickPointRequestMap, itemPickupPointDataChangeEventModelList, newlyAddedL5s,
        updatedItemPickupPoints, pickupPointMap.getOrDefault(request.getPickupPointCode(), new BusinessPartnerPickupPoint()));
    }
    List<ItemPickupPoint> allUpdatedPickupPoints =
        itemPickupPointService.saveItemPickupPoint(updatedItemPickupPoints, listOfItems);
    itemPickupPointDataChangeEventModelList.forEach(
        itemPickupPointDataChangeEventModel -> itemPickupPointService.publishItemPickupPointDataChangeEventWithPureCncStatusChange(
            itemPickupPointDataChangeEventModel, Collections.EMPTY_MAP));
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setUpdatedItems(listOfItems);
    editItemResponse.setUpdatedItemPickupPoints(newlyAddedL5s);
    if (CollectionUtils.isNotEmpty(newlyAddedL5s)) {
      addOrDeletePPPerformed = true;
    }
    editItemResponse.setAddOrDeletePPPerformed(addOrDeletePPPerformed);
    editItemResponse.setAllUpdatedItemPickupPoints(allUpdatedPickupPoints);
    return editItemResponse;
  }

  private boolean isCncActive(ItemPickupPoint itemPickupPoint) {
    ItemViewConfig cncViewConfig =
        itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(Constants.CNC);
    return cncViewConfig.isDiscoverable() || cncViewConfig.isBuyable();
  }

  private void updateL5ForNeedCorrectionActivation(String storeId, String productSku, String merchantCode,
    NeedCorrectionItemActivationRequest request,
    Map<String, Map<String, NeedCorrectionItemActivationRequest>> itemSkuPickPointRequestMap,
    List<ItemPickupPointDataChangeEventModel> itemPickupPointDataChangeEventModelList,
    List<ItemPickupPoint> newlyAddedL5s, List<ItemPickupPoint> updatedItemPickupPoints, BusinessPartnerPickupPoint businessPartnerPickupPoint) {
    ItemPickupPoint itemPickupPoint = new ItemPickupPoint();
    Map<String, NeedCorrectionItemActivationRequest> itemSkuAndL5Map =
        itemSkuPickPointRequestMap.getOrDefault(request.getItemSku(), new HashMap<>());
    if (itemSkuAndL5Map.size() == 1) {
      itemPickupPoint.setDelivery(true);
    }
    itemPickupPoint.setMarkForDelete(false);
    itemPickupPoint.setNewData(true);
    itemPickupPoint.setItemSku(request.getItemSku());
    itemPickupPoint.setPickupPointCode(request.getPickupPointCode());
    itemPickupPoint.setMerchantCode(merchantCode);
    itemPickupPoint.setStoreId(storeId);
    itemPickupPoint.setProductSku(productSku);
    itemPickupPoint.setOfflineItemId(CommonUtil.generateOfflineItemId(itemPickupPoint.getItemSku(), request.getPickupPointCode()));
    itemPickupPoint = objectConverterService.updateItemPickupPointOnNeedCorrectionActivation(
      request, itemPickupPoint, businessPartnerPickupPoint);
    ItemPickupPointDataChangeEventModel itemPickupPointDataChangeEventModel =
        objectConverterService.convertToItemPickupPointChangeEventModel(itemPickupPoint, false);
    itemPickupPointDataChangeEventModelList.add(itemPickupPointDataChangeEventModel);
    newlyAddedL5s.add(itemPickupPoint);
    updatedItemPickupPoints.add(itemPickupPoint);
  }

  private static void setBundleRecipeAndAddItemToBundleList(Item item, NeedCorrectionItemActivationRequest request,
    List<Pair<String, BundleRecipeRequest>> bundleRecipeRequestPairList, List<Item> bundleItems) {
    Set<BundleRecipe> bundleRecipe = new HashSet<>();
    for (BundleRecipeVo bundleRecipeVo : request.getBundleRecipe()) {
      BundleRecipe bundleRecipeObject = new BundleRecipe();
      BeanUtils.copyProperties(bundleRecipeVo, bundleRecipeObject);
      bundleRecipe.add(bundleRecipeObject);
    }
    bundleRecipeRequestPairList.add(
        Pair.of(item.getItemCode(), new BundleRecipeRequest(item.getItemSku(), request.getBundleRecipe())));
    item.setBundleRecipe(bundleRecipe);
    bundleItems.add(item);
  }

  private Map<String, Map<String, NeedCorrectionItemActivationRequest>> getItemSkuPickPointNeedCorrectionItemActivationRequestMap(
      List<NeedCorrectionItemActivationRequest> itemActivationRequests) {
    Map<String, Map<String, NeedCorrectionItemActivationRequest>> itemSkuPickPointRequestMap = new HashMap<>();
    for (NeedCorrectionItemActivationRequest itemActivationRequest : itemActivationRequests){
      Map<String, NeedCorrectionItemActivationRequest> pickPointRequestMap = itemSkuPickPointRequestMap
          .getOrDefault(itemActivationRequest.getItemSku(), new HashMap<>());
      pickPointRequestMap.put(itemActivationRequest.getPickupPointCode(), itemActivationRequest);
      itemSkuPickPointRequestMap.put(itemActivationRequest.getItemSku(), pickPointRequestMap);
    }
    return itemSkuPickPointRequestMap;
  }

  @Override
  public List<Item> getItemsByStoreIdAndItemSkus(String storeId, Set<String> itemSkus) {
    return CollectionUtils.isNotEmpty(itemSkus) ?
        this.itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, itemSkus) :
        Collections.emptyList();
  }

  @Override
  public List<Item> getItemsByStoreIdAndProductSkusOrItemSkusIn(String storeId, List<String> productSkus,
      List<String> itemSkus) {
    if (CollectionUtils.isNotEmpty(productSkus)) {
      return this.itemRepository.findItemsByStoreIdAndProductSkuIn(storeId, new HashSet<>(productSkus));
    } else {
      return this.itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, new HashSet<>(itemSkus));
    }
  }

  @Override
  public List<Item> getItemsByItemSkus(String storeId, Set<String> itemSkus, String[] fields) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!itemSkus.isEmpty(), CommonConstants.ITEM_SKU_SET_MUST_NOT_BE_EMPTY);
    checkArgument(fields != null, CommonConstants.FIELDS_MUST_NOT_BE_NULL);
    return this.itemRepository.findByStoreIdAndItemSkus(storeId, itemSkus, fields);
  }

  @Override
  public List<Item> getItemsByOfflineItemIds(String storeId,
    List<String> itemSkusForOfflineItemSearch, boolean setOfflineItemData) {
    Map<String, ItemPickupPoint> itemSkuAndOfflineItemMap = Optional.ofNullable(
         this.itemPickupPointService.findByStoreIdAndOfflineItemIdListAndMarkForDeleteFalse(storeId,
          itemSkusForOfflineItemSearch)).orElse(new ArrayList<>()).stream()
      .collect(toMap(ItemPickupPoint::getItemSku, Function.identity()));
    if (!Integer.valueOf(itemSkusForOfflineItemSearch.size())
        .equals(itemSkuAndOfflineItemMap.keySet().size())) {
      log.warn("Offline item not found. Offline item requested : {}, OfflineItems found : {}",
          itemSkusForOfflineItemSearch,
          itemSkuAndOfflineItemMap.values().stream().map(ItemPickupPoint::getOfflineItemId)
              .collect(Collectors.toList()));
    }
    List<Item> itemFromOfflineItemSku = Optional.ofNullable(this.getItemsByStoreIdAndItemSkus(storeId,
        itemSkuAndOfflineItemMap.keySet())).orElse(new ArrayList<>());
    if (!Integer.valueOf(itemFromOfflineItemSku.size())
        .equals(itemSkuAndOfflineItemMap.keySet().size())) {
      log.warn("Items not found. Requested items : {}, items found : {}",
          itemSkuAndOfflineItemMap.entrySet(),
          itemFromOfflineItemSku.stream().map(Item::getItemSku).collect(Collectors.toList()));
    }
    if (setOfflineItemData) {
      itemFromOfflineItemSku.forEach(item -> this.productHelperService.constructOfflineItem(item,
        CommonUtil.getOfflineItemByPickupPoint(itemSkuAndOfflineItemMap.get(item.getItemSku()),
          false, null)));
    } else {
      itemFromOfflineItemSku.forEach(
        item -> this.productHelperService.constructOfflineItemForTransaction(item,
          itemSkuAndOfflineItemMap.get(item.getItemSku())));
    }
    return itemFromOfflineItemSku;
  }

  @Override
  public List<ItemCodeDetailResponse> getItemDetailsByItemCodes(String storeId, Set<String> itemCodes) {
    List<Item> itemDetailsByItemCodes = itemRepository.getItemDetailsByItemCodes(itemCodes);
    Set<String> merchantCodeList =
        itemDetailsByItemCodes.stream().map(Item::getMerchantCode).collect(Collectors.toSet());
    List<BusinessPartner> businessPartnerList = businessPartnerService
        .findByStoreIdAndBusinessPartnerCodes(storeId, new SimpleListStringRequest(new ArrayList<>(merchantCodeList)));
    Map<String, String> businessPartnerCodeAndTypeMap = businessPartnerList.stream().filter(Objects::nonNull).collect(
        toMap(BusinessPartner::getBusinessPartnerCode, BusinessPartner::getMerchantType, (a, b) -> a));
    return getItemCodeDetailResponses(itemDetailsByItemCodes, businessPartnerCodeAndTypeMap);
  }

  private List<ItemCodeDetailResponse> getItemCodeDetailResponses(List<Item> itemDetailsByItemCodes,
      Map<String, String> businessPartnerCodeAndTypeMap) {
    List<ItemCodeDetailResponse> itemCodeDetailResponses = new ArrayList<>();
    for (Item item : itemDetailsByItemCodes) {
      ItemCodeDetailResponse itemCodeDetailResponse = new ItemCodeDetailResponse();
      itemCodeDetailResponse.setItemCode(item.getItemCode());
      itemCodeDetailResponse.setMerchantCode(item.getMerchantCode());
      itemCodeDetailResponse.setProductSku(item.getProductSku());
      itemCodeDetailResponse.setMerchantType(businessPartnerCodeAndTypeMap.get(item.getMerchantCode()));
      itemCodeDetailResponses.add(itemCodeDetailResponse);
    }
    return itemCodeDetailResponses;
  }

  @Override
  public void updateMerchantDiscountPriceAndPublishEvent(String storeId, String itemSku, DiscountPrice discountPrice,
      boolean activated) throws Exception {
    Item item = itemCacheHelperService
        .findItemByStoreIdAndItemSku(storeId, itemSku, Boolean.TRUE, Boolean.FALSE, false, null, false);
    GdnPreconditions.checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    item.getPrice().forEach(price -> price.setMerchantPromoDiscountPrice(discountPrice));
    this.updateItemMerchantDiscountPrice(storeId, item);
  }

  @Override
  public void updateProductTypeOrContentChange(ProductTypeEditRequest request, String storeId) {
    if (request.isContentChanged() && Objects.isNull(request.getProductType())) {
      updateContentChange(storeId, request.getProductSku(), request.isContentChanged(), request.isContentChanged());
    }
    if (Objects.nonNull(request.getProductType())) {
      List<Product> products = productService.findByStoreIdAndProductCode(storeId, request.getProductCode());
      if (products.stream().anyMatch(Product::isSynchronized)) {
        for (Product product : products) {
          product.setProductType(request.getProductType());
          saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
          List<Item> items =
              cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
          List<Item> contentChangedItems = new ArrayList<>();
          if (request.isContentChanged()) {
            setItemData(request.isContentChanged(), items, contentChangedItems);
            this.saveOperationService.saveItemsWithoutUpdatingSolr(items);
          }
          productAndItemSolrIndexerService.updateSolrOnProductTypeChange(items, request.getProductType());
        }
      }
    }
  }

  @Override
  public void activeDeactivatePromoBundling(
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel) {
    if (promoBundlingActivatedDeactivatedEventModel.isAllStore()) {
      businessPartnerPromoService.upsertBusinessPartnerPromo(promoBundlingActivatedDeactivatedEventModel.getStoreId(),
          promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(),
          promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
          promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
    } else {
      Boolean wholesalePriceActivated =
          Constants.WHOLESALE_PRICE.equals(promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType()) ?
              promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated() :
              null;
      promoBundlingActivatedDeactivatedEventModel.getItemSkus().stream().forEach(itemSku -> {
        try {
          processPromoBundlingStatusChangedEvent(promoBundlingActivatedDeactivatedEventModel.getStoreId(), itemSku,
              promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
              promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(), wholesalePriceActivated);
        } catch (Exception e) {
          log.error("Exception caught on processing processPromoBundlingStatusChangedEvent itemSku :{}, " + "error - ",
              itemSku, e);
        }
      });
    }
  }

  @Override
  public void activeDeactivatePromoBundlingInItemPickupPoint(
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel) {
    if (promoBundlingActivatedDeactivatedEventModel.isAllStore()) {
      businessPartnerPromoService.upsertBusinessPartnerPromo(promoBundlingActivatedDeactivatedEventModel.getStoreId(),
          promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(),
          promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
          promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
    } else {
      Boolean wholesalePriceActivated =
          Constants.WHOLESALE_PRICE.equals(promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType()) ?
              promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated() :
              null;
      promoBundlingActivatedDeactivatedEventModel.getItemSkus().stream().forEach(
          itemSku -> processPromoBundlingStatusChangedEventInItemPickupPoint(
              promoBundlingActivatedDeactivatedEventModel.getStoreId(), itemSku,
              promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
              promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(), wholesalePriceActivated));
    }
  }

  @Override
  public void activeDeactivatePromoBundlingInItemPickupPointByPPCode(
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel) {
    if (promoBundlingActivatedDeactivatedEventModel.isAllStore()) {
      businessPartnerPromoService.upsertBusinessPartnerPromo(promoBundlingActivatedDeactivatedEventModel.getStoreId(),
          promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(),
          promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
          promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
    } else {
      Boolean wholesalePriceActivated =
          Constants.WHOLESALE_PRICE.equals(promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType()) ?
              promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated() :
              null;
      List<ItemPickupPoint> updatedItemPickupPoints =
          promoBundlingActivatedDeactivatedEventModel.getItemInfoList().stream().map(
              itemInfo -> processPromoBundlingStatusChangedEventInItemPickupPointAndPPCode(
                  promoBundlingActivatedDeactivatedEventModel.getStoreId(), itemInfo.getItemSku(),
                  promoBundlingActivatedDeactivatedEventModel.getPromoBundlingType(),
                  promoBundlingActivatedDeactivatedEventModel.isPromoBundlingActivated(), wholesalePriceActivated,
                  itemInfo.getPickupPointCode())).filter(Objects::nonNull).collect(Collectors.toList());

      if (CollectionUtils.isNotEmpty(updatedItemPickupPoints)) {
        productL3SolrService.updatePromoOrWholesaleItemSkusByItemPickupPoint(updatedItemPickupPoints);
        updatedItemPickupPoints.forEach(
            itemPickupPoint -> cacheEvictItemService.evictFindL5ByItemSku(itemPickupPoint.getStoreId(),
                itemPickupPoint.getItemSku()));
      }
    }
  }

  @Override
  public void processAdjustmentProductChangeEventAtL5Level(AdjustmentProductChange adjustmentProductChange) {
    SystemParameter adjustmentProductChangeEventListenSwitch =
        systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
            SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT);
    if (Boolean.valueOf(adjustmentProductChangeEventListenSwitch.getValue())) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
              adjustmentProductChange.getProductSku(), adjustmentProductChange.getPickupPointCode());
      GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint),
          ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
      if (adjustmentProductChange.isActivated()) {
        itemPickupPoint.getPrice().forEach(price -> price.setListOfDiscountPrices(Arrays.asList(
            new DiscountPrice(adjustmentProductChange.getValue(), adjustmentProductChange.getStartDate(),
                adjustmentProductChange.getEndDate(), adjustmentProductChange.getAdjustmentName(),
                AdjustmentType.BLIBLI, adjustmentProductChange.getCampaignCode()))));
      } else {
        itemPickupPoint.getPrice().forEach(price -> price.setListOfDiscountPrices(Collections.emptyList()));
      }
      this.itemPickupPointService.updateDiscountPriceInItemPickupPointByItemSkuAndPickupPointCode(itemPickupPoint);
      Map<String, List<String>> productSkuMap = new HashMap<>();
      productSkuMap.put(itemPickupPoint.getProductSku(), new ArrayList<>(Arrays.asList(itemPickupPoint.getItemSku())));
      Map<String, String> productSkuToMerchantCodeMap = new HashMap<>();
      productSkuToMerchantCodeMap.put(itemPickupPoint.getProductSku(), itemPickupPoint.getMerchantCode());
      this.productAndItemSolrIndexerService.updateSolrOnPromoFlagChangeByItemSkus(productSkuMap,
          adjustmentProductChange.isActivated(), null, productSkuToMerchantCodeMap);
    }
  }

  @Override
  public void saveItems(List<Item> items) {
    if (CollectionUtils.isNotEmpty(items)) {
      itemRepository.saveAll(items);
    }
  }

  @Override
  public List<Item> findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(String storeId, Set<String> itemSkus) {
    List<Item> items = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(itemSkus)) {
      items = itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus);
    }
    return items;
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuInOrItemSkuInAndMarkForDeleteFalse(String storeId,
      List<String> productSkus, List<String> itemSkus) {
    if (CollectionUtils.isNotEmpty(productSkus)) {
      return itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDelete(storeId, new HashSet<>(productSkus),
          false);
    } else {
      return itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, new HashSet<>(itemSkus));
    }
  }

  @Override
  public Page<ItemBasicL4Response> getL4ItemListByProductSkuLite(Set<String> productSkus, String storeId, Integer page,
      Integer size) {
    checkArgument(CollectionUtils.isNotEmpty(productSkus), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(productSkus.size() <= Integer.parseInt(
        systemParameterService.findValueByStoreIdAndVariable(storeId,
            SystemParameterNames.PRODUCT_SKU_LIST_LIMIT_FOR_BASIC_DETAILS).getValue()), SKU_SIZE_LIMIT_EXCEEDED);
    Pageable pageable = PageRequest.of(page, size);
    Page<Item> itemPage =
        itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus, pageable);
    List<ItemBasicL4Response> itemBasicL4Responses = convertToItemBasicL4(itemPage.getContent());
    return new PageImpl<>(itemBasicL4Responses, PageRequest.of(pageable.getPageNumber(), pageable.getPageSize()),
        itemPage.getTotalElements());
  }

  private List<ItemBasicL4Response> convertToItemBasicL4(List<Item> itemList) {
    return itemList.stream().map(
        item -> ItemBasicL4Response.builder().itemName(item.getGeneratedItemName()).itemSku(item.getItemSku())
            .productSku(item.getProductSku()).upcCode(item.getUpcCode()).build()).toList();
  }

  @Override
  public Page<ItemLevel4ListingResponse> getL4ItemListByProductSku(Set<String> productSkus,
    String storeId, Integer page, Integer size) throws Exception {
    checkArgument(Objects.nonNull(productSkus), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(productSkus.size() <= Integer.parseInt(
      systemParameterService.findValueByStoreIdAndVariable(storeId,
        SystemParameterNames.PRODUCT_SKU_LIST_LIMIT).getValue()), SKU_SIZE_LIMIT_EXCEEDED);
    Pageable pageable = null;
    List<ItemLevel4ListingResponse> listingResponses = new ArrayList<>();
    Page<Item> items = null;
    List<Item> itemList = new ArrayList<>();
    Map<String, Boolean> productSkuAndSharedProductMap;
    if (CollectionUtils.isNotEmpty(productSkus)) {
      if (Objects.isNull(size)) {
        itemList = itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDelete(storeId, productSkus,false);
        pageable = PageRequest.of(page, itemList.size(), Sort.Direction.ASC, ITEM_SKU);
        items = convertListToPage(itemList, pageable);
      } else {
        pageable = PageRequest.of(page, size, Sort.Direction.ASC, ITEM_SKU);
        items = itemRepository.findItemsByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus, pageable);
      }

    }
    if (Objects.nonNull(items) && items.hasContent()) {
      List<ItemPickupPoint> itemPickupPointByProductSkus =
          itemPickupPointService.findItemPickupPointByProductSkus(storeId, new ArrayList<>(productSkus));
      productSkuAndSharedProductMap = getProductCodeAndSharedProductMap(items.getContent());
      listingResponses = objectConverterService.convertItemToItemLevel4SummaryResponse(items.getContent(), requestId,
          itemPickupPointByProductSkus, productSkuAndSharedProductMap);
    }
    log.info("Item Set as received from DB was not empty");
    if (CollectionUtils.isNotEmpty(listingResponses)) {
      return new PageImpl<>(listingResponses, PageRequest.of(pageable.getPageNumber(), pageable.getPageSize()),
          items.getTotalElements());
    } else {
      log.error("Error fetching the paginated Response for the list with size: {} ",
        listingResponses.size());
      return new PageImpl<>(new ArrayList<>());
    }
  }

  @Override
  public Map<String, Boolean> getProductCodeAndSharedProductMap(Collection<Item> items) {
    Map<String, Boolean> productSkuAndSharedProductMap = new HashMap<>();

    Set<String> productSkus = new HashSet<>();
    Set<String> parentProductSku = items.stream().map(Item::getProductSku).collect(Collectors.toSet());
    Set<String> bundleChildProductSkus = new HashSet<>();

    Set<String> bundleChildItemSkus =
        items.stream().map(Item::getBundleRecipe).filter(CollectionUtils::isNotEmpty).flatMap(Collection::stream)
            .map(BundleRecipe::getItemSku).collect(Collectors.toSet());
    if (CollectionUtils.isNotEmpty(bundleChildItemSkus)) {
      List<Item> bundleChildItems =
          itemRepository.findItemsByStoreIdAndItemSkuIn(GdnMandatoryRequestParameterUtil.getStoreId(),
              bundleChildItemSkus);
      bundleChildProductSkus = bundleChildItems.stream().map(Item::getProductSku).collect(Collectors.toSet());
    }

    productSkus.addAll(parentProductSku);
    productSkus.addAll(bundleChildProductSkus);
    List<Product> products = productService.getAllProducts(GdnMandatoryRequestParameterUtil.getStoreId(), productSkus, false);
    Map<String, List<Product>> productCodeProductSkuMap =
        products.stream().filter(product -> StringUtils.isNotBlank(product.getProductCode()))
            .collect(Collectors.groupingBy(Product::getProductCode));

    Map<String, Boolean> productCodeAndSharedProductMap =
        productService.getProductCodeAndSharedProductMap(GdnMandatoryRequestParameterUtil.getStoreId(),
            productCodeProductSkuMap.keySet());
    for (Map.Entry<String, Boolean> entry : productCodeAndSharedProductMap.entrySet()) {
      for (Product product : productCodeProductSkuMap.get(entry.getKey())) {
        productSkuAndSharedProductMap.put(product.getProductSku(), entry.getValue());
      }
    }

    return productSkuAndSharedProductMap;
  }

  @Override
  public boolean existsRecordForStoreIdAndProductSkuAndCncActivated(String storeId,
      String productSku, boolean cncActivated) {
    checkArgument(Objects.nonNull(productSku), ErrorMessages.PRODUCT_SKU_EMPTY);
    return itemRepository.existsByStoreIdAndProductSkuAndCncActivated(storeId, productSku, cncActivated);
  }

  @Override
  public List<Item> fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndArchivedFalseAndMarkForDeleteFalse(
      String storeId, String upcCode, Set<String> merchantCodes) {
    return itemRepository.fetchItemsByStoreIdAndUpcCodeAndMerchantCodesAndMarkForDeleteAndIsArchived(
        storeId, upcCode, merchantCodes, false, false);
  }

  public Page<Item> convertListToPage(List<Item> itemList, Pageable pageable) {
    int start = (int) pageable.getOffset();
    int end = Math.min((start + pageable.getPageSize()), itemList.size());

    return new PageImpl<>(itemList.subList(start, end), pageable, itemList.size());
  }


  @Override
  public List<ItemLevel5Response> getL5ItemListByProductSku(String storeId, List<String> productSkus,
      boolean fetchProductData, List<String> pickupPointCodes, List<String> promoTypes, boolean fetchB2bData,
      boolean fetchCategoryData, String fetchViewConfigByChannel, boolean excludeDistributionPickupPoint)
      throws Exception {
    checkArgument(CollectionUtils.isNotEmpty(productSkus), ErrorMessages.PRODUCT_SKU_EMPTY);
    List<ItemLevel5Response> listingResponses = new ArrayList<>();
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointList =
        itemPickupPointService.findItemPickupPointByProductSkusAndPickupPointCodes(storeId, productSkus,
            pickupPointCodes);
    if (excludeDistributionPickupPoint) {
      itemPickupPointList = itemPickupPointList.stream().filter(Predicate.not(ItemPickupPoint::isDistribution))
          .collect(Collectors.toList());
    }
    if(CollectionUtils.isNotEmpty(itemPickupPointList)) {
      Set<String> itemSkus = itemPickupPointList.stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet());
      items = itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, itemSkus);
    }
    List<Product> productList = new ArrayList<>();
    if (fetchProductData) {
      for (String productSku : productSkus) {
        Product product =
            productCacheableService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
        if(Objects.nonNull(product)) {
          productList.add(product);
        }
      }
    }
    Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache = new HashMap<>();
    if (fetchCategoryData) {
      Set<String> categoryCodes = Optional.ofNullable(items).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
          .map(item -> Optional.ofNullable(item.getCategoryCode()).orElse(StringUtils.EMPTY))
          .collect(Collectors.toSet());
      parentCategoriesFromDbAndCache =
          cachedService.getParentCategoriesFromDbAndCache(GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), categoryCodes);
    }
    if (CollectionUtils.isNotEmpty(itemPickupPointList) && CollectionUtils.isNotEmpty(items)) {
      listingResponses =
          objectConverterService.convertItemToItemLevel5Response(storeId, itemPickupPointList, items, productList,
              fetchB2bData, parentCategoriesFromDbAndCache, cncForWarehouseFeatureSwitch, fetchViewConfigByChannel);
    }
    return listingResponses;
  }

  @Override
  public List<ItemPriceResponse> getAllItemSkuByItemCodeAndPickupPointCode(String storeId, String itemCode,
      String pickupPointCode, boolean fetchAll) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemCode), ITEM_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    List<Item> itemList =
        itemCacheHelperService.findAllItemByStoreIdAndItemCodeAndMarkForDeleteFalse(storeId, itemCode);

    if (CollectionUtils.isEmpty(itemList)) {
      return new ArrayList<>();
    }

    if (Objects.nonNull(itemList.get(0).getPristineDataItem())) {
      itemList = this.itemCacheHelperService.getItemsByPristineId(storeId,
          itemList.get(0).getPristineDataItem().getPristineId(), false);

    }

    Map<String, ItemPickupPoint> itemPickupPointMap =
        getItemPickupPointByItemSkusAndPickupPointCode(storeId, itemList, pickupPointCode);
    Map<String, Set<Price>> priceMap = this.itemPriceService.getDiscountItemPickupPoint(itemPickupPointMap.values());

    if (!fetchAll) {
      itemList = itemList.stream()
          .filter(item -> isItemPickupPointBuyableAndDiscoverable(itemPickupPointMap.get(item.getItemSku())))
          .collect(toList());
    }

    return ResponseHelper.toItemPriceResponse(pickupPointCode, itemList, itemPickupPointMap, priceMap);
  }

  @Override
  public List<ItemPriceResponse> getAllItemSkuByPristineIdAndPickupPointCode(String storeId, String pristineId,
      String pickupPointCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pristineId), ErrorMessages.PRISTINE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointCode), PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    List<Item> itemList = this.itemCacheHelperService.getItemsByPristineId(storeId, pristineId, false);

    if (CollectionUtils.isEmpty(itemList)) {
      return new ArrayList<>();
    }

    Map<String, ItemPickupPoint> itemPickupPointMap =
        getItemPickupPointByItemSkusAndPickupPointCode(storeId, itemList, pickupPointCode);
    Map<String, Set<Price>> priceMap = this.itemPriceService.getDiscountItemPickupPoint(itemPickupPointMap.values());
    itemList = itemList.stream()
        .filter(item -> isItemPickupPointBuyableAndDiscoverable(itemPickupPointMap.get(item.getItemSku())))
        .collect(toList());

    return ResponseHelper.toItemPriceResponse(pickupPointCode, itemList, itemPickupPointMap, priceMap);
  }

  private Map<String, ItemPickupPoint> getItemPickupPointByItemSkusAndPickupPointCode(String storeId,
      List<Item> itemList, String pickupPointCode) {
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.getItemPickupPointByItemSkuInAndPickupPointCode(storeId,
            ProductAndItemsUtil.getItemSkuFromItem(itemList), pickupPointCode);
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    return itemPickupPointMap;
  }

  private boolean isItemPickupPointBuyableAndDiscoverable(ItemPickupPoint itemPickupPoint) {
    if (Objects.nonNull(itemPickupPoint)) {
      ItemViewConfig itemViewConfig =
          Optional.ofNullable(itemPickupPoint.getItemViewConfig()).orElse(new HashSet<>()).stream().findFirst()
              .orElseGet(ItemViewConfig::new);
      return (CommonUtil.getBuyableFromConfig(itemViewConfig) && CommonUtil.getDiscoverableFromConfig(itemViewConfig));
    }
    return Boolean.FALSE;
  }

  /**
   * Add items to ItemPriceVo list without filtering which are buyable as well as discoverable
   *
   * @param items list of items to add in itemPriceVOS
   * @return Item Price VO list
   */
  private List<ItemPriceVO> addItemToItemWithoutFilteringBuyableAndDiscoverablePriceVOList(List<Item> items,
      String pickupPointCode) {
    List<ItemPriceVO> list = new ArrayList<>();
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndPickupPointCode(item.getStoreId(), item.getItemSku(), pickupPointCode);
      if (Objects.isNull(itemPickupPoint)) {
        continue;
      }
      ItemPriceVO itemPriceVO =
          new ItemPriceVO.ItemPriceBuilder().setItemSku(item.getItemSku()).setBuyable(isBuyable(itemPickupPoint))
              .setMerchantCode(item.getMerchantCode()).build();
      this.setDefaultPrice(itemPickupPoint, itemPriceVO);
      list.add(itemPriceVO);
    }
    return list;
  }

  /**
   * Add items to ItemPriceVo list which are buyable as well as discoverable
   *
   * @param items list of items to add in itemPriceVOS
   * @return Item Price VO list
   */
  private List<ItemPriceVO> addItemToItemPriceVOList(List<Item> items, String pickupPointCode) {
    List<ItemPriceVO> itemPriceVOS = new ArrayList<>();
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndPickupPointCode(item.getStoreId(), item.getItemSku(), pickupPointCode);
      if (Objects.nonNull(itemPickupPoint) && isBuyableDiscoverable(itemPickupPoint)) {
        ItemPriceVO itemPriceVO = new ItemPriceVO.ItemPriceBuilder().setItemSku(item.getItemSku()).setBuyable(true)
            .setMerchantCode(item.getMerchantCode()).build();
        this.setDefaultPrice(itemPickupPoint, itemPriceVO);
        itemPriceVOS.add(itemPriceVO);
      }
    }
    return itemPriceVOS;
  }

  @Override
  public Set<String> getItemSkuByItemNameKeyword(String storeId, String productSku, String keyword) {
    List<Item> items = itemRepository.getItemSkuByItemNameKeyword(storeId, productSku, keyword);
    return Optional.ofNullable(items).orElse(new ArrayList<>()).stream().map(Item::getItemSku)
        .collect(Collectors.toSet());
  }

  @Override
  public Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(String storeId, String productSku,
      boolean cncActivated) {
    return itemRepository
        .countByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(storeId, productSku, cncActivated, false);
  }

  @Override
  public Long findCountByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivatedReadFromPrimary(String storeId,
      String productSku, boolean cncActivated) {
    return itemRepository.countByStoreIdAndProductSkuAndMarkForDeleteFalseAndCncActivated(storeId, productSku,
        cncActivated, true);
  }

  @Override
  public Set<String> getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(String storeId, String merchantCode,
      Set<String> itemCodes, String keyword) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(merchantCode), ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemCodes) || StringUtils.isNotBlank(keyword),
        ErrorMessages.ITEM_CODE_OR_KEYWORD_MUST_NOT_BE_BLANK);
    List<Item> items =
        itemRepository.getItemSkuByMerchantCodeAndItemCodeAndItemNameKeyword(storeId, merchantCode, itemCodes, keyword);
    return Optional.ofNullable(items).orElse(new ArrayList<>()).stream().map(Item::getItemSku)
        .collect(Collectors.toSet());
  }

  @Override
  public void publishUpdateToSolrEvent(Product product, List<Item> itemList) {
    saveAndPublishService.publishSolrUpdateEvent(Collections.singletonList(
        objectConverterService.convertToProductAndItemEventModel(new ProductAndItemsVO(product, itemList))));
  }

  @Override
  public void updateItemDimensionsAndUpcCode(String itemCode, double length, double width, double weight, double height,
      List<String> eanUpcCodes) {
    if (validateDimensionSwitch) {
      length = validateFieldForPositiveValues(length);
      width = validateFieldForPositiveValues(width);
      weight = validateFieldForPositiveValues(weight);
      height = validateFieldForPositiveValues(height);
      if(invalidDimension(length, width, weight, height)){
      log.error("Error processing DimensionUpdateListener, invalid dimension for itemCode : {}", itemCode);
      return;
    }
    }
    List<Item> items = this.getItemsByStoreIdAndItemCodeAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, itemCode);
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    items = items.stream().filter(Item::isSynchronized).collect(toList());
    if (CollectionUtils.isNotEmpty(items)) {
      String categoryCode = items.get(0).getCategoryCode();
      double shippingWeight =
          productService.generateShippingWeight(Constants.DEFAULT_STORE_ID, categoryCode, length, width, height,
              weight);
      Set<String> productSkuSet = new HashSet<>();
      for (Item item : items) {
        boolean dimensionsOrEanUpcCodeChanged =
            setDimensionsAndShippingWeightAndEanUpcCodes(length, width, weight, height, shippingWeight, item, auditTrailDtoList,
                Optional.ofNullable(eanUpcCodes).orElse(new ArrayList<>()).stream().findFirst()
                    .orElse(StringUtils.EMPTY));
        if (dimensionsOrEanUpcCodeChanged) {
          productSkuSet.add(item.getProductSku());
            AuditTrailListResponse auditTrailListResponse =
                new AuditTrailListResponse(auditTrailDtoList, Constants.DEFAULT, Constants.WAREHOUSE_USERNAME,
                    Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, true);
          auditTrailListResponse.setUpdateDirectlyToDB(true);
          log.info("Publishing the event {} to update the Dimensions/Ean UPC for product {} ",
              ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, item.getProductSku());
          kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
        }
      }
      updateMissingFieldsAtProductLevel(productSkuSet);
      saveOperationService.saveItemsWithoutUpdatingSolr(items);
    }
  }

  private void updateMissingFieldsAtProductLevel(Set<String> productSkuSet) {
    if (CollectionUtils.isNotEmpty(productSkuSet)) {
      if (updateProductInDimensionEvent) {
        for (String productSku : productSkuSet) {
          Product product = productService.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, productSku);
          updateProductOnDimensionChange(product);
        }
      }
    }
  }

  private void updateProductOnDimensionChange(Product product) {
    if (Objects.nonNull(product) && (Boolean.TRUE.equals(product.getDimensionsMissing()) || (
        CollectionUtils.isNotEmpty(product.getMissingFields()) && product.getMissingFields()
            .contains(Constants.DIMENSIONS_MISSING)))) {
      product.setDimensionsMissing(false);
      Set<String> missingFields =
          Optional.ofNullable(new HashSet<>(product.getMissingFields())).orElse(new HashSet<>());
      missingFields.remove(Constants.DIMENSIONS_MISSING);
      product.setMissingFields(missingFields);
      productService.saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY);
    }
  }

  private double validateFieldForPositiveValues(double value) {
    double roundedValue = Math.round(value * 1000.0) / 1000.0;
    if(roundedValue <= 0) {
      return 0.0;
    } else {
      return roundedValue;
    }
  }

  private boolean invalidDimension(double length, double width, double weight, double height) {
    double[] dimensions = {length, width, weight, height};
    return Arrays.stream(dimensions).anyMatch(dimension -> dimension <= MINIMUM_DIMENSION_ALLOWED);
  }

  private boolean setDimensionsAndShippingWeightAndEanUpcCodes(double length, double width, double weight, double height,
      double shippingWeight, Item item, List<AuditTrailDto> auditTrailDtoList, String eanUpc) {
    boolean dimensionsOrEanUpcCodeChanged = false;
    if (length != item.getLength()) {
      auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), Constants.LENGTH,
          Double.toString(item.getLength()), Double.toString(length), null, item.getProductSku(),
          item.getGeneratedItemName(), item.getPickupPointCode(), true));
      item.setLength(length);
      dimensionsOrEanUpcCodeChanged = true;
    }
    if (width != item.getWidth()) {
      auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), Constants.WIDTH,
          Double.toString(item.getWidth()), Double.toString(width), null, item.getProductSku(),
          item.getGeneratedItemName(), item.getPickupPointCode(), true));
      item.setWidth(width);
      dimensionsOrEanUpcCodeChanged = true;
    }
    if (height != item.getHeight()) {
      auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), Constants.HEIGHT,
          Double.toString(item.getHeight()), Double.toString(height), null, item.getProductSku(),
          item.getGeneratedItemName(), item.getPickupPointCode(), true));
      item.setHeight(height);
      dimensionsOrEanUpcCodeChanged = true;
    }
    if (weight != item.getWeight()) {
      auditTrailDtoList.add(new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), Constants.WEIGHT,
          Double.toString(item.getWeight()), Double.toString(weight), null, item.getProductSku(),
          item.getGeneratedItemName(), item.getPickupPointCode(), true));
      item.setWeight(weight);
      dimensionsOrEanUpcCodeChanged = true;
    }
    if (shippingWeight != item.getShippingWeight()) {
      auditTrailDtoList.add(
          new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), Constants.SHIPPING_WEIGHT_CHANGE,
              Double.toString(item.getShippingWeight()), Double.toString(shippingWeight), null, item.getProductSku(),
              item.getGeneratedItemName(), item.getPickupPointCode(), true));
      item.setShippingWeight(shippingWeight);
      dimensionsOrEanUpcCodeChanged = true;
    }
    if (StringUtils.isNotBlank(eanUpc) && !item.getUpcCode().equals(eanUpc)) {
      auditTrailDtoList.add(
          new AuditTrailDto(item.getMerchantCode(), item.getItemSku(), Constants.UPC_CODE_CHANGE, item.getUpcCode(),
              eanUpc, null, item.getProductSku(), item.getGeneratedItemName(), item.getPickupPointCode(), true));
      item.setUpcCode(eanUpc);
      dimensionsOrEanUpcCodeChanged=true;
    }
    return dimensionsOrEanUpcCodeChanged;
  }

  @Override
  public List<Item> getItemsByStoreIdAndItemSkusAndMarkForDeleteFalse(String storeId, Set<String> itemSkus) {
    return CollectionUtils.isNotEmpty(itemSkus) ?
        this.itemRepository.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkus) :
        Collections.emptyList();
  }

  @Override
  public Item getOneItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(String storeId,
      String merchantCode, String merchantSku) {
    return itemRepository.findFirstItemByStoreIdAndMerchantCodeAndMerchantSkuAndMarkForDeleteFalse(
        storeId, merchantCode, merchantSku);
  }

  @Override
  public Item getOneItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(String storeId,
      String merchantCode, List<String> productSku, String merchantSku) {
    return itemRepository.findFirstItemByStoreIdAndMerchantCodeAndProductSkuInAndMerchantSku(
        storeId, merchantCode, productSku, merchantSku);
  }

  public BasicItemDTO getbasicItemDetails(String storeId, String itemSku, String pickupPointCode) {
    ItemPickupPoint itemPickupPoint = itemPickupPointService
        .findByStoreIdAndItemSkuAndPickupPointCodeAndMarkForDeleteFalse(storeId, itemSku, pickupPointCode);
    GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPoint), ErrorMessages.ITEM_PICKUP_POINT_MUST_NOT_BE_NULL);
    Item item = cacheItemHelperService.findCacheableByStoreIdAndItemSkuAndMarkForDeleteFalse(storeId, itemSku);
    GdnPreconditions.checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
    return ResponseHelper.toBasicItemDTO(item, itemPickupPoint);
  }

  @Override
  public List<Item> findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(String storeId,
    Set<String> itemCodes) {
    return itemRepository.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(storeId, itemCodes);
  }

  @Override
  public ItemAndItemPickupPointVo getItemAndPickupPointDetailsForCnc(String storeId, List<String> productSkus,
    List<String> l5IdList, Boolean cncActivated, Integer page, Integer pageSize) {
    return this.itemCacheHelperService.findItemAndItemPickPointByproductSkusAndCncActive(storeId,
        productSkus, l5IdList, cncActivated, page, pageSize);
  }

  private void setActivePromoBundlings(String storeId, ItemAndItemPickupPointVo itemAndItemPickupPointVo){
    HashMap<String, Item> itemSkuAndItemMap = new HashMap<>();
    itemAndItemPickupPointVo.getItem().forEach(item -> itemSkuAndItemMap.put(item.getItemSku(), item));
    itemAndItemPickupPointVo.getItemPickupPoints()
        .forEach(itemPickupPoint -> setActivePromoBundlingsByPristineOrItemCode(storeId,
            itemSkuAndItemMap.get(itemPickupPoint.getItemSku()), itemPickupPoint));
  }

  @Override
  public ItemAndItemPickupPointVo getAllItemAndPickupPointDetailsWithoutPagination(String storeId,
      List<String> productSkus, List<String> l5Idlist) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productSkus)) {
      itemPickupPoints =
          itemPickupPointRepository.findByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus);
    } else {
      checkArgument(CollectionUtils.isNotEmpty(l5Idlist), ErrorMessages.OFFLINE_ITEM_ID_MUST_NOT_BE_BLANK);
      itemPickupPoints =
          itemPickupPointRepository.findByStoreIdAndOfflineItemIdInAndMarkForDeleteFalse(storeId, l5Idlist);
    }
    if (CollectionUtils.isEmpty(itemPickupPoints)) {
      return null;
    }
    List<Item> items = findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId,
        itemPickupPoints.stream().map(ItemPickupPoint::getItemSku).collect(Collectors.toSet()));
    checkArgument(CollectionUtils.isNotEmpty(items), CommonConstants.ITEMS_NOT_FOUND);
    ItemAndItemPickupPointVo itemAndItemPickupPointVo =
        new ItemAndItemPickupPointVo(items, itemPickupPoints, itemPickupPoints.size());
    setActivePromoBundlings(storeId, itemAndItemPickupPointVo);
    return itemAndItemPickupPointVo;
  }

  @Override
  public void updateMasterSku(MasterSkuMappingEventModel masterSkuMapping) {
    Item item = findByStoreIdAndItemSku(masterSkuMapping.getStoreId(), masterSkuMapping.getItemSku());
    if (Objects.isNull(item)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, ApiErrorCode.ITEM_NOT_FOUND.getCode());
    }
    if (checkIfMasterSkuChanged && StringUtils.equals(masterSkuMapping.getMasterItemSku(), item.getMasterSku())) {
      return;
    }
    item.setMasterSku(masterSkuMapping.getMasterItemSku());
    item.setItemChangeEventTypes(Arrays.asList(ItemChangeEventType.MASTER_SKU_UPDATE));
    saveAndPublishService.saveItems(Collections.singletonList(item));
    cacheEvictHelperService.evictItemCache(item.getStoreId(), item);
  }


  @Override
  public void addRecipeToOtherSisterProductForSharedProduct(String storeId,
      List<Pair<String, BundleRecipeRequest>> bundleRecipeRequestPairList, boolean readFromPrimary) {
    try {
      if (CollectionUtils.isNotEmpty(bundleRecipeRequestPairList)) {
        List<Item> items = itemRepository.findByStoreIdAndItemCodeInAndMarkForDeleteFalse(storeId,
            bundleRecipeRequestPairList.stream().map(Pair::getLeft).collect(Collectors.toSet()), readFromPrimary);
        Set<String> updatedProductSkus = new HashSet<>();
        List<Item> updatedItems = new ArrayList<>();

        if (CollectionUtils.isNotEmpty(items)) {
          Map<String, List<Item>> itemGroupByItemCode =
              items.stream().collect(Collectors.groupingBy(Item::getItemCode));
          itemGroupByItemCode =
              itemGroupByItemCode.entrySet().stream().filter(entry -> entry.getValue().size() > Constants.ONE)
                  .collect(Collectors.toMap(Entry::getKey, Entry::getValue));

          for (Pair<String, BundleRecipeRequest> bundleRecipeRequestPair : bundleRecipeRequestPairList) {
            if (itemGroupByItemCode.containsKey(bundleRecipeRequestPair.getLeft())) {
              List<Item> itemList = itemGroupByItemCode.get(bundleRecipeRequestPair.getLeft());
              itemList.removeIf(item -> item.getItemSku().equals(bundleRecipeRequestPair.getRight().getItemSku()));
              for (Item item : itemList) {
                item.setBundleRecipe(toBundleRecipe(bundleRecipeRequestPair.getRight().getBundleRecipe()));
                updatedItems.add(item);
                updatedProductSkus.add(item.getProductSku());
              }
            }
          }
        }

        if (CollectionUtils.isNotEmpty(updatedItems)) {
          List<Product> products = productService.getAllProducts(storeId, updatedProductSkus, readFromPrimary);
          products = products.stream().filter(Product::isTradingProduct).collect(Collectors.toList());
          Set<String> tdProductSkus = products.stream().map(Product::getProductSku).collect(Collectors.toSet());
          updatedItems =
              updatedItems.stream().filter(item -> tdProductSkus.contains(item.getProductSku())).collect(toList());

          if (CollectionUtils.isNotEmpty(updatedItems)) {
            saveOperationService.saveAndEvictItems(updatedItems);
            for (Product product : products) {
              product.setBundleProduct(true);
              saveOperationService.saveProduct(product);
            }
          }
        }

      }
    } catch (Exception e) {
      log.error("Error while adding recipe to sister products : request : {} ", bundleRecipeRequestPairList, e);
    }
  }

  private Set<BundleRecipe> toBundleRecipe(Set<BundleRecipeVo> bundleRecipeVoSet) {
    Set<BundleRecipe> bundleRecipeSet = new HashSet<>();
    for (BundleRecipeVo bundleRecipeVo : bundleRecipeVoSet) {
      BundleRecipe bundleRecipe = new BundleRecipe();
      BeanUtils.copyProperties(bundleRecipeVo, bundleRecipe);
      bundleRecipeSet.add(bundleRecipe);
    }
    return bundleRecipeSet;
  }

  @Override
  public List<Item> findItemsByStoreIdAndProductSkuAndMarkForDeleteReadFromPrimary(String storeId, String productSku,
      boolean markForDelete) {
    return itemRepository.findItemsByStoreIdAndProductSkuAndMarkForDelete(storeId, productSku, markForDelete, true);
  }

  @Override
  public List<SharedProductBundleRecipeResponse> getBundleRecipeForSharedItems(String storeId, Set<String> itemCodes) {
    // get items by itemCodes
    List<Item> items = itemRepository.findByStoreIdAndItemCodeIn(storeId, itemCodes);
    List<Item> bundleRecipeItems = new ArrayList<>();

    // get bundle recipe items
    if (CollectionUtils.isNotEmpty(items)) {
      Set<String> bundleRecipeItemSkus =
          items.stream().map(Item::getBundleRecipe).filter(CollectionUtils::isNotEmpty).flatMap(Collection::stream)
              .map(BundleRecipe::getItemSku).collect(Collectors.toSet());
      if (CollectionUtils.isNotEmpty(bundleRecipeItemSkus)) {
        bundleRecipeItems = itemRepository.findItemsByStoreIdAndItemSkuIn(storeId, bundleRecipeItemSkus);
      }
    }

    // convert to response
    return ResponseHelper.toSharedProductBundleRecipeResponse(items, bundleRecipeItems);
  }

  @Override
  public void updateRecipeForSharedProducts(String storeId, List<Item> bundleItems) throws Exception {
    Set<String> itemSkus = Optional.ofNullable(bundleItems).orElse(new ArrayList<>()).stream().map(Item::getItemSku)
        .collect(Collectors.toSet());
    Set<String> itemCodes = Optional.ofNullable(bundleItems).orElse(new ArrayList<>()).stream().map(Item::getItemCode)
        .collect(Collectors.toSet());
    Map<String, Set<BundleRecipe>> itemCodeAndBundleRecipeMap =
        bundleItems.stream().collect(Collectors.toMap(Item::getItemCode, Item::getBundleRecipe, (v1, v2) -> v1));

    if (CollectionUtils.isNotEmpty(itemCodes)) {
      List<Item> items = itemRepository.findByStoreIdAndItemCodeIn(storeId, itemCodes);
      List<Item> updatedItems = updateBundleRecipeForSharedItems(itemSkus, itemCodeAndBundleRecipeMap, items);
      updateBundleFlagForSharedProducts(storeId, updatedItems);
    }
  }

  private void updateBundleFlagForSharedProducts(String storeId, List<Item> updatedItems) throws Exception {
    if (CollectionUtils.isNotEmpty(updatedItems)) {
      Set<String> productSkus = updatedItems.stream().map(Item::getProductSku).collect(Collectors.toSet());
      List<Product> products = productService.getAllProducts(storeId, productSkus, true);
      products = products.stream().filter(Product::isTradingProduct).collect(toList());
      Set<String> tdProductSkus = products.stream().map(Product::getProductSku).collect(Collectors.toSet());
      updatedItems =
          updatedItems.stream().filter(item -> tdProductSkus.contains(item.getProductSku())).collect(toList());
      if (CollectionUtils.isNotEmpty(updatedItems)) {
        saveOperationService.saveItemsWithoutUpdatingSolr(updatedItems);
        saveAndPublishService.publishProductBundleOneToOneMappingEvent(updatedItems);
        setBundleFlagAtProductLevel(products);
      }
    }
  }

  private void setBundleFlagAtProductLevel(List<Product> products) {
    for (Product product : Optional.ofNullable(products).orElse(new ArrayList<>())) {
      product.setBundleProduct(true);
      saveOperationService.saveProductWithoutUpdatingSolr(product, new ArrayList<>(), StringUtils.EMPTY, Collections.EMPTY_MAP);
      Map<String, Object> fieldsAndValues = Map.of(SolrFieldNames.BUNDLE_PRODUCT,
          Collections.singletonMap(SolrConstants.SET_CLAUSE, product.isBundleProduct()));
      log.info("Publishing event : {}, product-sku : {} and fieldsAndValues : {}",
          ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), fieldsAndValues);
      kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(),
          new ProductAndItemEventModel(product.getProductSku(), fieldsAndValues, product.getMerchantCode()));
    }
  }

  private List<Item> updateBundleRecipeForSharedItems(Set<String> itemSkus,
      Map<String, Set<BundleRecipe>> itemCodeAndBundleRecipeMap, List<Item> items) {
    List<Item> updatedItems = new ArrayList<>();
    for (Item item : Optional.ofNullable(items).orElse(new ArrayList<>())) {
      if (!itemSkus.contains(item.getItemSku()) && itemCodeAndBundleRecipeMap.containsKey(item.getItemCode())
          && !Optional.ofNullable(item.getBundleRecipe()).orElse(new HashSet<>())
          .equals(itemCodeAndBundleRecipeMap.get(item.getItemCode()))) {
        item.setBundleRecipe(itemCodeAndBundleRecipeMap.get(item.getItemCode()));
        updatedItems.add(item);
      }
    }
    return updatedItems;
  }

  @Override
  public List<UpcStatusResponse> fetchUpcCodeStatus(String storeId, String merchantCode,
      Set<String> upcCodes) {
    return upcCodes.stream().map(upcCode -> UpcStatusResponse.builder().upcCode(upcCode).exists(
        itemRepository.existsByStoreIdAndUpcCodeAndMerchantCodeAndMarkForDeleteFalseAndIsArchivedFalse(
            storeId, upcCode, merchantCode)).build()).toList();
  }

}
