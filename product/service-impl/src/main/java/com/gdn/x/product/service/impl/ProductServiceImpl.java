package com.gdn.x.product.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.common.base.GdnPreconditions.checkState;
import static java.util.stream.Collectors.toMap;

import java.util.ArrayList;
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
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.product.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.domain.event.model.VideoCompressionEventModel;
import com.gdn.x.product.model.entity.Video;
import com.gdn.x.product.model.vo.BulkDownloadProductBasicInfoResponse;
import com.gdn.x.product.model.vo.ProductBasicInfoResponse;
import com.gdn.x.product.rest.web.model.request.OmniChannelSkuUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductLevel3SummaryRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuResponse;
import com.gdn.x.product.model.vo.ReelProductListingRequestVo;
import com.gdn.x.product.rest.web.model.request.ReelProductListingRequest;
import com.gdn.x.product.rest.web.model.response.PromoEligibilityResponse;
import com.gdn.x.product.rest.web.model.response.ReelProductDetailResponse;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import com.gdn.x.productcategorybase.dto.VideoDTO;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.jetbrains.annotations.Nullable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnObjects;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.constants.CommonConstants;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.dao.api.ItemRepository;
import com.gdn.x.product.dao.api.ProductRepository;
import com.gdn.x.product.dao.solr.api.ProductAndItemSolrRepository;
import com.gdn.x.product.dao.solr.api.ProductSolrRepository;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.enums.ItemPickupPointChangeEventType;
import com.gdn.x.product.domain.event.enums.ProductChangeEventType;
import com.gdn.x.product.domain.event.model.CategoryProductSkuMappingRequest;
import com.gdn.x.product.domain.event.model.ExternalSearchReindexToSolrEventModel;
import com.gdn.x.product.domain.event.model.HalalHistoryUpdateEventModel;
import com.gdn.x.product.domain.event.model.ItemCacheClearModel;
import com.gdn.x.product.domain.event.model.ProductChange;
import com.gdn.x.product.domain.event.model.ProductPreOrderStatus;
import com.gdn.x.product.domain.event.model.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.x.product.enums.ChannelName;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.enums.DescriptiveAttributeValueType;
import com.gdn.x.product.enums.ItemChangeEventType;
import com.gdn.x.product.enums.MasterDataAttributeType;
import com.gdn.x.product.enums.ProductCenterActivity;
import com.gdn.x.product.enums.ProductFieldNames;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.enums.SolrConstants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.exception.ApiErrorCodes;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.entity.BundleRecipe;
import com.gdn.x.product.model.entity.BusinessPartner;
import com.gdn.x.product.model.entity.BusinessPartnerPickupPoint;
import com.gdn.x.product.model.entity.BusinessPartnerPromo;
import com.gdn.x.product.model.entity.Category;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
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
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductAttribute;
import com.gdn.x.product.model.entity.ProductAttributeDetail;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.model.entity.ProductScore;
import com.gdn.x.product.model.entity.ProductSpecialAttribute;
import com.gdn.x.product.model.entity.SalesCatalog;
import com.gdn.x.product.model.entity.SalesCategorySequence;
import com.gdn.x.product.model.entity.SystemParameter;
import com.gdn.x.product.model.solr.ProductSolr;
import com.gdn.x.product.model.vo.AddProductAndItemsResponseVo;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.model.vo.ItemCatalogVO;
import com.gdn.x.product.model.vo.ItemVo;
import com.gdn.x.product.model.vo.MasterCategoryResponse;
import com.gdn.x.product.model.vo.MasterDataCacheVo;
import com.gdn.x.product.model.vo.MasterDataProductAndItemsVO;
import com.gdn.x.product.model.vo.OfflineItemHistoryDetailVO;
import com.gdn.x.product.model.vo.PreOrderVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.ProductCenterSummaryRequest;
import com.gdn.x.product.model.vo.ProductCenterSummaryResponse;
import com.gdn.x.product.model.vo.ProductCollectionsVo;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.model.vo.ProductScoreVo;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.model.vo.ProductSkuSummaryRequestVo;
import com.gdn.x.product.model.vo.ProductSummaryRequestVo;
import com.gdn.x.product.model.vo.UnmappedSkuResponse;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.outbound.api.WarehouseItemMasterOutbound;
import com.gdn.x.product.outbound.api.XbpOutbound;
import com.gdn.x.product.outbound.helper.ResponseHelper;
import com.gdn.x.product.rest.web.model.ActivateNeedRevisionResponse;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.B2bFieldsDTO;
import com.gdn.x.product.rest.web.model.dto.DiscountPriceDTO;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataItemImageDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductScoreRetryDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.enums.EditChangeType;
import com.gdn.x.product.rest.web.model.request.AddDeleteVariantRetryRequest;
import com.gdn.x.product.rest.web.model.request.AttributeScoreRequest;
import com.gdn.x.product.rest.web.model.request.CategoryBrandRequest;
import com.gdn.x.product.rest.web.model.request.ItemScoreRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.NeedCorrectionProductActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductBundleCreationRequest;
import com.gdn.x.product.rest.web.model.request.ProductScoreRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryMappingUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SalesCategoryUpdateRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.EditProductDetailDTO;
import com.gdn.x.product.rest.web.model.response.ItemL4SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductNameSuggestionResponse;
import com.gdn.x.product.rest.web.model.response.ProductPickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.ProductScoreResponse;
import com.gdn.x.product.rest.web.model.response.ProductSkuSummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductTypeResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;
import com.gdn.x.product.rest.web.model.response.SkuCodeBundleRecipeResponse;
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
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPriceService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.MasterDataAttributeService;
import com.gdn.x.product.service.api.MasterDataConstructorService;
import com.gdn.x.product.service.api.MasterDataService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PCBMasterDataService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductCenterHistoryService;
import com.gdn.x.product.service.api.ProductHelperService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductScoreHistoryL3Service;
import com.gdn.x.product.service.api.ProductSearchHelperService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.SaveAndPublishService;
import com.gdn.x.product.service.api.SaveOperationService;
import com.gdn.x.product.service.api.SkuValidator;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.product.service.config.KafkaPublisher;
import com.gdn.x.product.service.interceptor.MandatoryParameterHelper;
import com.gdn.x.product.service.util.AddDeleteVariantReconcileUtil;
import com.gdn.x.product.service.util.CommonUtil;
import com.gdn.x.product.service.util.ProductScoreUtil;
import com.gdn.x.product.service.util.ValidationUtil;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.enums.UpdatedFields;
import com.google.api.services.youtube.YouTube;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.Lists;


import lombok.extern.slf4j.Slf4j;


@Slf4j
@Service
public class ProductServiceImpl implements ProductService {

  private static final String SKU_SIZE_LIMIT_EXCEEDED = "sku size limit exceeded";

  private static final Logger LOG = LoggerFactory.getLogger(ProductServiceImpl.class);

  private static final String SHIPPING_WEIGHT_FIELD =
      ProductFieldNames.MASTER_DATA_PRODUCT + "." + ProductFieldNames.SHIPPING_WEIGHT;

  private static final String NO_MASTER_DATA_PRODUCT_SPECIFIED = "No master data specified for this product";

  private static final String INVALID_PRODUCT_SKU_FORMAT = "Invalid product sku format : ";

  private static final String INVALID_ITEM_SKU_FORMAT = "Invalid item sku format : ";

  private static final String PRODUCT_CODE_NOT_FOUND_ON_MASTER_DATA_WITH_PRODUCT_CODE =
      "product code not found on master data with product code ";

  private static final String ITEMS_MUST_NOT_BE_NULL_OR_EMPTY = "items must not be null or empty";

  private static final String MERCHANT_CODE_MUST_NOT_BE_BLANK = "merchant code must not be blank";

  private static final String THIS_MERCHANT_ALREADY_HAVE_THIS_PRODUCT_CODE =
      "This merchant already have this product code";

  private static final String BOTH_SALES_CATALOG_AND_NEW_CATEGORY_CODE_MUST_NOT_BE_NULL =
      "Both sales catalog and new category code must not be null";

  private static final String CATALOG_CODE_MUST_NOT_BE_BLANK = "catalog code must not be blank";

  private static final String BOTH_OLD_AND_NEW_CATEGORY_CODE_SHOULD_NOT_BE_NULL =
      "both old and new category code should not be null";

  private static final String OLD_CATEGORY_NOT_FOUND = "old category not found on product";

  private static final String CATEGORY_NOT_FOUND_ON_PCB = "category not found";

  private static final String STORE_ID_MUST_NOT_BE_BLANK = "storeId must not be blank";

  private static final String PRODUCT_SKU_SET_MUST_NOT_BE_EMPTY = "productSku set must not be empty";

  private static final String FIELDS_MUST_NOT_BE_NULL = "fields must not be null";

  private static final String USERNAME_MUST_NOT_BE_BLANK = "username must not be blank";

  private static final String REQUEST_ID_MUST_NOT_BE_BLANK = "requestId must not be blank";

  private static final String PRODUCT_MUST_NOT_BE_NULL = "product must not be null";

  private static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";

  private static final String PRODUCT_SKU_MUST_NOT_BE_BLANK = "productSku must not be blank";
  private static final String PRODUCT_TYPE_MUST_NOT_BE_BLANK = "product type must not be null";
  private static final String CATEGORY_CODE_MUST_NOT_BE_BLANK = "categoryCode must not be blank";

  private static final String PRODUCT_ATTRIBUTE_CANNOT_BE_NULL = "salesCatalog cannot be null";

  private static final String SALES_CATALOG_CANNOT_BE_NULL = "salesCatalog cannot be null";

  private static final String MASTER_CATALOG_CANNOT_BE_NULL = "masterCatalog cannot be null";

  private static final String SALES_CATALOG_NOT_FOUND = "salesCatalog not found";

  private static final String PRODUCT_NOT_FOUND = "Product not found";

  private static final String CHANGE_SYNC_STATUS_FAILED = "change sync status failed, invalid product status";

  private static final String FETCH_SALES_CATALOG_FAILED = "Failed to fetch sales categories for given product";

  public static final String ITEM_NOT_FOUND = "item not found";
  private static final String ITEM_PICKUP_POINT_NOT_FOUND = "item pickup point not found";

  private static final String PRODUCT_DETAIL_RESPONSE_NOT_FOUND = "Product Detail Response not found";

  public static final String CHANNEL_NOT_FOUND = "channel not found";

  public static final String COMPLETED = "COMPLETED";

  public static final String FAILED = "FAILED";

  public static final String BRAND = "Brand";

  private static final String ACTIVE = "Active";

  private static final String SUSPENDED = "Suspended";

  private static final String NO_FBB = "No FBB";

  private static final String PRODUCT_AND_ITEMS_REQUEST_VO_MUST_NOT_BE_NULL =
      "productAndItemsRequestVO must not be null";

  private static final String NO_ITEM_UPDATED_WITH_SKU = "no item updated with sku ";

  private static final String ITEM_NOT_FOUND_FOR_PRODUCT_WITH_PRODUCT_SKU_AND_PPCODE =
      "item not found for product with product sku and pickup point code";

  public static final double ROUND_OFF_FACTOR = 100D;
  public static final int ONE = 1;
  public static final String PUBLISHING_THE_EVENT_TO_UPDATE_THE_AUDIT_LOGS =
    "Publishing the event {} to update the AuditLogs";

  private static final String BUSINESS_PARTNER_PRODUCT_SKU_MAP_MUST_NOT_BE_EMPTY = "business partner product sku map must not be empty";
  private static final String X_PRODUCT_PP_CODE_L_3_AUTO_HEAL = "X_PRODUCT_PP_CODE_L3_AUTO_HEAL";

  @Value("${product.visibility.switch.enabled}")
  private boolean isProductVisibilityEnabled;

  @Value("${sales.category.catalog.code}")
  private String salesCategoryCatalogCode;

  @Value("${b2b.sales.category.catalog.code}")
  private String b2bSalesCatalogCode;

  @Value("${sales.catalog.codes.value}")
  private String salesCatalogCodes;

  @Value("${external.search.reindex.enabled}")
  private boolean externalSearchReindexEnabled;

  @Value("${filter.mfd.false.product.attributes}")
  private boolean filterMfdFalseProductAttributes;

  @Value("${make.L5.offline.for.archived.products}")
  private boolean makeL5OfflineForArchivedProducts;

  @Value("${skip.mfd.update.for.suspended.products}")
  private boolean skipMfdUpdateForSuspendedProduct;

  @Value("${set.main.image.in.product.score.api.for.empty.case}")
  private boolean setMainImageInProductScoreApiForEmptyCase;

  @Value("${generate.product.score.filter.resize.image.enabled}")
  private boolean generateProductScoreFilterResizeImageEnabled;

  @Value("${generate.product.score.filter.resize.path}")
  private String generateProductScoreFilterResizePath;

  @Value("${replace.offer.price.with.merchant.promo.price.in.seller.side}")
  private boolean replaceOfferPriceWithMerchantPromoPriceInSellerSide;

  @Value("${replace.merchant.promo.discount.based.on.end.date}")
  private boolean replaceMerchantPromoDiscountBasedOnEndDate;

  @Value("${ignore.generating.merchant.promo.discount.when.promo.price.is.null}")
  private boolean ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull;

  @Value("${shared.product.bundle.recipe.edit.enabled}")
  private boolean sharedProductBundleRecipeEditEnabled;

  @Value("${override.archival.flag.at.l4.by.l3}")
  private boolean overrideArchivalFlagAtl4ByL3;

  @Value("${size.chart.addition.for.product}")
  private boolean sizeChartAdditionForProduct;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${enable.combine.other.bundling}")
  private boolean enableCombineOtherBundling;

  @Value("${bopis.category.action.on.internal.category.change.switch}")
  private boolean bopisCategoryActionOnInternalCategoryChangeSwitch;

  @Value("${product.score.update.without.special.attributes.enabled}")
  private boolean productScoreUpdateWithoutSpecialAttributesEnabled;

  @Value("${category.codes.limit.in.size.chart.listing}")
  private int categoryCodesLimitInSizeChartListing;

  @Value("${product.detail.remove.inactive.sales.categories}")
  private boolean productDetailRemoveInactiveSalesCategories;

  @Value("${multi.get.items.in.l3.listing}")
  private boolean multiGetItemsInL3Listing;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${odoo.creation.event.enabled}")
  private boolean odooCreationEventEnabled;

  @Value("${new.error.message.in.archive.flow}")
  private boolean newErrorMessageInArchiveFlow;

  @Value("${validate.youtube.url.in.x-product}")
  private boolean validateYouTubeUrlInXProduct;

  @Value("${product.basic.info.fetch.batch.size}")
  private int productBasicInfoFetchBatchSize;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Value("${auto.heal.missing.pp.codes.at.l3}")
  private boolean autoHealMissingPPCodesAtL3;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  @Lazy
  private ProductAndItemSolrRepository solrRepository;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private ProductCacheableService productCacheHelperService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private SaveAndPublishService saveAndPublishService;

  @Autowired
  private ProductHelperService productHelperService;

  @Autowired
  private MasterDataAttributeService masterDataAttributeService;

  @Autowired
  private SaveOperationService saveOperationService;

  @Autowired
  private CatalogService catalogService;

  @Autowired
  private SkuValidator skuValidator;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private MasterDataService masterDataService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private MasterDataConstructorService masterDataConstructorService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private ProductSearchHelperService productSearchHelperService;

  @Autowired
  @Lazy
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private BusinessPartnerPromoService businessPartnerPromoService;

  @Autowired
  private ProductScoreUtil productScoreUtil;

  @Autowired
  private YouTube youTube;

  @Autowired
  private ProductScoreHistoryL3Service productScoreHistoryL3Service;

  @Autowired
  private ProductSolrRepository productSolrRepository;

  @Autowired
  private ProductCenterHistoryService productCenterHistoryService;

  @Autowired
  private CacheEvictItemService cacheEvictItemService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private XbpOutbound xbpOutbound;

  @Value("${product.visibility.switch.enabled}")
  private boolean isPVSwitchEnabled;

  @Autowired
  private BusinessPartnerPickupPointService businessPartnerPickupPointService;

  @Autowired
  ItemPriceService itemPriceService;

  @Autowired
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Autowired
  private ItemRepository itemRepository;
  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value("${solr.string.delimiter}")
  private String solrStringDelimiter;

  @Autowired
  private CachedService cachedService;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${event.based.solr.update.enabled}")
  private boolean eventBasedSolrUpdateEnable;

  @Value("${master.category.catalog.code}")
  private String masterCatalogCode;

  @Value("${update.master.data.details.for.unsync.products}")
  private boolean updateMasterDataDetailsForUnsyncProducts;

  @Value("${halal.config.update.event.type.switch}")
  private boolean halaConfigUpdateEventTypeSwitch;

  @Value("${populate.product.main.image.in.listing}")
  private boolean populateProductMainImageInListing;

  @Value("${warehouse.bom.activated}")
  private boolean warehouseBomActivated;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCNCRestrictionEnabled;

  @Value("${bopis.category.validation.for.merchant.types}")
  private String bopisCategoryValidationMerchantTypes;

  @Value("${updated.error.message.get.product.and.items}")
  private boolean updatedErrorMessageInGetProductAndItems;

  @Value("${client-id.video.compression.event}")
  private String clientIdForVideoCompression;

  @Value("${publish.product.change.on.brand.or.name.change.generate.product.score}")
  private boolean publishProductChangeOnBrandOrNameChangeGenerateProductScore;

  @Value("${populate.label.for.upcoming.promo}")
  private boolean populateLabelForUpcomingPromo;

  @Value("${populate.label.for.pwp.promo}")
  private boolean populateLabelForPwpPromo;

  @Autowired
  private ChannelService channelService;

  @Autowired
  protected GdnMapper gdnMapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private WarehouseItemMasterOutbound warehouseItemMasterOutbound;

  @Autowired
  private PCBMasterDataService pcbMasterDataService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;


  @Override
  public boolean addProduct(String storeId, String requestId, String username, Product product) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(product != null, ProductServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(product.getProductCode()), ProductServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    ProductDetailResponse result =
        this.masterDataService.getProductDetailFromMasterData(username, requestId, product.getProductCode());
    product = this.getProductWithMasterData(requestId, username, storeId, product, result);
    this.saveOperationService.saveProduct(product);
    return true;
  }

  @Override
  public AddProductAndItemsResponseVo addProductAndItems(String storeId, String requestId, String username,
      ProductItemsVo productAndItemsRequestVO) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(productAndItemsRequestVO != null, ProductServiceImpl.PRODUCT_AND_ITEMS_REQUEST_VO_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(productAndItemsRequestVO.getProductVo().getProductCode()),
        ProductServiceImpl.PRODUCT_AND_ITEMS_REQUEST_VO_MUST_NOT_BE_NULL);
    checkArgument(StringUtils.isNotBlank(productAndItemsRequestVO.getProductVo().getMerchantCode()),
        ProductServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(productAndItemsRequestVO.getItemVoList() != null && !productAndItemsRequestVO.getItemVoList().isEmpty(),
        ProductServiceImpl.ITEMS_MUST_NOT_BE_NULL_OR_EMPTY);
    String productSku = productAndItemsRequestVO.getProductVo().getProductSku();
    if (StringUtils.isNotBlank(productSku)) {
      Product product = productRepository.findByStoreIdAndProductSku(storeId, productSku);
      if (Objects.nonNull(product)) {
        // if there is same storeid and productsku already in db, return default response
        return this.objectConverterService.convertToAddProductItemsResponseVo(productAndItemsRequestVO.getProductVo(),
            productAndItemsRequestVO.getItemVoList());
      }
    }
    ProductDetailResponse result = this.masterDataService.getProductDetailFromMasterData(username, requestId,
        productAndItemsRequestVO.getProductVo().getProductCode());
    BusinessPartner businessPartner = businessPartnerService.getBusinessPartnerByBusinessPartnerCode(storeId,
        productAndItemsRequestVO.getProductVo().getMerchantCode());
    Product product =
        this.getProductWithMasterData(requestId, username, storeId, productAndItemsRequestVO.getProductVo(), result);
    this.setProductScore(product, result);
    product.setPreOrder(productAndItemsRequestVO.getProductVo().getPreOrder());
    product.setVideo(productAndItemsRequestVO.getProductVo().getVideo());
    List<ItemVo> items = this.itemService.addItems(storeId, requestId, username, product.getProductSku(),
        productAndItemsRequestVO.getItemVoList(), product, result, businessPartner);
    publishProductPreorderStatusEvent(productAndItemsRequestVO);
    if (CollectionUtils.isNotEmpty(items)) {
      this.saveAndPublishService.publishItemCreationForMigration(
          items.stream().map(Item::getItemSku).collect(Collectors.toList()));
    }
    try {
      if (odooCreationEventEnabled) {
        saveAndPublishService.publishOdooCreationEvent(
            objectConverterService.convertToOdooCreationEventModel(product, items));
      }
    } catch (Exception exp) {
      log.error("Exception while publishing odoo creation event for product code : {} ", product.getProductCode(), exp);
    }
    return this.objectConverterService.convertToAddProductItemsResponseVo(product, items);
  }

  private void publishProductPreorderStatusEvent(ProductItemsVo productAndItemsRequestVO) {
    if (Objects.nonNull(productAndItemsRequestVO.getProductVo().getPreOrder()) && Boolean.TRUE.equals(
        productAndItemsRequestVO.getProductVo().getPreOrder().getIsPreOrder())) {
      PreOrderVO preOrderVO = new PreOrderVO();
      BeanUtils.copyProperties(productAndItemsRequestVO.getProductVo().getPreOrder(), preOrderVO);
      ProductPreOrderStatus productPreOrderStatus =
          ProductPreOrderStatus.builder().productCode(productAndItemsRequestVO.getProductVo().getProductCode())
              .productSku(productAndItemsRequestVO.getProductVo().getProductSku()).preOrder(preOrderVO).build();
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_PREORDER_STATUS,
        productPreOrderStatus.getProductSku(), productPreOrderStatus);
    }
  }

  private void setProductScore(Product product, ProductDetailResponse productDetailResponse) throws Exception {
    ProductScoreVo productScore = this.productScoreUtil
        .getProductScoreByProductScoreRequest(CommonUtil.getProductScoreRequest(product, productDetailResponse));
    ProductScore score = new ProductScore();
    if (Objects.nonNull(productScore) && Objects.isNull(product.getProductScore())) {
      BeanUtils.copyProperties(productScore, score);
    } else if (Objects.nonNull(productScore)) {
      BeanUtils.copyProperties(productScore, score, "variantCreatingScore");
      setVariantCreatingScore(product.getProductScore().getVariantCreatingScore(), score);
    }
    product.setProductScore(score);
  }

  @Override
  public void alterSalesCategorySequence(String storeId, String productSku,
      List<SalesCategorySequence> salesCategorySequenceRequests) {
    Product product = this.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    product.setSalesCategorySequences(salesCategorySequenceRequests);
    this.saveOperationService.saveProduct(product);
    if (isPVSwitchEnabled) {
      List<String> pristineIds = itemService.getPristineIdsByProductSku(storeId, productSku);
      itemService.updateSalesCatalogForAllPristineProducts(pristineIds);
    }
  }

  @Override
  public boolean deleteProduct(String storeId, String productSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);

    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    product.setMarkForDelete(true);
    List<Item> listOfItems = this.itemService.getItemsByProductSku(storeId, productSku);
    for (Item item : listOfItems) {
      item.setMarkForDelete(true);
    }
    this.saveOperationService.saveProductAndItems(new ProductAndItemsVO(product, listOfItems), new ArrayList<>());
    return true;
  }

  @Override
  public Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    return this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
  }

  @Override
  public Product findProductByStoreIdAndProductSkuAndMarkForDeleteFalseReadFromPrimary(String storeId,
      String productSku) {
    return this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, true);
  }

  @Override
  public List<Product> getAllSimpleProduct() {
    return this.productRepository.findByMarkForDeleteFalse();
  }

  @Override
  public Product getProduct(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    Product product =
        this.productCacheHelperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    return product;
  }


  @Override
  public Product getProductReadFromPrimary(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    Product product = productRepository.findProductByStoreIdAndProductSku(storeId, productSku, true);
    if (Objects.isNull(product) || product.isMarkForDelete()) {
      return null;
    }
    return product;
  }

  @Override
  public Product getProductFromDB(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    return this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
  }

  @Override
  public ProductItemsVo getProductAndItems(String storeId, String requestId, String username, String productSku,
    boolean showDeleted, boolean combineOthersBundlings, boolean off2On, boolean needProductData,
    boolean includeForceReview, boolean isMigrateAndSyncProduct) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    Product product = this.getProductForView(storeId, requestId, username, productSku, true, showDeleted);
    List<Item> items = this.itemService
        .getItemsForViewByProductSku(storeId, requestId, username, productSku, product.isSynchronized(),
            product.getMerchantCode(), showDeleted, combineOthersBundlings, off2On, isMigrateAndSyncProduct);
    productHelperService.findAndConstructOfflineItems(storeId, items);
    ProductAndItemsVO productAndItemsVO = null;
    if (needProductData) {
      productAndItemsVO = this.masterDataConstructorService
          .constructProductAndItemWithMasterData(storeId, username, requestId, product, items, false, false);
    } else {
      productAndItemsVO = this.masterDataConstructorService
          .constructProductAndItemWithMasterData(storeId, username, requestId, product, items,
              isProductVisibilityEnabled, false);
    }
    setItemCatalog(username, requestId, productAndItemsVO.getProduct());
    if (!includeForceReview) {
      this.checkProductAndItemsForForceReview(Collections.singletonList(productAndItemsVO.getProduct()),
        productAndItemsVO.getItems());
    }
    ProductItemsVo productItemsVo = CommonUtil.toProductItemVo(productAndItemsVO.getProduct(), productAndItemsVO.getItems());
    this.setSellerPromoBundlings(storeId, Collections.singletonList(productItemsVo));
    return productItemsVo;
  }

  private void setItemCatalog(String username, String requestId, Product product) throws Exception {
    List<ItemCatalogVO> itemCatalogVOS =
        this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, product);
    CommonUtil.removeInactiveSalesCategories(product, itemCatalogVOS, productDetailRemoveInactiveSalesCategories);
    product.setItemCatalogs(itemCatalogVOS);
  }

  @Override
  public ProductItemsVo getProductAndItemDetails(String storeId, String requestId, String username, String productSku,
      String pickupPointCode, boolean showDeleted, boolean combineOthersBundlings, boolean off2On,
      boolean needProductData, boolean includeForceReview, String fetchViewConfigByChannel) throws Exception {
    CommonUtil.checkArgumentApiIncorrectInputDataException(this.skuValidator.isProductSku(productSku),
        ApiErrorCodes.INVALID_SKU.getErrorMessage() + productSku, ApiErrorCodes.INVALID_SKU.getErrorCode());
    Product product;
    if (updatedErrorMessageInGetProductAndItems) {
      product = getProductByProductSkuAndShowDeleted(storeId, productSku, showDeleted);
    } else {
      product = this.getProductForView(storeId, requestId, username, productSku, true, showDeleted);
    }
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.findItemPickupPointsByProductSkuAndPPCodeWithShowDeleted(storeId, productSku,
            pickupPointCode, showDeleted);
    CommonUtil.checkArgumentApiIncorrectInputDataException(CollectionUtils.isNotEmpty(itemPickupPoints),
        ApiErrorCodes.INVALID_PP.getErrorMessage() + productSku + ", " + pickupPointCode, ApiErrorCodes.INVALID_PP.getErrorCode());
    Map<String, ItemPickupPoint> itemPickupPointMap =
        itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity()));
    List<Item> items = itemService.getItemsForViewByProductSkuAndPickUpPoint(storeId, productSku,
        showDeleted, enableCombineOtherBundling && combineOthersBundlings, off2On, itemPickupPointMap);
    items = itemService.getItemsWithDiscountAndPickUpPointDetails(storeId, productSku, pickupPointCode, items, itemPickupPoints, itemPickupPointMap, true);
    ProductAndItemsVO productAndItemsVO = null;
    if (needProductData) {
      productAndItemsVO = this.masterDataConstructorService
          .constructProductAndItemWithMasterData(storeId, username, requestId, product, items, false, showDeleted);
    } else {
      productAndItemsVO = this.masterDataConstructorService
          .constructProductAndItemWithMasterData(storeId, username, requestId, product, items,
              isProductVisibilityEnabled, showDeleted);
    }
    if (filterMfdFalseProductAttributes) {
      Optional.ofNullable(productAndItemsVO.getProduct()).map(Product::getMasterDataProduct)
          .map(MasterDataProduct::getMasterDataProductAttributes).orElse(new ArrayList<>()).removeIf(
              masterDataProductAttribute -> Optional.ofNullable(masterDataProductAttribute)
                  .map(MasterDataProductAttribute::getMarkForDelete).orElse(false));
    }


    setItemCatalog(username, requestId, productAndItemsVO.getProduct());
    if (!includeForceReview) {
      this.checkProductAndItemsForForceReview(Collections.singletonList(productAndItemsVO.getProduct()),
          productAndItemsVO.getItems());
    }
    ProductItemsVo productItemsVo =
        CommonUtil.toProductItemVoConverter(productAndItemsVO.getProduct(),
            productAndItemsVO.getItems(), itemPickupPointMap, overrideArchivalFlagAtl4ByL3);
    this.setSellerPromoBundlings(storeId, Collections.singletonList(productItemsVo));
    return productItemsVo;
  }

  @Override
  public Map<String, List<ProductAndItemsVO>> getProductAndItemsAvailability(String storeId, String requestId,
      String username, List<String> productSkus) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(productSkus != null, ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(productSkus.size() <= Integer.valueOf(systemParameterService
            .findValueByStoreIdAndVariable(storeId, SystemParameterNames.PRODUCT_AVAILIBILITY_REQUEST_LIMIT).getValue()),
        SKU_SIZE_LIMIT_EXCEEDED);

    String[] includedField =
        {ProductFieldNames.PRODUCT_SKU, ProductFieldNames.PRODUCT_CODE, ProductFieldNames.IS_SYNCHRONIZED,
            ProductFieldNames.MERCHANT_CODE, SHIPPING_WEIGHT_FIELD, ProductFieldNames.PRODUCT_TYPE};

    List<Product> productAvailables = new ArrayList<>();

    List<Product> productAvailableCandidate =
        productRepository.findByStoreIdAndProductSkusAndMarkForDeleteFalse(storeId, productSkus, includedField);

    Set<String> foundProductSkus = new HashSet<>();
    Set<String> syncProductCodes = new HashSet<>();

    for (Product productCandidate : productAvailableCandidate) {
      if (productCandidate.isSynchronized()) {
        syncProductCodes.add(productCandidate.getProductCode());
      } else {
        productAvailables.add(productCandidate);
        foundProductSkus.add(productCandidate.getProductSku());
      }
    }
    productAvailableCandidate = null; // for gc

    List<Product> productSyncAvailableCandidate = productRepository
        .findSyncProductByStoreIdAndProductCodesAndMarkForDeleteFalse(storeId, syncProductCodes, includedField);

    for (Product productCandidate : productSyncAvailableCandidate) {
      productAvailables.add(productCandidate);
      foundProductSkus.add(productCandidate.getProductSku());
    }
    productSyncAvailableCandidate = null; // for gc

    Map<String, List<Item>> itemAvailableCandidate = itemService.getItemAvailability(storeId, foundProductSkus);
    Map<String, MasterDataProductAndItemsVO> masterDataProducts =
        masterDataService.getMasterDataProductDetailResponse(storeId, username, requestId, syncProductCodes, false);

    return productHelperService
        .constructProductWithItemAndMasterData(productAvailables, itemAvailableCandidate, masterDataProducts);
  }

  @Override
  public List<Product> deleteProductByStoreIdAndProductSkus(String storeId, Set<String> productSkus) {
    checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(productSkus), ErrorMessages.PRODUCT_SKU_SET_MUST_NOT_BE_EMPTY);
    return productRepository.deleteByStoreIdAndProductSkuIn(storeId, productSkus);
  }

  @Override
  public List<ProductAndItemsVO> getProductAndItemsByItemSkus(String storeId, String requestId, String username,
      Set<String> itemSkus) throws Exception {
    return getProductAndItemsByItemSkusForActiveItems(storeId, requestId, username, itemSkus, true, false, false,
        false);
  }

  @Override
  public List<ProductAndItemsVO> getProductAndItemsByItemSkusForActiveItems(String storeId, String requestId,
      String username, Set<String> itemSkus, boolean fullFetch, boolean combineOthersBundlings, boolean off2On,
      boolean needProductData) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(itemSkus), ITEMS_MUST_NOT_BE_NULL_OR_EMPTY);
    checkArgument(StringUtils.isNotBlank(requestId), REQUEST_ID_MUST_NOT_BE_BLANK);
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    Map<String, List<ItemCatalogVO>> mapOfItemCatalogs = new HashMap<>();
    for (String itemSku : itemSkus) {
      try {
        ProductAndItemsVO productAndItemsVO =
            getProductAndSingleItemByItemSku(storeId, requestId, username, itemSku, true, fullFetch,
                combineOthersBundlings, false, null, off2On, needProductData, false);
        setItemCatalogsForProduct(requestId, username, productAndItemsVOs, mapOfItemCatalogs, productAndItemsVO);
      } catch (Exception e) {
        ProductServiceImpl.LOG.error("Error getProductAndItemsByItemSkus itemSku:{}", itemSku, e);
      }
    }
    return productAndItemsVOs;
  }

  @Override

  public List<ProductAndItemsVO> getProductAndItemsByItemSkusForAllItems(String storeId, String requestId,
      String username, Set<String> itemSkus, boolean fullFetch, boolean combineOthersBundlings, boolean off2On)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(itemSkus), ITEMS_MUST_NOT_BE_NULL_OR_EMPTY);
    checkArgument(StringUtils.isNotBlank(requestId), REQUEST_ID_MUST_NOT_BE_BLANK);
    List<ProductAndItemsVO> productAndItemsVOs = new ArrayList<>();
    Map<String, List<ItemCatalogVO>> mapOfItemCatalogs = new HashMap<>();
    for (String itemSku : itemSkus) {
      try {
        ProductAndItemsVO productAndItemsVO =
            getProductDetailAndSingleItemByItemSku(storeId, requestId, username, itemSku, true, fullFetch,
                combineOthersBundlings, false, null, off2On, null);
        setItemCatalogsForProduct(requestId, username, productAndItemsVOs, mapOfItemCatalogs, productAndItemsVO);
      } catch (Exception e) {
        ProductServiceImpl.LOG.error("Error getProductAndItemsByItemSkus itemSku:{}", itemSku, e);
      }
    }
    return productAndItemsVOs;
  }

  private void setItemCatalogsForProduct(String requestId, String username, List<ProductAndItemsVO> productAndItemsVOs,
      Map<String, List<ItemCatalogVO>> mapOfItemCatalogs, ProductAndItemsVO productAndItemsVO) throws Exception {
    Product product = productAndItemsVO.getProduct();
    List<ItemCatalogVO> itemCategoryVOListMap = mapOfItemCatalogs.get(product.getProductSku());
    if (CollectionUtils.isEmpty(itemCategoryVOListMap)) {
      itemCategoryVOListMap = this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, product);
      mapOfItemCatalogs.put(product.getProductSku(), itemCategoryVOListMap);
    }
    product.setItemCatalogs(itemCategoryVOListMap);
    productAndItemsVOs.add(productAndItemsVO);
  }

  @Override
  public ProductItemsVo getProductAndSingleItemByItemSku(String storeId, String requestId, String username,
      String itemSku, boolean needMasterDataDetail, boolean instantPickup, String pickupPointCode,
      boolean fetchMfdTrueItem) throws Exception {
    ProductAndItemsVO result =
        getProductAndSingleItemByItemSku(storeId, requestId, username, itemSku, needMasterDataDetail, true, false,
            instantPickup, pickupPointCode, false, false, fetchMfdTrueItem);
    ProductItemsVo productItemsVo = null;
    List<ProductItemsVo> productItemsVoList = null;
    if (Objects.nonNull(result) && Objects.nonNull(result.getProduct())) {
      this.productSearchHelperService
          .setItemCatalogs(storeId, username, requestId, true, Collections.singletonList(result), Collections
              .singletonMap(result.getProduct().getProductCode(), result.getProduct().getMasterDataProduct()));
      setPriceAndItemViewConfigFromItemPickupPoint(storeId, result);
      productItemsVo = CommonUtil.toProductItemVo(result.getProduct(), result.getItems());
      productItemsVoList = Collections.singletonList(productItemsVo);
      setSellerPromoBundlings(storeId, productItemsVoList);
      return productItemsVoList.get(0);
    }
    log.error("productItemsVoList is null, itemSku : {} ", itemSku);
    return productItemsVo;
  }

  @Override
  public ProductAndItemsVO getProductAndSingleItemByItemSku(String storeId, String requestId, String username,
      String itemSkuL4OrL5Id, boolean needMasterDataDetail, boolean fullFetch, boolean combineOthersBundlings,
      boolean instantPickup, String pickupPointCode, boolean off2On, boolean needProductData, boolean fetchMfdTrueItem)
      throws Exception {
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSkuL4OrL5Id),
        ProductServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSkuL4OrL5Id);
    Item item = this.itemService
        .getItem(storeId, requestId, username, itemSkuL4OrL5Id, needMasterDataDetail, fullFetch, combineOthersBundlings,
            instantPickup, pickupPointCode, off2On, fetchMfdTrueItem);
    if (needProductData) {
      return getProductAndItemsVODetail(storeId, requestId, username, itemSkuL4OrL5Id, needMasterDataDetail, fullFetch,
          combineOthersBundlings, instantPickup, pickupPointCode, off2On, item, false, false, true);
    } else {
      return getProductAndItemsVODetail(storeId, requestId, username, itemSkuL4OrL5Id, needMasterDataDetail, fullFetch,
          combineOthersBundlings, instantPickup, pickupPointCode, off2On, item, false, false, null);
    }
  }

  @Override
  public ProductAndItemsVO getProductDetailAndSingleItemByItemSku(String storeId, String requestId, String username,
      String itemSku, boolean needMasterDataDetail, boolean instantPickup, String pickupPointCode) throws Exception {
    log.info(
        "Fetching the product and item detail : itemSku {}, needMasterDataDetail {},  instantPickup {} ,  pickupPointCode {}",
        itemSku, needMasterDataDetail, instantPickup, pickupPointCode);
    ProductAndItemsVO result =
        getProductDetailAndSingleItemByItemSku(storeId, requestId, username, itemSku, needMasterDataDetail, true, false,
            instantPickup, pickupPointCode, false, Boolean.TRUE);
    if (Objects.nonNull(result) && Objects.nonNull(result.getProduct())) {
      this.productSearchHelperService
          .setItemCatalogs(storeId, username, requestId, true, Collections.singletonList(result), Collections
              .singletonMap(result.getProduct().getProductCode(), result.getProduct().getMasterDataProduct()));
      setPriceAndItemViewConfigFromItemPickupPoint(storeId, result);
    }
    return result;
  }

  @Override
  public void setPriceAndItemViewConfigFromItemPickupPoint(String storeId, ProductAndItemsVO productAndItemsVO) {
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService.findByItemSkuInAndDelivery(storeId,
        productAndItemsVO.getItems().stream().map(Item::getItemSku).distinct().collect(Collectors.toList()), true);
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      Map<String, ItemPickupPoint> itemPickupPointMap =
          itemPickupPoints.stream().collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
      for (Item item : productAndItemsVO.getItems()) {
        ItemPickupPoint itemPickupPoint =
            Optional.ofNullable(itemPickupPointMap.get(item.getItemSku())).orElse(new ItemPickupPoint());
        CommonUtil.setItemPickupPointDetailsInItem(item, itemPickupPoint, false);
      }
      itemPriceService.validateAndSetDiscountPrice(productAndItemsVO.getItems());
    }
  }

  @Override
  public void setPriceAndItemViewConfigFromItemPickupPointForItemSummary(String storeId,
      ProductAndItemsVO productAndItemsVO) {
    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService.findByItemSkusAndDelivery(storeId,
        productAndItemsVO.getItems().stream().map(Item::getItemSku).distinct().collect(Collectors.toList()), true);
    if (CollectionUtils.isNotEmpty(itemPickupPoints)) {
      Map<String, ItemPickupPoint> itemPickupPointMap = itemPickupPoints.stream()
          .collect(Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
      for (Item item : productAndItemsVO.getItems()) {
        ItemPickupPoint itemPickupPoint =
            Optional.ofNullable(itemPickupPointMap.get(item.getItemSku())).orElse(new ItemPickupPoint());
        CommonUtil.setItemPickupPointDetailsInItem(item, itemPickupPoint, true);
        item.setVersion(itemPickupPoint.getVersion()); // copying version from l5 to l4
      }
    }
  }

  @Override
  public void updateMasterDataFieldsInProduct(String storeId, String productSku) throws Exception {
    Product product = this.findByStoreIdAndProductSku(storeId, productSku);
    if (Objects.isNull(product) || !product.isSynchronized()) {
      return;
    }
    ProductDetailResponse productDetailResponse =
        this.productCategoryBaseOutbound.getProductDetailByProductCode(Constants.DEFAULT_USERNAME,
            Constants.DEFAULT_USERNAME, product.getProductCode());
    updateMasterDataFieldsInProduct(product, productDetailResponse, false);
  }

  private ProductAndItemsVO updateMasterDataFieldsInProduct(Product product,
      ProductDetailResponse productDetailResponse, boolean updateCategory) {
    List<Item> items =
        cacheItemHelperService.findCacheableByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, product.getProductSku());
    List<Item> updatedItems = setMasterDataDetailsInProductAndItems(product, items, productDetailResponse, updateCategory);
    ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, items);
    productAndItemsVO =
        saveOperationService.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(productAndItemsVO);
    productAndItemsVO.getProduct().setUpdatedFields(product.getUpdatedFields());
    if (CollectionUtils.isNotEmpty(updatedItems)) {
      saveAndPublishService.publishListOfItems(updatedItems);
    }
    return productAndItemsVO;
  }

  private ProductDetailResponse setMasterDataDetailsInProductAndItemsOnEdit(String storeId, Product product, List<Item> items,
      boolean updateCategory, ProductDetailResponse productDetailResponse) throws JsonProcessingException {
    try {
      if (Objects.isNull(productDetailResponse)) {
        productDetailResponse =
            this.productCategoryBaseOutbound.getProductDetailByProductCode(Constants.DEFAULT_REQUEST_ID,
                Constants.DEFAULT_USERNAME, product.getProductCode());
      }
      setMasterDataDetailsInProductAndItems(product, items, productDetailResponse, updateCategory);
    } catch (Exception e) {
      log.error("Error while setting master data details in product and items on edit,  product : {} ", product, e);
      String message = objectMapper.writeValueAsString(
          ProductScoreRetryDTO.builder().storeId(storeId).productCode(product.getProductCode())
              .productSku(product.getProductSku()).updateCategory(false).userName(Constants.DEFAULT_USERNAME)
              .requestId(Constants.DEFAULT_REQUEST_ID).build());
      ProductRetryEventPublish productRetryEventPublish =
          ProductRetryEventPublish.builder().clearCache(Boolean.FALSE).retryCount(0).identifier(message)
              .retryPublishStatus(RetryPublishStatus.PENDING).topicName(DomainEventName.PRODUCT_SCORE_UPDATE_EVENT_NAME)
              .build();
      this.productRetryEventPublishService.insertToRetryPublish(productRetryEventPublish);
    }
    return productDetailResponse;
  }

  private List<Item> setMasterDataDetailsInProductAndItems(Product product, List<Item> items,
      ProductDetailResponse productDetailResponse, boolean updateCategory) {
    List<Item> updatedItems = new ArrayList<>();
    //set master data details in product
    boolean l3DetailChanged = CommonUtil.isProductMasterDataDetailChanged(product, productDetailResponse, updateCategory);
    boolean curationStatusChanged = false;
    if (updateCategory) {
      curationStatusChanged = CommonUtil.updateCurationStatusOfProduct(product,
          CommonUtil.masterCategoryFromProductDetailResponse(productDetailResponse, salesCategoryCatalogCode));
    }
    boolean brandUpdated = !StringUtils.equals(product.getBrand(), productDetailResponse.getBrand());
    boolean nameUpdated = !StringUtils.equals(product.getProductName(), productDetailResponse.getName());
    setUpdatedFieldsAtProductLevel(product, brandUpdated, nameUpdated);
    product.setBrand(productDetailResponse.getBrand());
    product.setCategoryCode(productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode());
    product.setProductName(productDetailResponse.getName());
    product.setMasterCatalog(
        new MasterCatalog(masterCatalogCode, new Category(product.getCategoryCode(), product.getCategoryCode())));

    if (updateCategory) {
      setSalesCategoryMappingToProduct(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_REQUEST_ID,
          Constants.DEFAULT_USERNAME, product);
    }

    //set master data details in item
    Map<String, ProductItemResponse> productItemResponseMap =
        Optional.ofNullable(productDetailResponse.getProductItemResponses()).orElse(new HashSet<>()).stream()
            .collect(toMap(ProductItemResponse::getSkuCode, Function.identity()));
    CommonUtil.updateFieldsForCategoryBrandOrNameChange(items, updateCategory, nameUpdated, brandUpdated);
    for (Item item : items) {
      updateProductAndItemsData(product, productDetailResponse, item, productItemResponseMap,
        l3DetailChanged, curationStatusChanged, updatedItems);
    }

    return updatedItems;
  }

  private static void setUpdatedFieldsAtProductLevel(Product product, boolean brandUpdated, boolean nameUpdated) {
    if (brandUpdated || nameUpdated) {
      product.setUpdatedFields(new HashSet<>());
      if (brandUpdated) {
        product.getUpdatedFields().add(ProductChangeEventType.BRAND_CHANGE);
      }
      if (nameUpdated) {
        product.getUpdatedFields().add(ProductChangeEventType.PRODUCT_NAME_CHANGE);
      }
    }
  }

  private void updateProductAndItemsData(Product product, ProductDetailResponse productDetailResponse, Item item,
    Map<String, ProductItemResponse> productItemResponseMap, boolean l3DetailChanged,
    boolean curationStatusChanged, List<Item> updatedItems) {
    if (CommonUtil.isDimensionUpdated(productDetailResponse, item)) {
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>(item.getItemChangeEventTypes());
      itemChangeEventTypes.add(ItemChangeEventType.SHIPPING_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
    }
    boolean isL4DataChanged = false;
    ProductItemResponse productItemResponse = productItemResponseMap.get(item.getItemCode());
    if (Objects.nonNull(productItemResponse)) {
      isL4DataChanged = CommonUtil.isItemMasterDataDetailChanged(item, productDetailResponse, productItemResponse);
      item.setCategoryCode(
          productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode());
      item.setLength(Optional.ofNullable(productDetailResponse.getLength()).orElse(0.0));
      item.setWeight(Optional.ofNullable(productDetailResponse.getWeight()).orElse(0.0));
      item.setShippingWeight(
          Optional.ofNullable(productDetailResponse.getShippingWeight()).orElse(0.0));
      item.setWidth(Optional.ofNullable(productDetailResponse.getWidth()).orElse(0.0));
      item.setHeight(Optional.ofNullable(productDetailResponse.getHeight()).orElse(0.0));
      item.setBrand(productDetailResponse.getBrand());

      item.setGeneratedItemName(productItemResponse.getGeneratedItemName());
      item.setDangerousLevel(productItemResponse.getDangerousGoodsLevel());
      String mainImage = productItemResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
          .filter(image -> resizeImageFilter(image)).filter(CommonUtil::filterProcessedImages)
          .filter(image -> image.isMainImages()).map(Image::getLocationPath).findAny().orElse(StringUtils.EMPTY);
      mainImage = setMainImagesInProductScoreAPI(mainImage, productItemResponse);
      item.setMainImageUrl(mainImage);
      item.setDefiningAttributes(productItemResponse.getProductItemAttributeValueResponses().stream().filter(
          productItemAttributeValueResponse -> CommonUtil.isDefiningOrVariantCreating(
              productItemAttributeValueResponse)).map(
          productItemAttributeValueResponse -> objectConverterService.toProductAttributeDetail(
              productItemAttributeValueResponse)).collect(Collectors.toList()));
      item.setUpcCode(productItemResponse.getUpcCode());
      updateDimensionsMissing(product, item);
      if (l3DetailChanged || isL4DataChanged || curationStatusChanged) {
        updatedItems.add(item);
      }
    }
  }

  private @Nullable String setMainImagesInProductScoreAPI(String mainImage, ProductItemResponse productItemResponse) {
    if (StringUtils.isBlank(mainImage) && setMainImageInProductScoreApiForEmptyCase) {
      mainImage = productItemResponse.getImages().stream().filter(image -> !image.isMarkForDelete())
          .filter(image -> resizeImageFilter(image)).filter(image -> CommonUtil.filterProcessedImages(image))
          .map(Image::getLocationPath).findFirst().orElse(StringUtils.EMPTY);
    }
    return mainImage;
  }

  private static void updateDimensionsMissing(Product product, Item item) {
    if (Boolean.TRUE.equals(product.getDimensionsMissing()) && !ProductType.BOPIS.equals(
      product.getProductType())) {
      product.setDimensionsMissing(CommonUtil.isAllDimensionsAreZero(item));
    }
  }

  private boolean resizeImageFilter(Image image) {
    if (generateProductScoreFilterResizeImageEnabled) {
      return !Optional.ofNullable(image).map(Image::getLocationPath).orElse(StringUtils.EMPTY)
          .contains(generateProductScoreFilterResizePath);
    } else {
      return true;
    }
  }

  @Override
  public ProductAndItemsVO getProductDetailAndSingleItemByItemSku(String storeId, String requestId, String username,
      String itemSkuL4OrL5Id, boolean needMasterDataDetail, boolean fullFetch, boolean combineOthersBundlings,
      boolean instantPickup, String pickupPointCode, boolean off2On, Boolean ignorePVSwitch) throws Exception {
    checkArgument(this.skuValidator.isItemSkuL4OrL5(itemSkuL4OrL5Id),
        ProductServiceImpl.INVALID_ITEM_SKU_FORMAT + itemSkuL4OrL5Id);
    Item item = this.itemService
        .getDetailsForActiveOrSuspendedItem(storeId, requestId, username, itemSkuL4OrL5Id, needMasterDataDetail,
            fullFetch, combineOthersBundlings, instantPickup, pickupPointCode, off2On);
    return getProductAndItemsVODetail(storeId, requestId, username, itemSkuL4OrL5Id, needMasterDataDetail, fullFetch,
        combineOthersBundlings, instantPickup, pickupPointCode, off2On, item, true, true, ignorePVSwitch);
  }

  private ProductAndItemsVO getProductAndItemsVODetail(String storeId, String requestId, String username,
      String itemSkuL4OrL5Id, boolean needMasterDataDetail, boolean fullFetch, boolean combineOthersBundlings,
      boolean instantPickup, String pickupPointCode, boolean off2On, Item item, boolean showDeleted,
      boolean allProducts, Boolean ignorePVSwitch) throws Exception {
    if (Objects.isNull(item)) {
      ItemPickupPoint itemPickupPoint =
          this.itemPickupPointService.findByStoreIdAndOfflineItemIdAndMarkForDeleteFalse(storeId, itemSkuL4OrL5Id);
      checkArgument(Objects.nonNull(itemPickupPoint), ProductServiceImpl.ITEM_NOT_FOUND);
      String itemSkuFromOfflineItem = itemPickupPoint.getItemSku();
      if (allProducts) {
        item = this.itemService.getDetailsForActiveOrSuspendedItem(storeId, requestId, username, itemSkuFromOfflineItem,
            needMasterDataDetail, fullFetch, combineOthersBundlings, instantPickup, pickupPointCode, off2On);
      } else {
        item = this.itemService
            .getItem(storeId, requestId, username, itemSkuFromOfflineItem, needMasterDataDetail, fullFetch,
                combineOthersBundlings, instantPickup, pickupPointCode, off2On, false);
      }
      checkArgument(Objects.nonNull(item), ProductServiceImpl.ITEM_NOT_FOUND);
      productHelperService.constructOfflineItem(item,
          CommonUtil.getOfflineItemByPickupPoint(itemPickupPoint, false, null));
    } else {
      productHelperService.findAndConstructOfflineItemsByPickupPointCode(storeId, List.of(item), pickupPointCode);
    }
    item.setUniqueId(item.getItemSku());
    Product product =
        this.getProductForView(storeId, requestId, username, item.getProductSku(), needMasterDataDetail, showDeleted);
    return this.masterDataConstructorService
        .constructProductAndItemWithMasterData(storeId, username, requestId, product, List.of(item),
            Boolean.TRUE.equals(ignorePVSwitch) ? !ignorePVSwitch : isProductVisibilityEnabled, allProducts);
  }

  @Override
  public Product getProductDeletedOrUndeleted(String storeId, String productSku) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    return this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
  }

  @Override
  public Product getProductForView(String storeId, String requestId, String username, String productSku,
      boolean needMasterDataDetail, boolean showDeleted) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    Product product = null;
    if (showDeleted) {
      product = this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
    } else {
      product =
          this.productCacheHelperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    }
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    return product;
  }

  public Product getProductByProductSkuAndShowDeleted(String storeId, String productSku, boolean showDeleted) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    Product product = productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
    if (Objects.isNull(product)) {
      throw new ApiIncorrectInputDataException(ApiErrorCodes.INVALID_SKU.getErrorMessage() + productSku,
          ApiErrorCodes.INVALID_SKU.getErrorCode());
    } else if (!showDeleted && product.isMarkForDelete()) {
      if (product.isSuspended() || product.isTakenDown()) {
        throw new ApiIncorrectInputDataException(ApiErrorCodes.PRODUCT_SUSPENDED.getErrorMessage() + productSku,
            ApiErrorCodes.PRODUCT_SUSPENDED.getErrorCode());
      } else {
        throw new ApiIncorrectInputDataException(ApiErrorCodes.PRODUCT_PERMANENTLY_DELETED.getErrorMessage() + productSku,
            ApiErrorCodes.PRODUCT_PERMANENTLY_DELETED.getErrorCode());
      }
    }
    return product;
  }

  @Override
  public List<Product> getProducts(String storeId, Set<String> productSkus) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(productSkus != null && !productSkus.isEmpty(), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    Optional.ofNullable(productSkus).orElse(new HashSet<>()).forEach(productSku -> {
      checkArgument(this.skuValidator.isProductSku(productSku),
          ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    });
    return this.productRepository.findProductByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, productSkus);
  }

  @Override
  public List<Product> getProductsByMerchantCode(String storeId, String merchantCode) {
    Set<Product> products =
        this.productRepository.findProductsByStoreIdAndMerchantCodeAndMarkForDeleteFalse(storeId, merchantCode);
    return new ArrayList<>(products);
  }

  @Override
  public List<Product> getProductsByProductCatentryIds(String storeId, Set<String> productCatentryIds) {
    return this.productRepository.findByStoreIdAndProductCatentryIdInAndMarkForDeleteFalse(storeId, productCatentryIds);
  }

  @Override
  public List<Product> getProductsByProductCode(String storeId, String productCode) {
    return this.productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
  }

  @Override
  public List<Product> getProductsByProductCodes(String storeId, Set<String> productCodes) {
    return this.productRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes);
  }

  @Override
  public List<Product> getProductsByProductCodes(String storeId, Set<String> productCodes, String[] fields) {
    return this.productRepository.findByStoreIdAndProductCodes(storeId, productCodes, fields);
  }

  @Override
  public List<Product> getProductsByProductSkus(String storeId, Set<String> productSkus, String[] fields,  boolean showDeleted) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!productSkus.isEmpty(), ProductServiceImpl.PRODUCT_SKU_SET_MUST_NOT_BE_EMPTY);
    checkArgument(fields != null, ProductServiceImpl.FIELDS_MUST_NOT_BE_NULL);
    return this.productRepository.findByStoreIdAndProductSkus(storeId, productSkus, fields, showDeleted);
  }

  @Override
  public Long getProductsCountByBrand(String storeId, String brand) {
    return this.solrRepository.findByStoreIdAndBrandAndMarkForDeleteFalse(storeId, brand);
  }

  private Product getProductWithMasterData(String requestId, String username, String storeId, Product product,
      ProductDetailResponse productDetailResponse) throws Exception {
    checkArgument(productDetailResponse != null,
        ProductServiceImpl.PRODUCT_CODE_NOT_FOUND_ON_MASTER_DATA_WITH_PRODUCT_CODE + product.getProductCode());
    if (product.getProductSpecialAttributes() != null && productDetailResponse.getProductAttributeResponses() != null) {
      for (ProductSpecialAttribute productSpecialAttribute : product.getProductSpecialAttributes()) {
        if (StringUtils.isBlank(productSpecialAttribute.getAttributeCode())) {
          continue;
        }
        for (ProductAttributeResponse attribute : productDetailResponse.getProductAttributeResponses()) {
          AttributeResponse attributeDetail = attribute.getAttribute();
          if (productSpecialAttribute.getAttributeCode().equals(attributeDetail.getAttributeCode())) {
            productSpecialAttribute.setAttributeName(attributeDetail.getName());
            break;
          }
        }
      }
    }
    return this.productHelperService.setProductDetail(requestId, username, storeId, product, productDetailResponse);
  }

  @Override
  public ProductAndItemsVO synchronizeProduct(String storeId, String productSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    checkState(product.getProductCode() != null, ProductServiceImpl.NO_MASTER_DATA_PRODUCT_SPECIFIED);
    checkState(!product.isSynchronized(), ProductServiceImpl.CHANGE_SYNC_STATUS_FAILED);
    ProductScore existingProductScore = new ProductScore();
    if (Objects.nonNull(product.getProductScore())) {
      BeanUtils.copyProperties(product.getProductScore(), existingProductScore);
    }
    List<Item> items = this.itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    product.setSynchronized(true);
    ProductDetailResponse productDetailResponse = this.masterDataService
        .getProductDetailFromMasterData(GdnMandatoryRequestParameterUtil.getUsername(),
            GdnMandatoryRequestParameterUtil.getRequestId(), product.getProductCode());
    this.setProductScore(product, productDetailResponse);
    ProductScore updatedProductScore = new ProductScore();
    if (Objects.nonNull(product.getProductScore())) {
      BeanUtils.copyProperties(product.getProductScore(), updatedProductScore);
    }
    ProductAndItemsVO productAndItemsVO;
    if (CollectionUtils.isNotEmpty(items)) {
      for (Item item : items) {
        item.setSynchronized(true);
        item.setItemChangeEventTypes(new ArrayList<>());
        item.getItemChangeEventTypes().add(ItemChangeEventType.SYNC_UNSYNC_FLAG_CHANGE);
        item.setContentChanged(true);
        if (Objects.nonNull(item.getPristineDataItem())) {
          item.setPristineDataItem(null);
          item.getItemChangeEventTypes().add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
        }
      }
      productAndItemsVO = this.masterDataConstructorService
          .constructProductAndItemWithMasterDataAndEvictCaches(storeId, GdnMandatoryRequestParameterUtil.getUsername(),
              GdnMandatoryRequestParameterUtil.getRequestId(), product, items);
      this.masterDataAttributeService.setAndSaveMasterDataAttributeProduct(productAndItemsVO.getProduct());
      this.masterDataAttributeService.setAndSaveMasterDataAttributeItems(productAndItemsVO.getItems());
      setMasterDataProductAndItemsAsNull(productAndItemsVO.getProduct(), productAndItemsVO.getItems());
      productAndItemsVO = this.saveOperationService.saveProductAndItemsWithoutUpdatingSolr(productAndItemsVO);
    } else {
      product.setMasterDataProduct(null);
      productAndItemsVO =
          this.saveOperationService.saveProductAndItemsWithoutUpdatingSolr(new ProductAndItemsVO(product, items));
    }
    productAndItemSolrIndexerService.updateSolrOnSyncUnsyncAction(productAndItemsVO);
    if (!GdnObjects.equals(existingProductScore, updatedProductScore)) {
      this.productScoreHistoryL3Service
          .saveProductScoreHistoryL3(storeId, productSku, existingProductScore, updatedProductScore);
    }
    return productAndItemsVO;
  }

  private static void setMasterDataProductAndItemsAsNull(Product product, List<Item> items) {
    product.setMasterDataProduct(null);
    items.stream().forEach(item -> item.setMasterDataItem(null));
  }

  @Override
  public ProductAndItemsVO unsynchronizeProduct(String storeId, String requestId, String username, String productSku,
      boolean overwriteExistingMasterData) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);

    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    checkState(product.isSynchronized(), ProductServiceImpl.CHANGE_SYNC_STATUS_FAILED);
    List<Item> items = this.itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    ProductAndItemsVO productAndItemsVO = null;
    if (overwriteExistingMasterData || product.getMasterDataProduct() == null) {
      productAndItemsVO = this.masterDataConstructorService
          .constructProductAndItemWithMasterData(storeId, username, requestId, product, items);
      for (Item item : items) {
        item.setMasterDataItem(this.masterDataConstructorService.
            constructItemDimensionFields(item.getMasterDataItem(), product.getMasterDataProduct()));
      }
      this.masterDataAttributeService.setAndSaveMasterDataAttributeProduct(productAndItemsVO.getProduct());
      this.masterDataAttributeService.setAndSaveMasterDataAttributeItems(productAndItemsVO.getItems());
    } else {
      productAndItemsVO = new ProductAndItemsVO(product, items);
    }
    productAndItemsVO.getProduct().setSynchronized(false);
    for (Item item : productAndItemsVO.getItems()) {
      item.setSynchronized(false);
      item.setContentChanged(false);
      List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
      itemChangeEventTypes.add(ItemChangeEventType.SYNC_UNSYNC_FLAG_CHANGE);
      item.setItemChangeEventTypes(itemChangeEventTypes);
    }
    productAndItemsVO = this.saveOperationService.saveProductAndItemsWithoutUpdatingSolr(productAndItemsVO);
    productAndItemSolrIndexerService.updateSolrOnSyncUnsyncAction(productAndItemsVO);
    return productAndItemsVO;
  }

  @Override
  public Product updateProduct(String storeId, String requestId, Product product, boolean isOnlyExternal) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(product != null, ProductServiceImpl.PRODUCT_MUST_NOT_BE_NULL);
    boolean itemChanged = false;
    boolean freeSampleChanged = false;
    boolean off2OnActiveChanged = false;
    ProductPreOrderStatus productPreOrderStatus = null;
    log.info("Producct request = {}", product);
    Product currProduct =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, product.getProductSku(),
            false);
    checkState(currProduct != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    checkState(!currProduct.isForceReview(), CommonUtil.PRODUCT_IS_NOT_EDITABLE);
    ProductScore existingProductScore = new ProductScore();
    if (Objects.nonNull(currProduct.getProductScore())) {
      BeanUtils.copyProperties(currProduct.getProductScore(), existingProductScore);
    }
    List<Boolean> productAndSpecialAttributeChanged = compareAndUpdateProductFields(product, currProduct);
    boolean productChanged = productAndSpecialAttributeChanged.get(0);
    boolean specialAttributeChanged = productAndSpecialAttributeChanged.get(1);
    Map<String, Boolean> stringBooleanMap = compareProductFlagsChanged(product, currProduct);
    off2OnActiveChanged = stringBooleanMap.getOrDefault(Constants.OFF_2ON_ACTIVE_CHANGED, false);
    freeSampleChanged = stringBooleanMap.getOrDefault(Constants.FREE_SAMPLE_CHANGED, false);
    if(off2OnActiveChanged){
      log.info("off2OnActiveChanged = {}", off2OnActiveChanged);
      Map<String, Boolean> off2OnFLagByProductSkuMap = new HashMap<>();
      off2OnFLagByProductSkuMap.put(product.getProductSku(), product.isOff2OnChannelActive());
      updateOff2OnFlagByProductSkus(product.getStoreId(), off2OnFLagByProductSkuMap,
          GdnMandatoryRequestParameterUtil.getUsername(),GdnMandatoryRequestParameterUtil.getChannelId(),
          GdnMandatoryRequestParameterUtil.getClientId(),requestId,false);
      currProduct.setOff2OnChannelActive(product.isOff2OnChannelActive());
      itemChanged = true;
      productChanged = true;
    }
    if(freeSampleChanged){
      currProduct.setFreeSample(product.isFreeSample());
      itemChanged = true;
      productChanged = true;
    }
    List<Item> items = items =
        this.itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, currProduct.getProductSku());
    SystemParameter systemParameter = new SystemParameter();
    if (!currProduct.isSynchronized()) {
      boolean disableUnSyncUpdate = false;
      try {
        systemParameter = this.systemParameterService
            .findValueByStoreIdAndVariable(currProduct.getStoreId(), Constants.CATEGORY_CODE_VARIABLE);
      } catch (ApplicationRuntimeException e) {
        systemParameter.setValue(StringUtils.EMPTY);
      }
      if (Objects.nonNull(currProduct.getMasterDataProduct())) {
        if (Objects.nonNull(currProduct.getMasterDataProduct().getMasterCatalog())) {
          if (Objects.nonNull(currProduct.getMasterDataProduct().getMasterCatalog().getCategory())) {
            disableUnSyncUpdate = CommonUtil.checkForDisableUnSyncUpdate(systemParameter.getValue(),
                currProduct.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode());
          }
        }
      }
      MasterDataProduct currentMasterDataProduct = currProduct.getMasterDataProduct();
      MasterDataProduct newMasterDataProduct = product.getMasterDataProduct();
      String oldProductName = currentMasterDataProduct.getProductName();
      String newProductName = newMasterDataProduct.getProductName();
      if (!oldProductName.equals(newProductName)) {
        if (disableUnSyncUpdate && isOnlyExternal) {
          throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.DISABLED_MASTER_DATA_UPDATE);
        }
        currentMasterDataProduct.setProductName(newProductName);
        items = this.productHelperService.modifyItemNames(items, oldProductName, newProductName);
        itemChanged = true;
        productChanged = true;
      }
      boolean masterDataChanged =
          compareAndUpdateMasterDataProduct(currentMasterDataProduct, newMasterDataProduct, disableUnSyncUpdate,
              isOnlyExternal);
      productChanged = productChanged || masterDataChanged;
      boolean attributeChanged =
          compareAndUpdateMasterDataProductAttributes(currentMasterDataProduct, newMasterDataProduct,
              disableUnSyncUpdate, isOnlyExternal);
      if (attributeChanged) {
        String specificationDetail = this.productHelperService.generateSpecificationDetail(currProduct);
        currentMasterDataProduct.setSpecificationDetail(specificationDetail);
      }
      productChanged = productChanged || attributeChanged;
    }
    if (Objects.nonNull(currProduct.getProductScore())) {
      try {
        if (productChanged || itemChanged) {
          generateAndSetProductScore(requestId, currProduct, specialAttributeChanged, items, false, null);
        }
      } catch (Exception e) {
        LOG.error("Error while setting product score for : {}", product.getProductSku(), e);
      }
    }
    Product updatedProduct = currProduct;
    ProductScore updatedProductScore = new ProductScore();
    if (Objects.nonNull(updatedProduct.getProductScore())) {
      BeanUtils.copyProperties(updatedProduct.getProductScore(), updatedProductScore);
    }
    for (Item item : items) {
      ItemPickupPoint itemPickupPoint =
          itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID, item.getItemSku());
      item.setFreeSample(product.isFreeSample());
      if (off2OnActiveChanged) {
        item.setOff2OnChannelActive(product.isOff2OnChannelActive());
      }
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(false);
      Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
      itemViewConfigSet.add(itemViewConfig);
      if (freeSampleChanged && product.isFreeSample()) {
        item = this.productHelperService.updateItemViewConfigForExistingChannel(item, itemViewConfig);
        this.itemPickupPointService.updateItemViewConfigForExistingChannel(itemPickupPoint, itemViewConfigSet);
        itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
        cacheEvictItemService.evictFindL5ByItemSku(Constants.STORE_ID, itemPickupPoint.getItemSku());
      }
    }
    boolean preOrderChanged = isPreOrderChanged(product.getPreOrder(), currProduct.getPreOrder());
    if (preOrderChanged && Objects.nonNull(product.getPreOrder())) {
      currProduct.setPreOrder(product.getPreOrder());
      PreOrderVO preOrderVO = new PreOrderVO();
      BeanUtils.copyProperties(product.getPreOrder(), preOrderVO);
      productPreOrderStatus = ProductPreOrderStatus.builder().productCode(currProduct.getProductCode())
          .productSku(currProduct.getProductSku()).preOrder(preOrderVO).build();
    } else if (preOrderChanged) {
      productPreOrderStatus = ProductPreOrderStatus.builder().productCode(currProduct.getProductCode())
          .productSku(currProduct.getProductSku()).preOrder(null).build();
    }
    productChanged = productChanged || preOrderChanged;
    if (itemChanged) {
      // not publishing any ITEM_CHANGE events here as we are publishing those events in updateItem api for same changes based on contentChanged flag
      updatedProduct = (this.saveOperationService
          .saveProductAndItemsWithoutPublishingItemChange(new ProductAndItemsVO(currProduct, items))).getProduct();
    } else if (productChanged) {
      updatedProduct = this.saveOperationService.saveProduct(currProduct);
    }
    if (preOrderChanged) {
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_PREORDER_STATUS,
        productPreOrderStatus.getProductSku(), productPreOrderStatus);
    }
    if (!GdnObjects.equals(updatedProductScore, existingProductScore)) {
      try {
        LOG.info(
            "Updating the product score history change on productUpdate for : {} with oldValues : {}, newValues : {}",
            currProduct.getProductSku(), existingProductScore, updatedProductScore);
        this.productScoreHistoryL3Service
            .saveProductScoreHistoryL3(currProduct.getStoreId(), currProduct.getProductSku(), existingProductScore,
                updatedProductScore);
      } catch (Exception e) {
        LOG.error("Error while updating the product score history for :{}", currProduct.getProductSku());
      }
    }
    return updatedProduct;
  }

  private boolean isPreOrderChanged(PreOrder newPreOrder, PreOrder existingPreOrder) {
    if (Objects.isNull(newPreOrder) && Objects.isNull(existingPreOrder)) {
      return false;
    } else if (Objects.isNull(newPreOrder) || Objects.isNull(existingPreOrder)) {
      return true;
    } else if (newPreOrder.getIsPreOrder() != existingPreOrder.getIsPreOrder()) {
      return true;
    } else if ((StringUtils.isNotEmpty(newPreOrder.getPreOrderType()) && !StringUtils
        .equals(newPreOrder.getPreOrderType(), existingPreOrder.getPreOrderType()))) {
      return true;
    } else if ((Objects.nonNull(newPreOrder.getPreOrderValue()) && !newPreOrder.getPreOrderValue()
        .equals(existingPreOrder.getPreOrderValue()))) {
      return true;
    } else
      return Objects.nonNull(newPreOrder.getPreOrderDate()) && !newPreOrder.getPreOrderDate()
          .equals(existingPreOrder.getPreOrderDate());
  }

  private ProductDetailResponse generateAndSetProductScore(String requestId, Product currProduct,
      boolean specialAttributeChanged, List<Item> items, boolean isCombinedEditRequest,
      ProductDetailResponse productDetailResponse) throws Exception {
    String categoryCode = null;
    if (currProduct.isSynchronized()) {
      if (specialAttributeChanged || productScoreUpdateWithoutSpecialAttributesEnabled) {
        if(!isCombinedEditRequest) {
          productDetailResponse = productCategoryBaseOutbound.getProductDetailByProductCode(requestId, Constants.DEFAULT_USERNAME,
              currProduct.getProductCode());
        }
        categoryCode = productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
        List<AttributeScoreRequest> attributeScoreRequestList = CommonUtil
            .generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(productDetailResponse, currProduct);
        double recommendedAttributeScore =
            productScoreUtil.getRecommendedAttributeScore(attributeScoreRequestList, categoryCode);
        double currRecommendedAttributeScore = currProduct.getProductScore().getRecommendedAttributeScore();
        currProduct.getProductScore().setRecommendedAttributeScore(recommendedAttributeScore);
        currProduct.getProductScore().setTotalScore(
            currProduct.getProductScore().getTotalScore() - currRecommendedAttributeScore + recommendedAttributeScore);
        setProductScore(currProduct, CommonUtil.getProductScoreRequest(currProduct, productDetailResponse));
      }
    } else {
      categoryCode = Objects.isNull(currProduct.getMasterDataProduct().getMasterCatalog()) ?
          currProduct.getMasterCatalog().getCategory().getCategoryCode() :
          currProduct.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode();
      ProductScoreRequest productScoreRequest = ProductScoreRequest.builder().productCode(
          StringUtils.isNotEmpty(currProduct.getProductCode()) ? currProduct.getProductCode() : StringUtils.EMPTY)
          .brand(currProduct.getMasterDataProduct().getBrand()).description(
              Optional.ofNullable(currProduct.getMasterDataProduct().getDescription()).orElse(StringUtils.EMPTY)
                  .getBytes()).name(currProduct.getMasterDataProduct().getProductName())
          .uniqueSellingPoint(currProduct.getMasterDataProduct().getUniqueSellingPoint())
          .url(currProduct.getMasterDataProduct().getUrl()).categoryCode(categoryCode).itemRequests(new ArrayList<>())
          .synchronised(currProduct.isSynchronized())
          .build();
      if (StringUtils.isEmpty(currProduct.getProductCode())) {
        if (CollectionUtils.isEmpty(items) && !isCombinedEditRequest) {
          items = this.itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(currProduct.getStoreId(),
              currProduct.getProductSku());
        }
        productScoreRequest.setItemRequests(CommonUtil.setItemRequestsForNullProductCodeL4(items));
      }
      productScoreRequest
          .setProductAttributeRequests(CommonUtil.generateAttributeScoreRequestsForUnsyncL3(currProduct));
      setProductScore(currProduct, productScoreRequest);
    }
    return productDetailResponse;
  }

  private void setProductScore(Product currProduct, ProductScoreRequest productScoreRequest) throws Exception {
    ProductScoreVo productScoreVo = this.productScoreUtil.getProductScoreByProductScoreRequest(productScoreRequest);
    ProductScore productScore = currProduct.getProductScore();
    if (StringUtils.isEmpty(currProduct.getProductCode())) {
      BeanUtils.copyProperties(productScoreVo, productScore, "eanUpcScore", "variantCreatingScore");
    } else {
      BeanUtils.copyProperties(productScoreVo, productScore, "imageScore", "eanUpcScore", "variantCreatingScore");
    }
    productScore.setTotalScore(
        (productScore.getMandatoryAttributeScore() + productScore.getProductTitleScore() + productScore
            .getRecommendedAttributeScore() + productScore.getDescriptionScore() + productScore.getImageScore()
            + productScore.getRemainingAttributeScore() + productScore.getUspScore() + productScore
            .getVariantCreatingScore() + productScore.getVideoUrlScore() + productScore.getEanUpcScore())
            * ROUND_OFF_FACTOR / ROUND_OFF_FACTOR);
    currProduct.setProductScore(productScore);
  }

  private boolean compareAndUpdateMasterDataProductAttributes(MasterDataProduct currentMasterDataProduct,
      MasterDataProduct newMasterDataProduct, boolean disableUnSyncUpdate, boolean isOnlyExternal) {
    boolean attributeChanged = false;
    Map<String, MasterDataProductAttribute> attributeCodeAndMasterDataAttributeMap =
        currentMasterDataProduct.getMasterDataProductAttributes().stream().collect(Collectors
            .toMap(masterDataProductAttribute -> masterDataProductAttribute.getMasterDataAttribute().getAttributeCode(),
                Function.identity(), (oldValue, newValue) -> oldValue));
    Set<String> newAttributeCodeSet = new HashSet<>();
    for (MasterDataProductAttribute newProductAttribute : newMasterDataProduct.getMasterDataProductAttributes()) {
      String newAttributeCode = newProductAttribute.getMasterDataAttribute().getAttributeCode();
      newAttributeCodeSet.add(newAttributeCode);
      if (attributeCodeAndMasterDataAttributeMap.containsKey(newAttributeCode)
          && !attributeCodeAndMasterDataAttributeMap.get(newAttributeCode).getMasterDataAttribute()
          .isVariantCreation()) {
        for (MasterDataProductAttributeValue masterDataProductAttributeValue : newProductAttribute
            .getMasterDataProductAttributeValues()) {
          if (Objects.nonNull(masterDataProductAttributeValue.getDescriptiveAttributeValueType())) {
            if (masterDataProductAttributeValue.getDescriptiveAttributeValueType()
                .equals(DescriptiveAttributeValueType.PREDEFINED)) {
              if (Objects.isNull(
                  attributeCodeAndMasterDataAttributeMap.get(newAttributeCode).getMasterDataProductAttributeValues()
                      .get(0).getPredefinedAllowedAttributeValue())) {
                attributeChanged = true;
              } else if (!masterDataProductAttributeValue.getPredefinedAllowedAttributeValue().getValue().equals(
                  attributeCodeAndMasterDataAttributeMap.get(newAttributeCode).getMasterDataProductAttributeValues()
                      .get(0).getPredefinedAllowedAttributeValue().getValue())) {
                attributeChanged = true;
              }
            } else if (masterDataProductAttributeValue.getDescriptiveAttributeValueType()
                .equals(DescriptiveAttributeValueType.SINGLE)) {
              if (!masterDataProductAttributeValue.getDescriptiveAttributeValue().equals(
                  attributeCodeAndMasterDataAttributeMap.get(newAttributeCode).getMasterDataProductAttributeValues()
                      .get(0).getDescriptiveAttributeValue())) {
                attributeChanged = true;
              }
            }
          }
        }
        if (attributeChanged) {
          attributeCodeAndMasterDataAttributeMap.get(newAttributeCode)
              .setMasterDataProductAttributeValues(newProductAttribute.getMasterDataProductAttributeValues());
        }
      } else if (!attributeCodeAndMasterDataAttributeMap.containsKey(newAttributeCode)) {
        log.debug("Adding new attribute if eligible: {} ", newProductAttribute);
        if (!newProductAttribute.getMasterDataAttribute().isVariantCreation()) {
          attributeChanged = true;
          newProductAttribute.setMasterDataAttribute(this.masterDataAttributeService
              .getOrSaveNewMasterDataAttribute(newProductAttribute.getMasterDataAttribute()));
          List<MasterDataProductAttribute> masterDataProductAttributes =
              new ArrayList<>(currentMasterDataProduct.getMasterDataProductAttributes());
          masterDataProductAttributes.add(newProductAttribute);
          currentMasterDataProduct.setMasterDataProductAttributes(masterDataProductAttributes);
        }
      }
    }
    List<MasterDataProductAttribute> masterDataProductAttributes = new ArrayList<>();
    for (MasterDataProductAttribute currentMasterDataProductAttribute : currentMasterDataProduct
        .getMasterDataProductAttributes()) {
      if (newAttributeCodeSet.contains(currentMasterDataProductAttribute.getMasterDataAttribute().getAttributeCode())
          || isDefiningOrVariantCreatingAttribute(currentMasterDataProductAttribute.getMasterDataAttribute())) {
        masterDataProductAttributes.add(currentMasterDataProductAttribute);
      } else {
        attributeChanged = true;
      }
    }
    currentMasterDataProduct.setMasterDataProductAttributes(masterDataProductAttributes);
    if (attributeChanged && disableUnSyncUpdate && isOnlyExternal) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.DISABLED_MASTER_DATA_UPDATE);
    }
    return attributeChanged;
  }

  private boolean isDefiningOrVariantCreatingAttribute(MasterDataAttribute masterDataAttribute) {
    return MasterDataAttributeType.DEFINING_ATTRIBUTE.equals(masterDataAttribute.getAttributeType())
        || masterDataAttribute.isVariantCreation();
  }

  private boolean compareAndUpdateMasterDataProduct(MasterDataProduct currentMasterDataProduct,
      MasterDataProduct newMasterDataProduct, boolean disableUnSyncUpdate, boolean isOnlyExternal) {
    boolean masterDataProductChanged = false;
    if (currentMasterDataProduct.isActivated() != newMasterDataProduct.isActivated()) {
      currentMasterDataProduct.setActivated(newMasterDataProduct.isActivated());
    }
    if (!StringUtils.equals(currentMasterDataProduct.getBrand(), newMasterDataProduct.getBrand())) {
      currentMasterDataProduct.setBrand(newMasterDataProduct.getBrand());
      masterDataProductChanged = true;
    }
    if (!CommonUtil
        .compareProductTextField(currentMasterDataProduct.getDescription(), newMasterDataProduct.getDescription())) {
      currentMasterDataProduct.setDescription(newMasterDataProduct.getDescription());
      masterDataProductChanged = true;
    }
    if (currentMasterDataProduct.getWeight() != newMasterDataProduct.getWeight()
        && newMasterDataProduct.getWeight() != 0.0) {
      currentMasterDataProduct.setWeight(newMasterDataProduct.getWeight());
      masterDataProductChanged = true;
    }
    if (currentMasterDataProduct.getWidth() != newMasterDataProduct.getWidth()
        && newMasterDataProduct.getWidth() != 0.0) {
      currentMasterDataProduct.setWidth(newMasterDataProduct.getWidth());
      masterDataProductChanged = true;
    }
    if (currentMasterDataProduct.getHeight() != newMasterDataProduct.getHeight()
        && newMasterDataProduct.getHeight() != 0.0) {
      currentMasterDataProduct.setHeight(newMasterDataProduct.getHeight());
      masterDataProductChanged = true;
    }
    if (currentMasterDataProduct.getLength() != newMasterDataProduct.getLength()
        && newMasterDataProduct.getLength() != 0.0) {
      currentMasterDataProduct.setLength(newMasterDataProduct.getLength());
      masterDataProductChanged = true;
    }
    if (currentMasterDataProduct.getShippingWeight() != newMasterDataProduct.getShippingWeight()
        && newMasterDataProduct.getShippingWeight() != 0.0) {
      currentMasterDataProduct.setShippingWeight(newMasterDataProduct.getShippingWeight());
      masterDataProductChanged = true;
    }
    if (!StringUtils.equals(currentMasterDataProduct.getLongDescription(), newMasterDataProduct.getLongDescription())
        && StringUtils.isNotEmpty(newMasterDataProduct.getLongDescription())) {
      currentMasterDataProduct.setLongDescription(newMasterDataProduct.getLongDescription());
      masterDataProductChanged = true;
    }
    if (!StringUtils.equals(currentMasterDataProduct.getProductStory(), newMasterDataProduct.getProductStory())) {
      currentMasterDataProduct.setProductStory(newMasterDataProduct.getProductStory());
      masterDataProductChanged = true;
    }
    if (!CommonUtil.compareProductTextField(currentMasterDataProduct.getUniqueSellingPoint(),
        newMasterDataProduct.getUniqueSellingPoint())) {
      currentMasterDataProduct.setUniqueSellingPoint(newMasterDataProduct.getUniqueSellingPoint());
      masterDataProductChanged = true;
    }
    if (!StringUtils.equals(currentMasterDataProduct.getUom(), newMasterDataProduct.getUom()) && StringUtils
        .isNotEmpty(newMasterDataProduct.getUom())) {
      currentMasterDataProduct.setUom(newMasterDataProduct.getUom());
      masterDataProductChanged = true;
    }
    if (!StringUtils.equals(currentMasterDataProduct.getUrl(), newMasterDataProduct.getUrl())) {
      currentMasterDataProduct.setUrl(newMasterDataProduct.getUrl());
      masterDataProductChanged = true;
    }
    if (currentMasterDataProduct.isViewable() != newMasterDataProduct.isViewable()) {
      currentMasterDataProduct.setViewable(newMasterDataProduct.isViewable());
    }
    newMasterDataProduct.setMasterDataProductImages(
        Optional.ofNullable(newMasterDataProduct.getMasterDataProductImages()).orElse(new ArrayList<>()).stream().map(
            masterDataProductImage -> new MasterDataProductImage(masterDataProductImage.isMainImage(),
                masterDataProductImage.getLocationPath(), null, masterDataProductImage.getSequence()))
            .collect(Collectors.toList()));
    if (!CollectionUtils.isEmpty(newMasterDataProduct.getMasterDataProductImages()) && !GdnObjects
        .equals(newMasterDataProduct.getMasterDataProductImages().toArray(),
            currentMasterDataProduct.getMasterDataProductImages().toArray())) {
      currentMasterDataProduct.setMasterDataProductImages(newMasterDataProduct.getMasterDataProductImages());
      masterDataProductChanged = true;
    }
    if (masterDataProductChanged && disableUnSyncUpdate && isOnlyExternal) {
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.DISABLED_MASTER_DATA_UPDATE);
    }
    return masterDataProductChanged;
  }

  private List<Boolean> compareAndUpdateProductFields(Product product, Product currProduct) {
    boolean isProductChanged = false;
    boolean specialAttributeChanged = false;
    if (!currProduct.getProductType().equals(product.getProductType())) {
      currProduct.setProductType(product.getProductType());
      isProductChanged = true;
    }
    if (!StringUtils.equals(product.getSettlementType(), currProduct.getSettlementType())) {
      currProduct.setSettlementType(product.getSettlementType());
      isProductChanged = true;
    }
    if (currProduct.isInstallationRequired() != product.isInstallationRequired()) {
      currProduct.setInstallationRequired(product.isInstallationRequired());
      isProductChanged = true;
    }
    if (!currProduct.getProductSpecialAttributes().equals(product.getProductSpecialAttributes())) {
      currProduct.setProductSpecialAttributes(product.getProductSpecialAttributes());
      isProductChanged = true;
      specialAttributeChanged = true;
    }
    List<Boolean> booleanList = new ArrayList<>();
    booleanList.add(isProductChanged);
    booleanList.add(specialAttributeChanged);
    return booleanList;
  }

  private Map<String, Boolean> compareProductFlagsChanged(Product product, Product currProduct) {
    boolean off2OnActiveChanged = false;
    boolean freeSampleChanged = false;
    if (currProduct.isOff2OnChannelActive() != product.isOff2OnChannelActive()) {
      currProduct.setOff2OnChannelActive(product.isOff2OnChannelActive());
      off2OnActiveChanged = true;
    }
    if (currProduct.isFreeSample() != product.isFreeSample()) {
      currProduct.setFreeSample(product.isFreeSample());
      freeSampleChanged = true;
    }
    Map<String, Boolean> isFlagChangedMap = new HashMap<>();
    if(isPreOrderChanged(currProduct.getPreOrder(),product.getPreOrder())){
      isFlagChangedMap.put(Constants.PREORDER_DETAILS_CHANGED,true);
    }
    if(!StringUtils.equals(Optional.ofNullable(product.getSizeChartCode()).orElse(StringUtils.EMPTY),
      Optional.ofNullable(currProduct.getSizeChartCode()).orElse(StringUtils.EMPTY)) && product.isSizeChartChanged()){
      isFlagChangedMap.put(Constants.SIZE_CHART_CHANGED, true);
    }
    isFlagChangedMap.put(Constants.OFF_2ON_ACTIVE_CHANGED, off2OnActiveChanged);
    isFlagChangedMap.put(Constants.FREE_SAMPLE_CHANGED, freeSampleChanged);
    return isFlagChangedMap;
  }

  @Override
  public void updateCncActivatedByMerchantCodeL3(String storeId, String merchantCode, boolean cncActivated,
      String username) {
    this.productRepository.updateCncActivatedByMerchantCode(storeId, merchantCode, cncActivated, username);
    this.solrRepository.updateCncActivatedByMerchantCodeSolr(storeId, merchantCode, cncActivated);
  }

  @Override
  public void updateCncActivatedByProductSkuAndSolrL3(String storeId, Set<String> productSkuSet, boolean cncActivated,
      String username) {
    List<Product> products =
        this.productRepository.updateCncActivatedByProductSkusMarkForDeleteFalse(storeId, productSkuSet, cncActivated,
            username);
    for (Product product : products) {
      if (Objects.nonNull(product)) {
        cacheEvictHelperService.evictProductData(storeId, product);
        this.solrRepository.updateCncActivatedByProductSkuSetSolr(storeId, Set.of(product.getProductSku()), cncActivated,
          product.getMerchantCode());
      }
    }
  }

  @Override
  public boolean addProductAttribute(String storeId, String productCode,
      MasterDataProductAttribute masterDataProductAttribute) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode), ProductServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(masterDataProductAttribute != null, ProductServiceImpl.PRODUCT_ATTRIBUTE_CANNOT_BE_NULL);
    List<Product> products =
        this.productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    checkState(!CollectionUtils.isEmpty(products), ProductServiceImpl.PRODUCT_NOT_FOUND);
    for (Product product : products) {
      if (product.getMasterDataProduct() != null) {
        boolean exists = false;
        if (product.getMasterDataProduct().getMasterDataProductAttributes() != null) {
          for (MasterDataProductAttribute masterAttrbute : product.getMasterDataProduct()
              .getMasterDataProductAttributes()) {
            if (masterAttrbute.getMasterDataAttribute() != null & masterAttrbute.getMasterDataAttribute()
                .getAttributeCode().equals(masterDataProductAttribute.getMasterDataAttribute().getAttributeCode())) {
              exists = true;
              break;
            }
          }
        } else {
          product.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
        }

        if (!exists) {
          product.getMasterDataProduct().getMasterDataProductAttributes().add(masterDataProductAttribute);
          this.masterDataAttributeService.setAndSaveMasterDataAttributeProduct(product);
          this.saveOperationService.saveProduct(product);
        }
      }
    }
    return true;
  }

  @Override
  public boolean updateProductMasterCatalog(String storeId, String productCode, MasterCatalog masterCatalog)
      throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode), ProductServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(masterCatalog != null, ProductServiceImpl.MASTER_CATALOG_CANNOT_BE_NULL);

    List<Product> products =
        this.productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    checkState(!CollectionUtils.isEmpty(products), ProductServiceImpl.PRODUCT_NOT_FOUND);
    for (Product product : products) {
      if (product.getMasterCatalog() != null) {
        if (!product.getMasterCatalog().getCategory().getCategoryCode()
            .equals(masterCatalog.getCategory().getCategoryCode())) {
          product.setMasterCatalog(masterCatalog);
        }
      }
      if (product.getMasterDataProduct() != null) {
        if (!product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode()
            .equals(masterCatalog.getCategory().getCategoryCode())) {
          product.getMasterDataProduct().setMasterCatalog(masterCatalog);
        }
      }
      product = this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
          StringUtils.EMPTY, Collections.EMPTY_MAP);
      List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
      saveAndPublishService.publishListOfItems(items);
      this.productAndItemSolrIndexerService.updateSolrOnMasterCatalogChanges(product, items);
    }
    return true;
  }

  @Override
  public boolean updateProductSalesCatalogByProductCode(String storeId, String productCode, SalesCatalog salesCatalog,
      boolean replace) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode), ProductServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(salesCatalog != null, ProductServiceImpl.SALES_CATALOG_CANNOT_BE_NULL);

    List<Product> products =
        this.productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    checkState(!CollectionUtils.isEmpty(products), ProductServiceImpl.PRODUCT_NOT_FOUND);

    for (Product product : products) {
      if (replace) {
        product.setSalesCatalogs(Collections.singletonList(salesCatalog));
      } else {
        for (SalesCatalog currSalesCatalog : product.getSalesCatalogs()) {
          if (currSalesCatalog.getCatalogCode().equals(salesCatalog.getCatalogCode())) {
            Set<Category> setOfCategories = new LinkedHashSet<Category>();
            setOfCategories.addAll(currSalesCatalog.getListOfCategories());
            for (Category category : salesCatalog.getListOfCategories()) {
              setOfCategories.add(category);
            }
            currSalesCatalog.setListOfCategories(new ArrayList<>(setOfCategories));
          }
        }
      }
      this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
      List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
      productAndItemSolrIndexerService.updateSolrOnSalesCatalogChanges(product, items);
      saveAndPublishService.publishListOfItems(items);
    }
    return true;
  }

  @Override
  public boolean updateProductCatalogByProductCodeOnCategoryChange(String storeId, String productCode,
      ProductSalesCategoryMapping productSalesCategoryMapping,
      List<ProductCategoryDomainEventModel> productCategories, boolean bopisEligible) {
    List<Product> products =
        this.productRepository.findByStoreIdAndProductCode(storeId, productCode);
    if (CollectionUtils.isEmpty(products)) {
      return true;
    }
    List<Category> newSalesCatalogToBeMappedToProduct =
        getSalesCatalogFromCategoryCodeList(productSalesCategoryMapping.getNewSalesCategoryCodes());
    List<Category> newSalesAndUmkmCatalogToBeMappedToProduct = Stream
        .concat(newSalesCatalogToBeMappedToProduct.stream(),
            getSalesCatalogFromCategoryCodeList(productSalesCategoryMapping.getNewUmkmSalesCategoryCodes()).stream())
        .collect(Collectors.toList());
    List<Category> newB2bCatalogToBeMappedToProduct =
            getSalesCatalogFromCategoryCodeList(productSalesCategoryMapping.getNewB2bSalesCategoryCodes());
    for (Product product : products) {
      mapSalesCategoryToProduct(storeId, productSalesCategoryMapping, newSalesCatalogToBeMappedToProduct,
          newSalesAndUmkmCatalogToBeMappedToProduct, product, newB2bCatalogToBeMappedToProduct);
      setMasterCatalog(productCategories, product, bopisEligible);
      this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
      List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
      List<String> itemSkuList = new ArrayList<>();
      for (Item item : items) {
        if (!StringUtils.equals(product.getCategoryCode(), item.getCategoryCode())) {
          itemSkuList.add(item.getItemSku());
        }
      }
      if (CollectionUtils.isNotEmpty(itemSkuList)) {
        this.itemService.updateCategoryCodeByItemSkuList(product.getStoreId(), itemSkuList,
            productCategories.get(0).getCategory().getCategoryCode());
      }
      productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(product, items, false);
    }
    return true;
  }

  private void setMasterCatalog(List<ProductCategoryDomainEventModel> productCategories, Product product,
      boolean bopisEligible) {
    try {
      MasterCatalog masterCatalog = constructMasterCatalog(productCategories);
      if (Objects.nonNull(masterCatalog) && Objects.nonNull(masterCatalog.getCategory())) {
        if (Objects.nonNull(product.getMasterCatalog()) && !StringUtils
            .equals(product.getMasterCatalog().getCategory().getCategoryCode(),
                masterCatalog.getCategory().getCategoryCode())) {
          product.setMasterCatalog(masterCatalog);
        }
        if (Objects.nonNull(product.getMasterDataProduct()) && !StringUtils
            .equals(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode(),
                masterCatalog.getCategory().getCategoryCode())) {
          product.getMasterDataProduct().setMasterCatalog(masterCatalog);
        }
        if (!StringUtils.equals(product.getCategoryCode(), productCategories.get(0).getCategory().getCategoryCode())) {
          product.setCategoryCode(productCategories.get(0).getCategory().getCategoryCode());
          setBopisDimensionMissingTrueAndL5Offline(product, bopisEligible);
        }
      }
    } catch (Exception e) {
      log.error("Exception caught while building masterCatalog productCode : {}", product.getProductCode());
    }
  }

  private void setBopisDimensionMissingTrueAndL5Offline(Product product, boolean bopisEligible)
    throws JsonProcessingException {
    if (checksForBopisEligibility(product, bopisEligible)) {
      ProductAndL5MigrationRequest productAndL5MigrationRequest = CommonUtil.getProductAndL5MigrationRequest(product);
      migrateProductAndL5DetailByProductSku(product.getStoreId(), productAndL5MigrationRequest);
    }
  }

  private boolean checksForBopisEligibility(Product product, boolean bopisEligible) {
    return bopisCategoryActionOnInternalCategoryChangeSwitch && ProductType.BOPIS.equals(product.getProductType())
        && CommonUtil.fetchDimensionsMissingForBopisProduct(new Item(), getProfileResponse(product),
        bopisCategoryValidationMerchantTypes, !bopisEligible);
  }

  private ProfileResponse getProfileResponse(Product product) {
    return xbpOutbound.getBusinessPartnerDetails(product.getStoreId(), GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), product.getMerchantCode());
  }

  private MasterCatalog constructMasterCatalog(List<ProductCategoryDomainEventModel> productCategories) {
    if (CollectionUtils.isEmpty(productCategories) || Objects.isNull(productCategories.get(0).getCategory())) {
      return null;
    }
    CategoryDomainEventModel category = productCategories.get(0).getCategory();
    return new MasterCatalog(category.getCatalog().getCatalogCode(),
        new Category(category.getCategoryCode(), category.getCategoryCode()));
  }

  private void mapSalesCategoryToProduct(String storeId, ProductSalesCategoryMapping productSalesCategoryMapping,
      List<Category> newSalesCatalogToBeMappedToProduct, List<Category> newSalesAndUmkmCatalogToBeMappedToProduct,
      Product product, List<Category> newB2bSalesCatalogToBeMappedToProduct) {
    Pair<Boolean, Boolean> isUmkmAndIsB2bMerchant =
        businessPartnerService.isBusinessPartnerUmkmAndB2b(storeId, product.getMerchantCode());
    String[] salesCatalogCodeList = salesCatalogCodes.split(Constants.COMMA_DELIMITER);
    for (String currSalesCatalogCode : salesCatalogCodeList) {
      if (CollectionUtils.isNotEmpty(product.getSalesCatalogByCatalogCode(currSalesCatalogCode))) {
        product.getSalesCatalogByCatalogCode(currSalesCatalogCode).forEach(
            currSalesCatalog -> modifyExistingSalesCategory(productSalesCategoryMapping,
                newSalesCatalogToBeMappedToProduct, newSalesAndUmkmCatalogToBeMappedToProduct, product,
                isUmkmAndIsB2bMerchant.getLeft(), currSalesCatalog, newB2bSalesCatalogToBeMappedToProduct,
                isUmkmAndIsB2bMerchant.getRight()));
      } else {
        mapNewSalesCatalogToProduct(newSalesCatalogToBeMappedToProduct, newSalesAndUmkmCatalogToBeMappedToProduct,
            product, newB2bSalesCatalogToBeMappedToProduct, isUmkmAndIsB2bMerchant.getLeft(),
            isUmkmAndIsB2bMerchant.getRight(), currSalesCatalogCode);
      }
    }
  }

  private void mapNewSalesCatalogToProduct(List<Category> newSalesCatalogToBeMappedToProduct,
      List<Category> newSalesAndUmkmCatalogToBeMappedToProduct, Product product,
      List<Category> newB2bSalesCatalogToBeMappedToProduct, boolean isUmkmMerchant, boolean isB2bMerchant,
      String currSalesCatalogCode) {
    List<SalesCatalog> salesCatalogs = new ArrayList<>();
    if (currSalesCatalogCode.equals(salesCategoryCatalogCode) && isSalesCategoryEligible(
        newSalesCatalogToBeMappedToProduct, newSalesAndUmkmCatalogToBeMappedToProduct, isUmkmMerchant)) {
      salesCatalogs.add(new SalesCatalog(salesCategoryCatalogCode,
          isUmkmMerchant ? newSalesAndUmkmCatalogToBeMappedToProduct : newSalesCatalogToBeMappedToProduct));
    }
    boolean isB2bSalesCategoryEligible =
        currSalesCatalogCode.equals(b2bSalesCatalogCode) && isB2bMerchant && CollectionUtils.isNotEmpty(
            newB2bSalesCatalogToBeMappedToProduct);
    if (isB2bSalesCategoryEligible) {
      salesCatalogs.add(new SalesCatalog(b2bSalesCatalogCode, newB2bSalesCatalogToBeMappedToProduct));
    }
    setProductSalesCatalog(product, salesCatalogs);
  }

  private static void setProductSalesCatalog(Product product, List<SalesCatalog> salesCatalogs) {
    if (CollectionUtils.isNotEmpty(product.getAllSalesCatalogs())) {
      product.getAllSalesCatalogs().addAll(salesCatalogs);
    } else {
      product.setSalesCatalogs(salesCatalogs);
    }
  }

  private static boolean isSalesCategoryEligible(List<Category> newSalesCatalogToBeMappedToProduct,
      List<Category> newSalesAndUmkmCatalogToBeMappedToProduct, boolean isUmkmMerchant) {
    return (isUmkmMerchant && CollectionUtils.isNotEmpty(newSalesAndUmkmCatalogToBeMappedToProduct)) || (!isUmkmMerchant
        && CollectionUtils.isNotEmpty(newSalesCatalogToBeMappedToProduct));
  }

  private void modifyExistingSalesCategory(ProductSalesCategoryMapping productSalesCategoryMapping,
      List<Category> newSalesCatalogToBeMappedToProduct, List<Category> newSalesAndUmkmCatalogToBeMappedToProduct,
      Product product, boolean isUmkmMerchant, SalesCatalog currSalesCatalog,
      List<Category> newB2bCatalogToBeMappedToProduct, boolean isB2bMerchant) {
    if (salesCategoryCatalogCode.equals(currSalesCatalog.getCatalogCode())) {
      Set<Category> categoriesToBeAdded = new LinkedHashSet<>();
      modifyOldSalesCategory(productSalesCategoryMapping, currSalesCatalog, categoriesToBeAdded);
      categoriesToBeAdded.addAll(
          isUmkmMerchant ? newSalesAndUmkmCatalogToBeMappedToProduct : newSalesCatalogToBeMappedToProduct);
      if (categoriesToBeAdded.isEmpty()) {
        // Removing sales category mapping object from product if there are no sales category present
        product.getAllSalesCatalogs().removeAll(product.getSalesCatalogByCatalogCode(salesCategoryCatalogCode));
      } else {
        currSalesCatalog.setListOfCategories(new ArrayList<>(categoriesToBeAdded));
      }
    }
    if (b2bSalesCatalogCode.equals(currSalesCatalog.getCatalogCode())) {
      Set<Category> categoriesToBeAdded = new LinkedHashSet<>();
      modifyB2bOldSalesCategory(productSalesCategoryMapping, currSalesCatalog, categoriesToBeAdded);
      categoriesToBeAdded.addAll(isB2bMerchant ? newB2bCatalogToBeMappedToProduct : new ArrayList<>());
      if (categoriesToBeAdded.isEmpty()) {
        // Removing b2b sales category mapping object from product if there are no b2b sales category present
        product.getAllSalesCatalogs().removeAll(product.getSalesCatalogByCatalogCode(b2bSalesCatalogCode));
      } else {
        currSalesCatalog.setListOfCategories(new ArrayList<>(categoriesToBeAdded));
      }
    }
  }

  private void modifyOldSalesCategory(ProductSalesCategoryMapping productSalesCategoryMapping,
      SalesCatalog currSalesCatalog, Set<Category> categoriesToBeAdded) {
    if (CollectionUtils.isNotEmpty(productSalesCategoryMapping.getOldSalesCategoryCodes())) {
      for (Category category : currSalesCatalog.getListOfCategories()) {
        if (!productSalesCategoryMapping.getOldSalesCategoryCodes().contains(category.getCategoryCode())) {
          // Remove old category's sales mapping from product
          categoriesToBeAdded.add(category);
        }
      }
    } else {
      // If Old sales category is null, directly add new sales category + existing sales category mapping
      categoriesToBeAdded.addAll(currSalesCatalog.getListOfCategories());
    }
  }

  private void modifyB2bOldSalesCategory(ProductSalesCategoryMapping productSalesCategoryMapping,
      SalesCatalog currSalesCatalog, Set<Category> categoriesToBeAdded) {
    if (CollectionUtils.isNotEmpty(productSalesCategoryMapping.getOldB2bSalesCategoryCodes())) {
      for (Category category : currSalesCatalog.getListOfCategories()) {
        if (!productSalesCategoryMapping.getOldB2bSalesCategoryCodes().contains(category.getCategoryCode())) {
          // Remove old b2b category's sales mapping from product
          categoriesToBeAdded.add(category);
        }
      }
    } else {
      // If Old b2b sales category is null, directly add new b2b sales category + existing b2b sales category mapping
      categoriesToBeAdded.addAll(currSalesCatalog.getListOfCategories());
    }
  }

  private List<Category> getSalesCatalogFromCategoryCodeList(List<String> categoryCodeList) {
    return Optional.ofNullable(categoryCodeList).orElseGet(Collections::emptyList).stream()
        .map(categoryCode -> new Category(categoryCode, categoryCode)).collect(Collectors.toList());
  }

  @Override
  public void publishAllProducts(String storeId) {
    LOG.info("Start publishAllProducts");
    try (Stream<Product> productStream = this.productRepository.streamAllByStoreId(storeId)) {
      this.saveAndPublishService.publishStreamOfProducts(productStream);
      LOG.info("Finish publishAllProducts");
    } catch (Exception e) {
      LOG.error("Failed publishAllProducts with error {}", e);
    }
  }

  @Override
  public void republishProductsToAgp(String storeId, List<String> productSkus) {
    LOG.info("Start republishProductsToAgp");
    try (Stream<Product> productStream = this.productRepository
        .streamAllByStoreIdAndProductSkuIn(storeId, productSkus)) {
      this.saveAndPublishService.publishStreamOfProducts(productStream);
      LOG.info("Finish republishProductsToAgp");
    } catch (Exception e) {
      LOG.error("Failed republishProductsToAgp with error {}", e);
    }
  }

  @Override
  public List<Product> updateProductSalesCategory(String storeId, String requestId, String username,
      List<String> productSkus, String catalogCode, String oldCategoryCode, String newCategoryCode) {
    CategoryDetailResponse oldCategoryDetailResponse = null;
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(!CollectionUtils.isEmpty(productSkus), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(catalogCode), ProductServiceImpl.CATALOG_CODE_MUST_NOT_BE_BLANK);
    String activity = StringUtils.EMPTY;
    // TODO : need to find another workaround, don't return all data, but solr need all the data
    List<Product> products =
        this.productRepository.findByStoreIdAndProductSkusAndMarkForDeleteFalse(storeId, productSkus);
    CategoryDetailResponse newCategoryDetailResponse = null;
    if (StringUtils.isNotEmpty(newCategoryCode)) {
      newCategoryDetailResponse =
          this.productCategoryBaseOutbound.getCategoryDetailByCategoryCode(requestId, username, newCategoryCode);
    }
    if (StringUtils.isNotEmpty(oldCategoryCode)) {
      oldCategoryDetailResponse =
          this.productCategoryBaseOutbound.getCategoryDetailByCategoryCode(requestId, username, oldCategoryCode);
    }
    List<Item> itemList = new ArrayList<>();
    for (Product product : products) {
      Set<String> productChannel = new HashSet<>();
      Product result =
          this.updateSalesCategory(storeId, itemList, catalogCode, oldCategoryCode, newCategoryCode, product,
              newCategoryDetailResponse);

      ProductChange productChange = new ProductChange();
      BeanUtils.copyProperties(result, productChange);
      CommonUtil.populateProductType(result, productChange);
      productChange.setMultiVariant(product.getDefiningAttributes().size() > Constants.SINGLE_VARIANT_L4_COUNT);
      setDistributionMappingStatusInEvent(result, productChange);
      if (Objects.nonNull(result.getMasterDataProduct())) {
        com.gdn.x.product.domain.event.model.MasterDataProduct masterDataProduct =
            new com.gdn.x.product.domain.event.model.MasterDataProduct();
        BeanUtils.copyProperties(result.getMasterDataProduct(), masterDataProduct);
        productChange.setMasterDataProduct(masterDataProduct);
      }
      if (CollectionUtils.isNotEmpty(result.getAllSalesCatalogs())) {
        for (SalesCatalog salesCatalog : result.getAllSalesCatalogs()) {
          productChange.getSalesCatalogs()
              .add(gdnMapper.deepCopy(salesCatalog, com.gdn.x.product.domain.event.model.SalesCatalog.class));
        }
      }
      if (result.isB2bActivated()) {
        productChannel.add(Constants.B2B);
      }
      if (result.getB2cActivated()) {
        productChannel.add(Constants.RETAIL);
      }
      productChange.setProductChannel(productChannel);
      CommonUtil.setBrandAndCategoryCodeAndPreOrderForOdoo(product, productChange);
      this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
        productChange.getProductSku(), productChange);
      if (StringUtils.isEmpty(oldCategoryCode)) {
        activity = ProductCenterActivity.COPY.name();
      } else if (StringUtils.isEmpty(newCategoryCode)) {
        activity = ProductCenterActivity.DELETE.name();
      } else {
        activity = ProductCenterActivity.MOVE.name();
      }
      this.productCenterHistoryService
          .saveProductCenterHistory(product.getProductSku(), ProductCenterActivity.valueOf(activity), requestId,
              Objects.nonNull(oldCategoryDetailResponse) ? oldCategoryDetailResponse.getName() : StringUtils.EMPTY,
              Objects.nonNull(newCategoryDetailResponse) ? newCategoryDetailResponse.getName() : StringUtils.EMPTY);
    }
    productAndItemSolrIndexerService.updateSolrOnSalesCatalogChangesForProductList(products, itemList);
    return products;
  }

  private Product updateSalesCategory(String storeId, List<Item> itemList, String catalogCode, String oldCategoryCode,
      String newCategoryCode, Product product, CategoryDetailResponse categoryDetailResponse) {
    SalesCatalog salesCatalog = null;
    // get sales catalog from product
    for (SalesCatalog catalog : product.getSalesCatalogs()) {
      if (catalog.getCatalogCode().equals(catalogCode)) {
        salesCatalog = catalog;
        break;
      }
    }
    // if specified sales catalog is null then create new
    if (Objects.isNull(salesCatalog)) {
      if (StringUtils.isEmpty(newCategoryCode)) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
            ProductServiceImpl.BOTH_SALES_CATALOG_AND_NEW_CATEGORY_CODE_MUST_NOT_BE_NULL);
      } else {
        salesCatalog = new SalesCatalog(catalogCode);
        product.getAllSalesCatalogs().add(salesCatalog);
      }
    }

    // get index of old and new category on list category product
    int oldCategoryIndex = -1;
    int newCategoryIndex = -1;
    for (int i = 0; i < salesCatalog.getListOfCategories().size(); i++) {
      String categoryCode = salesCatalog.getListOfCategories().get(i).getCategoryCode();
      if (categoryCode.equals(oldCategoryCode)) {
        oldCategoryIndex = i;
      }
      if (categoryCode.equals(newCategoryCode)) {
        newCategoryIndex = i;
      }
    }

    if (StringUtils.isEmpty(newCategoryCode)) {
      if (StringUtils.isEmpty(oldCategoryCode)) {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
            ProductServiceImpl.BOTH_OLD_AND_NEW_CATEGORY_CODE_SHOULD_NOT_BE_NULL);
      } else {
        if (oldCategoryIndex != -1) {
          salesCatalog.getListOfCategories().remove(oldCategoryIndex);
          if (salesCatalog.getListOfCategories().isEmpty()) {
            product.getAllSalesCatalogs().remove(salesCatalog);
          }
        } else {
          // if catalog to be removed is not found, just ok.
        }
      }
    } else {
      // jika newcategory nya ada
      if (newCategoryIndex == -1) {
        // dan tidak ada dalam list product
        if (Objects.isNull(categoryDetailResponse)) {
          throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
              ProductServiceImpl.CATEGORY_NOT_FOUND_ON_PCB);
        }
        Category category = new Category(newCategoryCode, newCategoryCode);
        if (StringUtils.isNotEmpty(oldCategoryCode)) {
          // jika oldcategory ada, maka move
          salesCatalog.getListOfCategories().remove(oldCategoryIndex);
        }
        salesCatalog.getListOfCategories().add(category);
      }
    }
    product.setProductCenterUpdatedDate(new Date());
    Product result = this.productRepository
        .updateSalesCatalog(storeId, product.getProductSku(), product.getSalesCatalogs(),
            product.getProductCenterUpdatedDate());
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
    itemList.addAll(items);
    CommonUtil.addUpdatedFieldsInItem(itemList, Collections.singletonList(UpdatedFields.CATEGORY_UPDATE));
    saveAndPublishService.publishListOfItems(items);
    this.cacheEvictHelperService.evictProductData(storeId, result);
    return result;
  }

  @Override
  public boolean addProductAttributeByProductSku(String storeId, String productSku,
      MasterDataProductAttribute masterDataProductAttribute) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(masterDataProductAttribute != null, ProductServiceImpl.PRODUCT_ATTRIBUTE_CANNOT_BE_NULL);
    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    if (product.getMasterDataProduct() != null) {
      boolean exists = false;
      if (product.getMasterDataProduct().getMasterDataProductAttributes() != null) {
        for (MasterDataProductAttribute masterAttrbute : product.getMasterDataProduct()
            .getMasterDataProductAttributes()) {
          if (masterAttrbute.getMasterDataAttribute() != null & masterAttrbute.getMasterDataAttribute()
              .getAttributeCode().equals(masterDataProductAttribute.getMasterDataAttribute().getAttributeCode())) {
            exists = true;
            break;
          }
        }
      } else {
        product.getMasterDataProduct().setMasterDataProductAttributes(new ArrayList<>());
      }
      if (!exists) {
        product.getMasterDataProduct().getMasterDataProductAttributes().add(masterDataProductAttribute);
        this.masterDataAttributeService.setAndSaveMasterDataAttributeProduct(product);
        this.saveOperationService.saveProduct(product);
      }
    }
    return true;
  }

  @Override
  public boolean updateProductSalesCatalogByProductSku(String storeId, String productSku, SalesCatalog salesCatalog,
      boolean replace) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(salesCatalog != null, ProductServiceImpl.SALES_CATALOG_CANNOT_BE_NULL);

    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);

    if (replace) {
      product.setSalesCatalogs(Collections.singletonList(salesCatalog));
    } else {
      for (SalesCatalog currSalesCatalog : product.getSalesCatalogs()) {
        if (currSalesCatalog.getCatalogCode().equals(salesCatalog.getCatalogCode())) {
          Set<Category> setOfCategories = new LinkedHashSet<Category>();
          setOfCategories.addAll(currSalesCatalog.getListOfCategories());
          for (Category category : salesCatalog.getListOfCategories()) {
            setOfCategories.add(category);
          }
          currSalesCatalog.setListOfCategories(new ArrayList<Category>(setOfCategories));
        }
      }
    }
    this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    productAndItemSolrIndexerService.updateSolrOnSalesCatalogChanges(product, items);
    saveAndPublishService.publishListOfItems(items);
    return true;
  }

  @Override
  public boolean updateProductMasterCatalogByProductSku(String storeId, String productSku,
      MasterCatalog masterCatalog) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(masterCatalog != null, ProductServiceImpl.MASTER_CATALOG_CANNOT_BE_NULL);

    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    if (product.getMasterCatalog() != null) {
      if (!product.getMasterCatalog().getCategory().getCategoryCode()
          .equals(masterCatalog.getCategory().getCategoryCode())) {
        product.setMasterCatalog(masterCatalog);
      }
    }
    if (product.getMasterDataProduct() != null) {
      if (!product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode()
          .equals(masterCatalog.getCategory().getCategoryCode())) {
        product.getMasterDataProduct().setMasterCatalog(masterCatalog);
      }
    }
    product = this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    this.productAndItemSolrIndexerService.updateSolrOnMasterCatalogChanges(product, items);
    saveAndPublishService.publishListOfItems(items);
    return true;
  }

  @Override
  public boolean updateProductMasterCategory(String storeId, String productSku, String categoryCode) throws Exception {
    checkArgument(StringUtils.isNotBlank(productSku), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(storeId), STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(categoryCode), CATEGORY_CODE_MUST_NOT_BE_BLANK);
    Product product =
        this.productRepository.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku, false);
    checkState(product != null, ProductServiceImpl.PRODUCT_NOT_FOUND);
    if (product.getMasterCatalog() != null) {
      if (null != product.getMasterCatalog().getCategory() && !categoryCode
          .equals(product.getMasterCatalog().getCategory().getCategoryCode())) {
        product.getMasterCatalog().getCategory().setCategoryCode(categoryCode);
        product.getMasterCatalog().getCategory().setCatgroupId(categoryCode);
      }
    }
    if (product.getMasterDataProduct() != null) {
      if (null != product.getMasterDataProduct().getMasterCatalog() && null != product.getMasterDataProduct()
          .getMasterCatalog().getCategory() && !categoryCode
          .equals(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode())) {
        product.getMasterDataProduct().getMasterCatalog().getCategory().setCategoryCode(categoryCode);
        product.getMasterDataProduct().getMasterCatalog().getCategory().setCatgroupId(categoryCode);
      }
    }
    product = this.saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST,
        StringUtils.EMPTY, Collections.EMPTY_MAP);
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
    this.productAndItemSolrIndexerService.updateSolrOnMasterCatalogChanges(product, items);
    saveAndPublishService.publishListOfItems(items);
    return false;
  }


  @Override
  @Async
  public void processCategoryToProductSkuMapping(CategoryProductSkuMappingRequest mappingRequest) throws Exception {
    try {
      this.updateProductMasterCategory(mappingRequest.getStoreId(), mappingRequest.getProductSku(),
          mappingRequest.getCategoryCode());
      mappingRequest.setStatus(COMPLETED);
    } catch (Exception ex) {
      LOG.error("error occurred while mapping categoryCode : {} to productSku : {} for recateId : {}, error is : ",
          mappingRequest.getCategoryCode(), mappingRequest.getProductSku(), mappingRequest.getRecatId(), ex);
      mappingRequest.setStatus(FAILED);
    }
    LOG.debug("publishing kafka topic : {} with message : {}",
        ProductDomainEventName.CATEGORY_TO_PRODUCT_SKU_SAVE_EVENT, mappingRequest);
    kafkaPublisher.send(ProductDomainEventName.CATEGORY_TO_PRODUCT_SKU_SAVE_EVENT,
      mappingRequest.getCategoryCode(), mappingRequest);
  }


  @Override
  @Async
  public void processProductSkuToSalesCatalogMapping(ProductSkuToSalesCatalogMappingRequest mappingRequest)
      throws Exception {
    try {
      mappingRequest.setStatus(FAILED);
      checkArgument(StringUtils.isNotBlank(mappingRequest.getProductSku()), PRODUCT_SKU_MUST_NOT_BE_BLANK);
      List<String> productSkuList = new ArrayList<>();
      productSkuList.add(mappingRequest.getProductSku());
      List<Product> products =
          this.updateProductSalesCategory(mappingRequest.getStoreId(), mappingRequest.getRequestId(),
              mappingRequest.getUsername(), productSkuList, mappingRequest.getSalesCatalog(), null,
              mappingRequest.getSalesCategory());
      if (!CollectionUtils.isEmpty(products)) {
        mappingRequest.setStatus(COMPLETED);
      }
    } catch (Exception ex) {
      LOG.error("error occurred while mapping sales category : {} and salesCatalog : {} to productSku : {} for "
              + "recateId : {}, error is : ", mappingRequest.getSalesCategory(), mappingRequest.getSalesCatalog(),
          mappingRequest.getProductSku(), mappingRequest.getRecatId(), ex);
    }
    LOG.debug("publishing kafka topic : {} with message : {}",
        ProductDomainEventName.PRODUCT_SKU_TO_SALES_CATALOG_SAVE_EVENT, mappingRequest);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_TO_SALES_CATALOG_SAVE_EVENT,
      mappingRequest.getProductSku(), mappingRequest);
  }

  @Override
  public List<Product> findByStoreIdAndProductSkuIn(String storeId, List<String> skus) {
    return productRepository.findByStoreIdAndProductSkuIn(storeId, skus);
  }

  @Override
  public Product findByStoreIdAndProductSku(String storeId, String productSku) {
    return productRepository.findByStoreIdAndProductSku(storeId, productSku);
  }

  @Override
  public List<Product> findByStoreIdAndProductCodeAndMerchantCode(String storeId, String productCode,
      String merchantCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode), ProductServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(merchantCode), ProductServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    return productRepository.findByStoreIdAndProductCodeAndMerchantCode(storeId, productCode, merchantCode);
  }

  @Override
  public List<Product> findByStoreIdAndProductCodeInAndMarkForDeleteFalse(String storeId, Set<String> productCodes) {
    return productRepository.findByStoreIdAndProductCodeInAndMarkForDeleteFalse(storeId, productCodes);
  }

  @Override
  public List<Product> findByStoreIdAndProductCode(String storeId, String productCode) {
    return productRepository.findByStoreIdAndProductCode(storeId, productCode);
  }

  @Override
  public void toggleSuspensionProduct(String storeId, String productSku, String username, boolean suspendProduct,
      String requestId) throws Exception {
    List<String> productChangeEventTypes = new ArrayList<>();
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(username), ProductServiceImpl.USERNAME_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    Product product = findByStoreIdAndProductSku(storeId, productSku);
    Optional.ofNullable(product)
        .orElseThrow(() -> new ApplicationException(ErrorCategory.DATA_NOT_FOUND, PRODUCT_NOT_FOUND));

    if ((product.isMarkForDelete() && suspendProduct) || (!product.isMarkForDelete() && !suspendProduct)) {
      LOG.error("Product with productSku : {} cannot be suspended/Reativated. Product markForDelete : {}", productSku,
          product.isMarkForDelete());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE);
    }
    if (product.isSuspended() == suspendProduct) {
      LOG.warn("Product with productSku : {} already in the desired state. Product suspended : {}", productSku,
          product.isSuspended());
      return;
    } else {
      productChangeEventTypes.add(ProductChangeEventType.SUSPEND_FLAG_CHANGE);
      productChangeEventTypes.add(ProductChangeEventType.ARCHIVE_FLAG_CHANGE);
    }

    if (!suspendProduct && StringUtils.isNotBlank(product.getProductCode())) {
      fetchMasterDataToSetMasterAndSalesCatalog(storeId, requestId, username, product);
    }
    Pair<Map<String, Boolean>, List<Item>> archiveItemMapAndItemsPair =
        itemService.suspendItems(storeId, productSku, suspendProduct);
    Map<String, Boolean> archiveItemMap = archiveItemMapAndItemsPair.getLeft();
    saveProduct(product, username, suspendProduct, suspendProduct, productChangeEventTypes);
    if (!suspendProduct) {
      if (product.isSynchronized()) {
        productAndItemSolrIndexerService.applyProduct(product, false);
      } else {
        List<Item> items = this.itemService.findItemsByStoreIdAndProductSku(storeId, productSku);
        productAndItemSolrIndexerService.updateSolrOnMasterCatalogChanges(product, items);
        productAndItemSolrIndexerService.updateSolrOnSalesCatalogChanges(product, items);
      }
    }
    productAndItemSolrIndexerService.updateItemsInSolr(storeId, productSku, suspendProduct, archiveItemMap,
        product.isArchived(), archiveItemMapAndItemsPair.getRight(), product.getMerchantCode());
  }

  private void saveProduct(Product product, String username, boolean markForDelete, boolean isSuspended,
      List<String> productChangeEventTypes) {
    product.setMarkForDelete(markForDelete);
    product.setSuspended(isSuspended);
    product.setUpdatedBy(username);
    product.setUpdatedDate(Calendar.getInstance().getTime());
    if (isSuspended) {
      product.setArchivedBeforeSuspension(product.isArchived());
      product.setArchived(true);
    } else {
      product.setArchived(product.isArchivedBeforeSuspension());
    }
    this.saveOperationService.saveProductWithoutUpdatingSolr(product, productChangeEventTypes, StringUtils.EMPTY, Collections.EMPTY_MAP);
  }

  private void fetchMasterDataToSetMasterAndSalesCatalog(String storeId, String requestId, String username,
      Product product) throws Exception {
    ProductDetailResponse productDetailResponse =
        masterDataService.getProductDetailFromMasterData(username, requestId, product.getProductCode());
    ProductCategoryResponse masterCategory =
        Optional.ofNullable(productDetailResponse.getProductCategoryResponses()).orElse(new ArrayList<>()).stream()
            .filter(productCategoryResponse -> (
                !salesCategoryCatalogCode.equals(productCategoryResponse.getCategory().getCatalog().getCatalogCode())
                    && StringUtils.isNotEmpty(productCategoryResponse.getCategory().getCatalog().getCatalogCode())))
            .findFirst().orElse(new ProductCategoryResponse());
    product.setMasterCatalog(CommonUtil.constructMasterCatalog(masterCategory));
    CommonUtil.updateCurationStatusOfProduct(product, masterCategory);
    if (StringUtils.isNotEmpty(product.getProductCode())) {
      GdnRestSingleResponse<ProductSalesCategoryMappingResponse> salesCategoryResponse =
          this.productCategoryBaseOutbound.getSalesCategoryMappingByProductCode(storeId, requestId, username,
              product.getProductCode(), !CurationStatus.APPROVED.equals(product.getCurationStatus()));
      log.info("Sales category mapping response of the product : {} is {} ", product.getProductSku(),
          salesCategoryResponse);
      if (!ResponseHelper.isValid(salesCategoryResponse)) {
        throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, FETCH_SALES_CATALOG_FAILED);
      }
      setSalesCategoryMappingToProduct(storeId, salesCategoryResponse.getValue(), product);
    }
  }


  private void setSalesCategoryMappingToProduct(String storeId, String requestId, String username, Product product) {
    GdnRestSingleResponse<ProductSalesCategoryMappingResponse> salesCategoryResponse =
        this.productCategoryBaseOutbound
            .getSalesCategoryMappingByProductCode(storeId, requestId, username, product.getProductCode(),
                !CurationStatus.APPROVED.equals(product.getCurationStatus()));
    log.info("Sales category mapping response of the product : {} is {} ", product.getProductSku(),
        salesCategoryResponse);
    if (!ResponseHelper.isValid(salesCategoryResponse)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, FETCH_SALES_CATALOG_FAILED);
    }
    setSalesCategoryMappingToProduct(storeId, salesCategoryResponse.getValue(), product);
  }

  private void setSalesCategoryMappingToProduct(String storeId,
      ProductSalesCategoryMappingResponse productSalesCategoryMappingResponse, Product product) {
    if (CollectionUtils.isNotEmpty(productSalesCategoryMappingResponse.getOldSalesCategoryCodes()) || CollectionUtils
        .isNotEmpty(productSalesCategoryMappingResponse.getNewSalesCategoryCodes()) || CollectionUtils
        .isNotEmpty(productSalesCategoryMappingResponse.getNewUmkmSalesCategoryCodes())) {
      ProductSalesCategoryMapping productSalesCategoryMapping = new ProductSalesCategoryMapping();
      BeanUtils.copyProperties(productSalesCategoryMappingResponse, productSalesCategoryMapping);
      List<Category> newSalesCatalogToBeMappedToProduct =
          getSalesCatalogFromCategoryCodeList(productSalesCategoryMapping.getNewSalesCategoryCodes());
      List<Category> newSalesAndUmkmCatalogToBeMappedToProduct = Stream
          .concat(newSalesCatalogToBeMappedToProduct.stream(),
              getSalesCatalogFromCategoryCodeList(productSalesCategoryMapping.getNewUmkmSalesCategoryCodes()).stream())
          .collect(Collectors.toList());
      List<Category> newB2bCatalogToBeMappedToProduct =
          getSalesCatalogFromCategoryCodeList(productSalesCategoryMapping.getNewB2bSalesCategoryCodes());
      mapSalesCategoryToProduct(storeId, productSalesCategoryMapping, newSalesCatalogToBeMappedToProduct,
          newSalesAndUmkmCatalogToBeMappedToProduct, product, newB2bCatalogToBeMappedToProduct);
    }
  }

  @Override
  public Product saveProductWithoutUpdatingSolr(Product product, List<String> productChangeEventTypes,
      String productPublishSourceDeletePickupPoint) {
    return this.saveOperationService.saveProductWithoutUpdatingSolr(product, productChangeEventTypes,
        productPublishSourceDeletePickupPoint, Collections.EMPTY_MAP);
  }

  @Override
  public Product saveProductHalalConfigAndCaptureHistory(Product product,
      HalalHistoryUpdateEventModel halalHistoryUpdateEventModel, String storeId) {
    Product updatedProduct = this.saveOperationService.saveProduct(product);
    kafkaPublisher.send(ProductDomainEventName.HALAL_HISTORY_UPDATE, product.getProductSku(),
        halalHistoryUpdateEventModel);
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
    if (!halalHistoryUpdateEventModel.getPreviousValue().equals(halalHistoryUpdateEventModel.getCurrentValue())) {
      boolean halalProduct = Boolean.parseBoolean(halalHistoryUpdateEventModel.getCurrentValue());
      if(halaConfigUpdateEventTypeSwitch){
        items.forEach(item -> item.getItemChangeEventTypes().add(ItemChangeEventType.HALAL_CONFIG_CHANGE));
      }
      saveAndPublishService.publishListOfItemsForHalaConfigChange(items, halalProduct);
    } else {
      saveAndPublishService.publishListOfItems(items);
    }
    return updatedProduct;
  }

  @Override
  public void checkProductAndItemsForForceReview(List<Product> productList, List<Item> itemList) {
    CommonUtil.checkProductsForForceReview(productList);
    CommonUtil.checkItemsForForceReview(itemList);
  }

  @Override
  public void updateBrandForUnsyncProducts(String storeId, ProductDomainEventModel message) {
    String newBrandValue = null;
    String newBrandCode = null;
    if (CollectionUtils.isNotEmpty(message.getProductAttributes())) {
      for (ProductAttributeDomainEventModel productAttributeModel : message.getProductAttributes()) {
        if (BRAND.equalsIgnoreCase(productAttributeModel.getAttribute().getName())) {
          if (Objects
              .nonNull(productAttributeModel.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue())) {
            newBrandValue =
                productAttributeModel.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
                    .getValue();
            newBrandCode = productAttributeModel.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue()
                .getPredefinedAllowedAttributeCode();
          }
          break;
        }
      }
    }
    if (StringUtils.isEmpty(newBrandValue) || StringUtils.isEmpty(newBrandCode)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessages.NEW_BRAND_VALUE_OR_CODE_MUST_NOT_BE_NULL);
    }
    List<Product> products = this.productRepository.findByStoreIdAndProductCode(storeId, message.getProductCode());
    checkState(!CollectionUtils.isEmpty(products), ProductServiceImpl.PRODUCT_NOT_FOUND);

    updateUnsyncProductAndItemBrand(storeId, message, newBrandValue, newBrandCode, products);
  }

  private void updateUnsyncProductAndItemBrand(String storeId, ProductDomainEventModel message, String newBrandValue,
      String newBrandCode, List<Product> products) {
    for (Product product : products) {
      if (!product.isSynchronized() && Objects.nonNull(product.getMasterDataProduct())) {
        product.getMasterDataProduct().setBrand(message.getBrand());
        if (CollectionUtils.isNotEmpty(product.getMasterDataProduct().getMasterDataProductAttributes())) {
          for (MasterDataProductAttribute masterDataProductAttribute : product.getMasterDataProduct()
              .getMasterDataProductAttributes()) {
            if (BRAND.equalsIgnoreCase(masterDataProductAttribute.getMasterDataAttribute().getAttributeName())
                && CollectionUtils.isNotEmpty(masterDataProductAttribute.getMasterDataProductAttributeValues())) {
              for (MasterDataProductAttributeValue masterDataProductAttributeValue : masterDataProductAttribute
                  .getMasterDataProductAttributeValues()) {
                if (Objects.nonNull(masterDataProductAttributeValue.getPredefinedAllowedAttributeValue())) {
                  masterDataProductAttributeValue.getPredefinedAllowedAttributeValue().setValue(newBrandValue);
                  masterDataProductAttributeValue.getPredefinedAllowedAttributeValue()
                      .setPredefinedAllowedAttributeCode(newBrandCode);
                }
              }
              break;
            }
          }
        }
        List<Item> items =
            this.cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
        updateItemBrand(newBrandValue, items);
        this.saveOperationService.saveProductAndItems(new ProductAndItemsVO(product, items), new ArrayList<>());
      }
    }
  }

  private void updateItemBrand(String newBrandValue, List<Item> items) {
    if (CollectionUtils.isNotEmpty(items)) {
      for (Item item : items) {
        if (Objects.nonNull(item.getMasterDataItem()) && CollectionUtils
            .isNotEmpty(item.getMasterDataItem().getMasterDataItemAttributeValues())) {
          for (MasterDataItemAttributeValue masterDataItemAttributeValue : item.getMasterDataItem()
              .getMasterDataItemAttributeValues()) {
            if (BRAND.equalsIgnoreCase(masterDataItemAttributeValue.getMasterDataAttribute().getAttributeName())) {
              masterDataItemAttributeValue.setAttributeValue(newBrandValue);
              break;
            }
          }
        }
      }
    }
  }

  @Override
  public List<ProductAndItemsVO> updateProductScoreOnMasterDataChange(String storeId, String productCode,
      boolean isBackFilling, String productSku, boolean updateCategory, boolean isCombinedEditRequest,
      EditProductDetailDTO editProductDetailDTO, Product currentProduct) throws Exception {
    List<ProductAndItemsVO> updatedProductAndItems = new ArrayList<>();
    ProductDetailResponse productDetailResponse = null;
    String youTubeUrl = StringUtils.EMPTY;
    List<Product> products = new ArrayList<>();
    List<Item> items = new ArrayList<>();
      if (StringUtils.isNotBlank(productCode)) {
        products = this.findByStoreIdAndProductCode(storeId, productCode);
        if (!isBackFilling) {
          products.removeIf(product -> !product.isSynchronized());
        }
        productDetailResponse =
            this.productCategoryBaseOutbound.getProductDetailByProductCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, productCode);
      } else {
        Product product = getProduct(storeId, productSku, currentProduct);
        if (Objects.nonNull(product)) {
          products.add(product);
          if (StringUtils.isNotBlank(product.getProductCode())) {
            productDetailResponse =
                this.productCategoryBaseOutbound.getProductDetailByProductCode(Constants.DEFAULT_USERNAME, Constants.DEFAULT_USERNAME, product.getProductCode());
          }
          items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku);
        }
      }
    if (CollectionUtils.isNotEmpty(products)) {
      for (Product product : products) {
        ProductScore oldProductScore = Optional.ofNullable(product.getProductScore()).orElse(new ProductScore());
        if (Boolean.valueOf(isBackFilling)) {
          String[] apiKeys = youTubeDataApiKey.split(",");
          int apiKeyIndex = (int) (Math.random() * apiKeys.length);
          String youTubeApiKey = apiKeys[apiKeyIndex];
          youTubeUrl =
            fetchYouTubeUrl(storeId, product, productDetailResponse, apiKeyIndex, youTubeApiKey);
        }
        if (product.isSynchronized()) {
          ProductScoreRequest productScoreRequest =
              ProductScoreRequest.builder().productCode(productDetailResponse.getProductCode()).brand(productDetailResponse.getBrand())
                  .description(productDetailResponse.getDescription()).name(productDetailResponse.getName())
                  .uniqueSellingPoint(productDetailResponse.getUniqueSellingPoint()).url(productDetailResponse.getUrl())
                  .categoryCode(
                      productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode())
                  .build();
          productScoreRequest.setProductAttributeRequests(CommonUtil
              .generateAttributeScoreRequestsFromMasterDataAndSpecialAttributes(productDetailResponse, product));
          productScoreRequest.setItemRequests(generateItemScoreFromMasterDataRequests(productDetailResponse));
          productScoreRequest.setSynchronised(product.isSynchronized());
          ProductScoreVo productScoreVo =
              this.productScoreUtil.getProductScoreByProductScoreRequest(productScoreRequest);
          ProductScore productScore = new ProductScore();
          if (isBackFilling || Objects.isNull(product.getProductScore())) {
            checkAndSetDefaultVariantCreationScore(storeId, product, productScoreVo, productScore);
          } else {
            BeanUtils.copyProperties(productScoreVo, productScore, "variantCreatingScore");
            setVariantCreatingScore(product.getProductScore().getVariantCreatingScore(), productScore);
          }
          ProductAndItemsVO productAndItemsVO = null;
            productAndItemsVO = updateMasterDataFieldsInProduct(product, productDetailResponse, updateCategory);
            product = productAndItemsVO.getProduct();
            items = productAndItemsVO.getItems();
          product.setProductScore(productScore);
        } else {
          if(updateMasterDataDetailsForUnsyncProducts) {
            ProductAndItemsVO productAndItemsVO = null;
              productAndItemsVO = updateMasterDataFieldsInProduct(product, productDetailResponse, updateCategory);
              product = productAndItemsVO.getProduct();
              items = productAndItemsVO.getItems();
          }
          ProductScoreRequest productScoreRequest =
              ProductScoreRequest.builder().url(product.getMasterDataProduct().getUrl())
                  .uniqueSellingPoint(product.getMasterDataProduct().getUniqueSellingPoint())
                  .name(product.getMasterDataProduct().getProductName())
                  .description(product.getMasterDataProduct().getDescription().getBytes())
                  .brand(product.getMasterDataProduct().getBrand()).productCode(product.getProductCode()).build();
          productScoreRequest
              .setProductAttributeRequests(CommonUtil.generateAttributeScoreRequestsForUnsyncL3(product));
          if (StringUtils.isNotBlank(product.getProductCode())) {
            productScoreRequest.setItemRequests(generateItemScoreFromMasterDataRequests(productDetailResponse));
            productScoreRequest.setCategoryCode(
                productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode());
          } else {
            productScoreRequest.setItemRequests(generateItemScoreRequestFromItems(items));
            productScoreRequest
                .setCategoryCode(product.getMasterDataProduct().getMasterCatalog().getCategory().getCategoryCode());
          }
          productScoreRequest.setSynchronised(product.isSynchronized());
          ProductScoreVo productScoreVo =
              this.productScoreUtil.getProductScoreByProductScoreRequest(productScoreRequest);
          ProductScore productScore = new ProductScore();
          // If not back filling score wont get generated for unsync products so not adding any check here because
          // product list itself will have only sync products request.
          checkAndSetDefaultVariantCreationScore(storeId, product, productScoreVo, productScore);
          product.setProductScore(productScore);
        }
        boolean youTubeUrlUpdated = !Objects.equals(youTubeUrl,
          Optional.of(product).map(Product::getUrl).orElse(StringUtils.EMPTY));
        product.setUrl(youTubeUrl);
        updatedProductAndItems.add(new ProductAndItemsVO(product, items));
        Set<String> missingFields = product.getMissingFields();
        CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse, product);
        boolean needCacheClear = cacheClearNeeded(product, missingFields, youTubeUrlUpdated);
        Video currentVideo = product.getVideo();
        VideoDTO updatedVideo =
            Optional.ofNullable(productDetailResponse).map(ProductDetailResponse::getVideoDTO)
                .orElse(null);
        boolean videoUpdated = false;
        if (Objects.nonNull(currentVideo) && Objects.nonNull(updatedVideo)) {
          videoUpdated = !currentVideo.getVideoId().equals(updatedVideo.getVideoId());
        } else if (Objects.nonNull(currentVideo) ^ Objects.nonNull(updatedVideo)) {
          videoUpdated = true;
        }
        if (videoUpdated) {
          product.setVideo(CommonUtil.convertVideoDTOToVideo(updatedVideo));
        }
        needCacheClear = needCacheClear || videoUpdated;
        if (!GdnObjects.equals(product.getProductScore(), oldProductScore) || needCacheClear) {
          Product updatedProduct = saveOperationService.saveProductWithoutUpdatingSolr(product,
              CollectionUtils.isNotEmpty(product.getUpdatedFields()) ?
                  new ArrayList<>(product.getUpdatedFields()) :
                  Collections.EMPTY_LIST, StringUtils.EMPTY, Collections.EMPTY_MAP);
            this.productScoreHistoryL3Service.saveProductScoreHistoryL3(storeId, product.getProductSku(),
                oldProductScore, updatedProduct.getProductScore());
        } else if (publishProductChangeOnBrandOrNameChangeGenerateProductScore && CollectionUtils.isNotEmpty(
            product.getUpdatedFields())) {
          saveAndPublishService.publishProduct(product, new ArrayList<>(product.getUpdatedFields()));
        }
        String username =
          Optional.ofNullable(productDetailResponse).map(ProductDetailResponse::getUpdatedBy)
            .orElse(mandatoryParameterHelper.getUsername());
        if (videoUpdated) {
          processAndPublishEventsForVideoCompressionAndHistory(updatedVideo,
              currentVideo, product, username);
        }
      }
    }
    return updatedProductAndItems;
  }

  private Product getProduct(String storeId, String productSku, Product currentProduct) {
    Product product = currentProduct;
    if (Objects.isNull(currentProduct)) {
      product = this.findByStoreIdAndProductSku(storeId, productSku);
    }
    return product;
  }

  private static boolean cacheClearNeeded(Product product, Set<String> missingFields,
    boolean youTubeUrlUpdated) {
    return youTubeUrlUpdated || !CollectionUtils.isEqualCollection(missingFields,
      product.getMissingFields());
  }

  private String fetchYouTubeUrl(String storeId, Product product,
    ProductDetailResponse productDetailResponse, int apiKeyIndex, String youTubeApiKey)
    throws Exception {
    return validateYouTubeUrlInXProduct ?
      getValidatedUrl(storeId, product, productDetailResponse, apiKeyIndex, youTubeApiKey) :
      Optional.ofNullable(productDetailResponse).map(ProductDetailResponse::getUrl)
        .orElse(StringUtils.EMPTY);
  }

  private String getValidatedUrl(String storeId, Product product,
    ProductDetailResponse productDetailResponse, int apiKeyIndex, String youTubeApiKey)
    throws Exception {
    String youTubeUrl;
    if (product.isSynchronized()) {
      youTubeUrl =
        validateYoutubeUrlForProducts(storeId, productDetailResponse.getUrl(), apiKeyIndex,
          youTubeApiKey);
      productDetailResponse.setUrl(youTubeUrl);
    } else {
      youTubeUrl =
        validateYoutubeUrlForProducts(storeId, product.getMasterDataProduct().getUrl(), apiKeyIndex,
          youTubeApiKey);
      product.getMasterDataProduct().setUrl(youTubeUrl);
    }
    return youTubeUrl;
  }

  private void processAndPublishEventsForVideoCompressionAndHistory(VideoDTO updatedVideo,
      Video currentVideo, Product product, String userName) {

    String activity = StringUtils.EMPTY;
    VideoCompressionEventModel videoCompressionEventModel;
    if (Objects.nonNull(updatedVideo)) {
      activity = Objects.nonNull(currentVideo) ? Constants.VIDEO_UPDATE_ACTION_KEY
          : Constants.VIDEO_ADD_ACTION_KEY;
      videoCompressionEventModel =
        VideoCompressionEventModel.builder().videoId(updatedVideo.getVideoId())
          .oldVideoId(Optional.ofNullable(currentVideo).map(Video::getVideoId).orElse(null))
          .clientId(clientIdForVideoCompression).ownerId(product.getMerchantCode()).build();
    } else {
      activity = Constants.VIDEO_DELETE_ACTION_KEY;
      videoCompressionEventModel =
        VideoCompressionEventModel.builder().videoId(currentVideo.getVideoId())
          .ownerId(product.getMerchantCode()).clientId(clientIdForVideoCompression)
          .markForDelete(true).build();
    }

    videoCompressionEventModel.setAdditionalFields(
      Map.of(Constants.PRODUCT_SKU, product.getProductSku(), Constants.PRODUCT_CODE,
        product.getProductCode()));
    saveAndPublishService.publishVideoCompressionEvent(videoCompressionEventModel);

    AuditTrailListResponse auditTrailListResponse = CommonUtil.getAuditTrailListResponse(
        Optional.ofNullable(updatedVideo).map(VideoDTO::getVideoName).orElse(Constants.HYPHEN),
        Optional.ofNullable(currentVideo).map(Video::getVideoName).orElse(Constants.HYPHEN),
        product, activity, userName);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }

  private void checkAndSetDefaultVariantCreationScore(String storeId, Product product, ProductScoreVo productScoreVo,
      ProductScore productScore) {
    BeanUtils.copyProperties(productScoreVo, productScore);
    SystemParameter systemParameter = systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.VARIANT_CREATION_MADE_NON_MANDATORY);
    SystemParameter variantCreationScoreSystemParameter = systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.VARIANT_CREATION_MANDATORY_SCORE);
    Date variantCreationMadeNonMandatory = new Date(Long.parseLong(systemParameter.getValue()));
    Double variantCreationScore = Double.parseDouble(variantCreationScoreSystemParameter.getValue());
    if (Objects.nonNull(product.getCreatedDate()) && product.getCreatedDate().before(variantCreationMadeNonMandatory)) {
      setVariantCreatingScore(variantCreationScore, productScore);
    }
  }

  private void setVariantCreatingScore(Double variantCreationScore, ProductScore productScore) {
    productScore.setVariantCreatingScore(variantCreationScore);
    productScore.setTotalScore(
        (productScore.getMandatoryAttributeScore() + productScore.getProductTitleScore() + productScore
            .getRecommendedAttributeScore() + productScore.getDescriptionScore() + productScore.getImageScore()
            + productScore.getRemainingAttributeScore() + productScore.getUspScore() + productScore
            .getVariantCreatingScore() + productScore.getVideoUrlScore() + productScore.getEanUpcScore())
            * ROUND_OFF_FACTOR / ROUND_OFF_FACTOR);
  }

  private String validateYoutubeUrlForProducts(String storeId, String url, int apiKeyIndex, String youTubeApiKey)
      throws Exception {
    log.debug("Using youtube api key index: {} for url: {}", apiKeyIndex, url);
    boolean youTubeUrlValidationSwitch = Boolean.valueOf(systemParameterService
        .findValueByStoreIdAndVariable(storeId, SystemParameterNames.YOUTUBE_URL_VALIDATION_SWITCH).getValue());
    boolean youTubeUrlResponse =
        ValidationUtil.validateYouTubeUrl(url, youTubeApiKey, youTube, youTubeUrlValidationSwitch);
    if (!youTubeUrlResponse) {
      return StringUtils.EMPTY;
    }
    return url;
  }

  private List<ItemScoreRequest> generateItemScoreFromMasterDataRequests(ProductDetailResponse productDetailResponse) {
    List<ItemScoreRequest> itemScoreRequestList = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductItemResponses())) {
      for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
        ItemScoreRequest itemScoreRequest = ItemScoreRequest.builder().upcCode(productItemResponse.getUpcCode())
            .itemImages(CommonUtil
                .toMasterDataProductImageDTO(productDetailResponse.getProductCode(), productItemResponse.getImages()))
            .build();
        itemScoreRequestList.add(itemScoreRequest);
      }
    }
    return itemScoreRequestList;
  }

  private List<ItemScoreRequest> generateItemScoreRequestFromItems(List<Item> items) {
    List<ItemScoreRequest> itemScoreRequestList = new ArrayList<>();
    for (Item item : items) {
      ItemScoreRequest itemScoreRequest = ItemScoreRequest.builder().upcCode(item.getMasterDataItem().getUpcCode())
          .itemImages(CommonUtil
              .convertItemImagesToMasterDataProductImageDTO(item.getMasterDataItem().getMasterDataItemImages()))
          .build();
      itemScoreRequestList.add(itemScoreRequest);
    }
    return itemScoreRequestList;
  }

  @Override
  public double generateShippingWeight(String storeId, String categoryCode, double length, double width, double height,
      double weight) {
    return productCategoryBaseOutbound.generateShippingWeight(storeId, categoryCode, length, width, height, weight);
  }

  @Override
  public List<ProductAndItemsVO> generateProductScoreByProductSku(String storeId, String productSku, String productCode,
      String requestId, String userName, boolean updateCategory, Product product) throws Exception {
    List<ProductAndItemsVO> productAndItemsVOList =
        this.updateProductScoreOnMasterDataChange(storeId, productCode, true, productSku, updateCategory, false, null,
            product);
    for (ProductAndItemsVO productAndItemsVO : productAndItemsVOList) {
      this.productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(productAndItemsVO.getProduct(),
          productAndItemsVO.getItems(), true);
    }
    return productAndItemsVOList;
  }

  //service method to update the new product code
  @Override
  public Page<ProductCenterSummaryResponse> getProductCenterSummary(String storeId, String requestId,
      ProductCenterSummaryRequest productCenterSummaryRequest, PageRequest pageRequest) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    Page<ProductSolr> resultFromSolr =
        this.productSolrRepository.getProductCenterSummary(storeId, productCenterSummaryRequest, pageRequest);
    checkArgument(Objects.nonNull(resultFromSolr) && CollectionUtils.isNotEmpty(resultFromSolr.getContent()),
        ProductServiceImpl.PRODUCT_NOT_FOUND);

    //get master category name
    Set<String> categoryCodes =
        resultFromSolr.getContent().stream().filter(productSolr -> Objects.nonNull(productSolr.getMasterCatalog()))
            .map(productSolr -> productSolr.getMasterCatalog().split(solrStringDelimiter)[1])
            .collect(Collectors.toSet());
    CategoryNamesResponse categoryNamesResponse =
        productCategoryBaseOutbound.getCategoryNames(new ArrayList<>(categoryCodes));
    checkArgument(Objects.nonNull(categoryNamesResponse) && MapUtils.isNotEmpty(categoryNamesResponse.getCategoryMap()),
        ProductServiceImpl.CATEGORY_NOT_FOUND_ON_PCB);

    //Convert to Page<ProductCenterSummaryResponse>
    return getProductCenterSummaryResponse(resultFromSolr, categoryNamesResponse.getCategoryMap(), pageRequest);
  }

  @Override
  public List<UnmappedSkuResponse> getUnmappedProductSkus(String storeId, List<String> masterCategories)
      throws Exception {
    List<ProductSolr> resultFromSolr =
        productSolrRepository.getUnmappedProductSKusByCategoryCodes(storeId, masterCategories);
    return resultFromSolr.stream().filter(productSolr -> Objects.nonNull(productSolr.getMasterCatalog())).map(
        productSolr -> new UnmappedSkuResponse(productSolr.getProductSku(), productSolr.getProductName(),
            productSolr.getMasterCatalog().split(solrStringDelimiter)[0],
            productSolr.getMasterCatalog().split(solrStringDelimiter)[1], productSolr.getCreatedDate()))
        .collect(Collectors.toList());
  }

  private Page<ProductCenterSummaryResponse> getProductCenterSummaryResponse(Page<ProductSolr> resultFromSolr,
      Map<String, String> categoryMap, PageRequest pageRequest) {
    List<ProductCenterSummaryResponse> productCenterSummaryResponseList = new ArrayList<>();
    for (ProductSolr productSolr : resultFromSolr.getContent()) {
      ProductCenterSummaryResponse productCenterSummaryResponse = new ProductCenterSummaryResponse();
      productCenterSummaryResponse.setProductSku(productSolr.getProductSku());
      productCenterSummaryResponse.setProductName(productSolr.getProductName());
      productCenterSummaryResponse.setStatus(productSolr.isSuspended() ? Constants.SUSPENDED : Constants.ACTIVE);
      productCenterSummaryResponse.setLastUpdate(productSolr.getProductCenterUpdatedDate());
      String masterCategoryCode = Objects.nonNull(productSolr.getMasterCatalog()) ?
          productSolr.getMasterCatalog().split(solrStringDelimiter)[1] :
          StringUtils.EMPTY;
      productCenterSummaryResponse
          .setMasterCategory(new MasterCategoryResponse(masterCategoryCode, categoryMap.get(masterCategoryCode)));
      if (CollectionUtils.isNotEmpty(productSolr.getSalesCatalog())) {
        List<String> salesCategoryCodes =
            productSolr.getSalesCatalog().stream().map(saleCategory -> saleCategory.split(solrStringDelimiter)[1])
                .collect(Collectors.toList());
        productCenterSummaryResponse.setSalesCategories(salesCategoryCodes);
      } else {
        productCenterSummaryResponse.setSalesCategories(new ArrayList<>());
      }
      productCenterSummaryResponseList.add(productCenterSummaryResponse);
    }
    return new PageImpl<>(productCenterSummaryResponseList, pageRequest, resultFromSolr.getTotalElements());
  }

  @Override
  public void updateMigratedProductCode(String username, String requestId, String storeId, String productSku,
      String newProductCode, boolean rollback) throws Exception {
    Product product = productRepository.findByStoreIdAndProductSku(storeId, productSku);

    List<Item> items = itemService.findItemsByStoreIdAndProductSku(storeId, productSku);
    if ((!product.isMarkForDelete() || (product.isMarkForDelete() && product.isSuspended())) && StringUtils
        .isNotBlank(newProductCode)) {
      ProductDetailResponse productDetailResponse =
          this.masterDataService.getProductDetailFromMasterData(username, requestId, newProductCode);
      Map<String, ProductItemResponse> sourceItemCodeToProductItem = new HashMap<>();
      if (Objects.nonNull(product.getProductCode())) {
        sourceItemCodeToProductItem = generateSourceItemCodeToProductItem(productDetailResponse, rollback);
      } else {
        for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
          sourceItemCodeToProductItem.put(productItemResponse.getGeneratedItemName(), productItemResponse);
        }
      }
      product.setProductCode(newProductCode);
      if (items.size() != sourceItemCodeToProductItem.size()) {
        List<ProductItemResponse> productItemResponseList =
            new ArrayList<>(productDetailResponse.getProductItemResponses());
        int count = 0;
        for (Item item : items) {
          ProductItemResponse productItemResponse = productItemResponseList.get(count);
          if (Objects.nonNull(productItemResponse)) {
            item.setItemCode(productItemResponse.getSkuCode());
            item.setSourceItemCode(productItemResponse.getSourceItemCode());
            item.setContentChanged(productItemResponse.isContentChanged());
            item.setInitialContentChanged(productItemResponse.isContentChanged());
          }
          count++;
        }
      } else {
        for (Item item : items) {
          ProductItemResponse productItemResponse;
          if (StringUtils.isNotBlank(item.getItemCode())) {
            productItemResponse =
                sourceItemCodeToProductItem.get(rollback ? item.getSourceItemCode() : item.getItemCode());
          } else {
            productItemResponse = sourceItemCodeToProductItem.get(item.getMasterDataItem().getGeneratedItemName());
          }
          if (Objects.nonNull(productItemResponse)) {
            item.setItemCode(productItemResponse.getSkuCode());
            item.setSourceItemCode(productItemResponse.getSourceItemCode());
            item.setContentChanged(productItemResponse.isContentChanged());
            item.setInitialContentChanged(productItemResponse.isContentChanged());
          }
        }
      }
      if (rollback) {
        saveOperationService.saveProductAndItems(new ProductAndItemsVO(product, items), new ArrayList<>());
      } else {
        saveOperationService.saveProductAndItemsWithoutPublishingEvent(new ProductAndItemsVO(product, items));
      }
    }

    if (rollback && StringUtils.isBlank(newProductCode)) {
      product.setProductCode(null);
      product.setSynchronized(false);
      for (Item item : items) {
        item.setItemCode(null);
        item.setSynchronized(false);
        List<ItemChangeEventType> itemChangeEventTypes = new ArrayList<>();
        itemChangeEventTypes.add(ItemChangeEventType.SYNC_UNSYNC_FLAG_CHANGE);
        item.setItemChangeEventTypes(itemChangeEventTypes);
        saveOperationService.saveProductAndItems(new ProductAndItemsVO(product, items), new ArrayList<>());
      }
    }
  }

  private Map<String, ProductItemResponse> generateSourceItemCodeToProductItem(
      ProductDetailResponse productDetailResponse, boolean publishItems) {
    Map<String, ProductItemResponse> productItemResponseMap = new HashMap<>();
    if (publishItems) {
      for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
        productItemResponseMap.put(productItemResponse.getSkuCode(), productItemResponse);
      }
    } else {
      for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
        productItemResponseMap.put(productItemResponse.getSourceItemCode(), productItemResponse);
      }
    }
    return productItemResponseMap;
  }

  @Override
  public ProductCenterDetailResponse getProductDetailsForProductCenter(String storeId, String productSku,
      boolean needBusinessPartnerData)
      throws Exception {
    ProductSolr solr = productAndItemSolrIndexerService.getProductSolrByProductSku(productSku);
    BusinessPartner businessPartner = new BusinessPartner();
    if (needBusinessPartnerData) {
      businessPartner = businessPartnerService.getBusinessPartnerByBusinessPartnerCode(storeId, solr.getMerchantCode());
    }
    ProductCenterDetailResponse response =
        ProductCenterDetailResponse.builder().imagePath(solr.getProductMainImage()).sellerCode(solr.getMerchantCode())
            .sellerName(businessPartner.getBusinessPartnerName())
            .productCenterUpdatedDate(solr.getProductCenterUpdatedDate()).markForDelete(solr.isMarkForDelete())
            .productName(solr.getProductName()).build();
    if (solr.isSuspended()) {
      response.setStatus(SUSPENDED);
    } else {
      response.setStatus(ACTIVE);
    }
    return response;
  }

  @Override
  public void updateSalesCategory(String storeId, String productSku, SalesCategoryMappingUpdateRequest request,
      String requestId) {
    Product product =
        productCacheHelperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
    if (Objects.isNull(product)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, PRODUCT_NOT_FOUND + productSku);
    }

    if (CollectionUtils.isNotEmpty(request.getAddedCategories()) || CollectionUtils
        .isNotEmpty(request.getDeletedCategories())) {
      Set<String> productChannel = new HashSet<>();
      Product result = adjustAndUpdateSalesCategoryMappings(storeId, request, product);
      generateProductCenterHistory(productSku, request, requestId);

      ProductChange productChange = new ProductChange();
      BeanUtils.copyProperties(result, productChange);
      CommonUtil.populateProductType(result, productChange);
      productChange.setMultiVariant(product.getDefiningAttributes().size() > Constants.SINGLE_VARIANT_L4_COUNT);

      setDistributionMappingStatusInEvent(result, productChange);
      if (Objects.nonNull(result.getMasterDataProduct())) {
        com.gdn.x.product.domain.event.model.MasterDataProduct masterDataProduct =
            new com.gdn.x.product.domain.event.model.MasterDataProduct();
        BeanUtils.copyProperties(result.getMasterDataProduct(), masterDataProduct);
        productChange.setMasterDataProduct(masterDataProduct);
      }
      if (CollectionUtils.isNotEmpty(result.getAllSalesCatalogs())) {
        for (SalesCatalog salesCatalog : result.getAllSalesCatalogs()) {
          productChange.getSalesCatalogs()
              .add(gdnMapper.deepCopy(salesCatalog, com.gdn.x.product.domain.event.model.SalesCatalog.class));
        }
      }
      if (result.isB2bActivated()) {
        productChannel.add(Constants.B2B);
      }
      if (result.getB2cActivated()) {
        productChannel.add(Constants.RETAIL);
      }
      productChange.setProductChannel(productChannel);
      CommonUtil.setBrandAndCategoryCodeAndPreOrderForOdoo(product, productChange);
      this.kafkaPublisher.send(ProductDomainEventName.PRODUCT_CHANGE_EVENT_NAME,
        productChange.getProductSku(), productChange);
    }
  }

  private void setDistributionMappingStatusInEvent(Product result, ProductChange productChange) {
    if (ranchIntegrationEnabled) {
      if (Objects.nonNull(result.getDistributionStatus())) {
        productChange.setDistributionMappingStatus(result.getDistributionStatus().getDescription());
      }
    }
  }

  @Override
  public Long getProductL3CountByProductCode(String productCode) {
    Long l3Count = 0L;
    if (StringUtils.isNotBlank(productCode)) {
      l3Count = productSolrRepository.countByProductCode(productCode);
    }
    return l3Count;
  }

  private void generateProductCenterHistory(String productSku, SalesCategoryMappingUpdateRequest request,
      String requestId) {
    if (CollectionUtils.isNotEmpty(request.getAddedCategories())) {
      CategoryNamesResponse addCategoryNamesResponse = this.productCategoryBaseOutbound.getCategoryNames(
          request.getAddedCategories().stream().map(SalesCategoryUpdateRequest::getCategoryCode)
              .collect(Collectors.toList()));
      for (Map.Entry<String, String> entry : addCategoryNamesResponse.getCategoryMap().entrySet()) {
        productCenterHistoryService
            .saveProductCenterHistory(productSku, ProductCenterActivity.COPY, requestId, StringUtils.EMPTY,
                entry.getValue());
      }
    }
    if (CollectionUtils.isNotEmpty(request.getDeletedCategories())) {
      CategoryNamesResponse deletedCategoryNamesResponse = this.productCategoryBaseOutbound.getCategoryNames(
          request.getDeletedCategories().stream().map(SalesCategoryUpdateRequest::getCategoryCode)
              .collect(Collectors.toList()));
      for (Map.Entry<String, String> entry : deletedCategoryNamesResponse.getCategoryMap().entrySet()) {
        productCenterHistoryService
            .saveProductCenterHistory(productSku, ProductCenterActivity.DELETE, requestId, entry.getValue(),
                StringUtils.EMPTY);
      }
    }
  }


  private Product adjustAndUpdateSalesCategoryMappings(String storeId, SalesCategoryMappingUpdateRequest request,
      Product product) {
    updateSalesCategoryMappingOfProduct(request, product);
    product.setProductCenterUpdatedDate(new Date());
    Product result = this.productRepository
        .updateSalesCatalog(storeId, product.getProductSku(), product.getSalesCatalogs(),
            product.getProductCenterUpdatedDate());
    List<Item> items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, product.getProductSku());
    CommonUtil.addUpdatedFieldsInItem(items, Collections.singletonList(UpdatedFields.CATEGORY_UPDATE));
    productAndItemSolrIndexerService.updateSolrOnSalesCatalogChanges(product, items);
    saveAndPublishService.publishListOfItems(items);
    this.cacheEvictHelperService.evictProductData(storeId, result);
    return result;
  }

  @Override
  public void updateSalesCategoryOfProduct(SalesCategoryMappingUpdateRequest salesCategoryMappingUpdateRequest,
      Product product) {
    updateSalesCategoryMappingOfProduct(salesCategoryMappingUpdateRequest, product);
  }

  public static void updateSalesCategoryMappingOfProduct(SalesCategoryMappingUpdateRequest request, Product product) {
    Map<String, List<Category>> catalogCategoryMapExisting = product.getSalesCatalogs().stream()
        .collect(Collectors.toMap(SalesCatalog::getCatalogCode, SalesCatalog::getListOfCategories));

    Map<String, Map<String, List<SalesCategoryUpdateRequest>>> addedCatalogCategoryListMap =
        request.getAddedCategories().stream().collect(Collectors.groupingBy(SalesCategoryUpdateRequest::getCatalogCode,
            Collectors.groupingBy(SalesCategoryUpdateRequest::getCategoryCode)));

    Map<String, Map<String, List<SalesCategoryUpdateRequest>>> deletedCatalogCategoryListMap =
        request.getDeletedCategories().stream().collect(Collectors
            .groupingBy(SalesCategoryUpdateRequest::getCatalogCode,
                Collectors.groupingBy(SalesCategoryUpdateRequest::getCategoryCode)));

    for (String catalogCode : addedCatalogCategoryListMap.keySet()) {
      if (!catalogCategoryMapExisting.containsKey(catalogCode)) {
        catalogCategoryMapExisting.put(catalogCode, new ArrayList<>());
      }
      Map<String, String> existingCategoryMap = catalogCategoryMapExisting.get(catalogCode).stream()
          .collect(Collectors.toMap(Category::getCategoryCode, Category::getCatgroupId));
      for (String categoryCode : addedCatalogCategoryListMap.get(catalogCode).keySet()) {
        if (!existingCategoryMap.containsKey(categoryCode)) {
          Category category = new Category(categoryCode, categoryCode);
          catalogCategoryMapExisting.get(catalogCode).add(category);
        }
      }
    }
    for (String catalogCode : deletedCatalogCategoryListMap.keySet()) {
      if (catalogCategoryMapExisting.containsKey(catalogCode)) {
        Map<String, Category> existingCategoryMap = catalogCategoryMapExisting.get(catalogCode).stream()
            .collect(Collectors.toMap(Category::getCategoryCode, Function.identity()));
        for (String categoryCode : deletedCatalogCategoryListMap.get(catalogCode).keySet()) {
          if (existingCategoryMap.containsKey(categoryCode)) {
            catalogCategoryMapExisting.get(catalogCode).remove(existingCategoryMap.get(categoryCode));
          }
        }
      }
    }
    product.setSalesCatalogs(new ArrayList<>());
    for (Map.Entry<String, List<Category>> existingSaleCatalog : catalogCategoryMapExisting.entrySet()) {
      SalesCatalog salesCatalog = new SalesCatalog(existingSaleCatalog.getKey(), existingSaleCatalog.getValue());
      product.getAllSalesCatalogs().add(salesCatalog);
    }
  }

  @Override
  public ProductPickupPointListResponse getPickupPointCodesByProductSku(String storeId, String productSku)
      throws Exception {
    Product product = this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
    checkArgument(Objects.nonNull(product), ErrorMessages.PRODUCT_NOT_FOUND_ERROR);
    return new ProductPickupPointListResponse(product.getPickupPointCodes());
  }

  @Override
  public Page<ProductSkuSummaryResponse> getProductSkuList(String storeId,
      ProductSkuSummaryRequest productSkuSummaryRequest, String businessPartnerCode, int page, int size) {
    PageRequest pageRequest = PageRequest.of(page, size);
    ProductSkuSummaryRequestVo productSkuSummaryRequestVo = new ProductSkuSummaryRequestVo();
    BeanUtils.copyProperties(productSkuSummaryRequest, productSkuSummaryRequestVo);
    Page<ProductSolr> productSolrPage =
      productSolrRepository.getProductSkuSummary(storeId, productSkuSummaryRequestVo,
        businessPartnerCode, page, size);
    Pair<List<String>, List<ProductSkuSummaryResponse>> summaryResponseList =
      CommonUtil.toProductSkuSummaryResponseList(productSolrPage.getContent(), solrStringDelimiter);
    if (CollectionUtils.isNotEmpty(summaryResponseList.getKey())) {
      summaryResponseList.getKey().forEach(L3 -> {
        ProductRetryEventPublish productRetryEventPublish =
          ProductRetryEventPublish.builder().clearCache(Boolean.TRUE).retryCount(0).identifier(L3)
            .retryPublishStatus(RetryPublishStatus.PENDING)
            .topicName(ProductDomainEventName.REINDEX_PRODUCTS_TO_SOLR).retryCount(0).build();
        this.productRetryEventPublishService.insertToRetryPublish(productRetryEventPublish);
        log.info("Added the adjustment Event for Retry with Model : {} ", productRetryEventPublish);
      });
    }
    return new PageImpl<>(summaryResponseList.getValue(), pageRequest, productSolrPage.getTotalElements());
  }

  @Override
  public EditItemResponse toggleArchiveProduct(String storeId, String username, String productSku,
      boolean doArchive, String source) throws Exception {
    List<String> productChangeEventTypes = new ArrayList<>();
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productSku),
      ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    EditItemResponse editItemResponse = new EditItemResponse();
    Product product;
    if (newErrorMessageInArchiveFlow) {
      product = this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
      checkState(Objects.nonNull(product), ProductServiceImpl.PRODUCT_NOT_FOUND + productSku);
      if (product.isMarkForDelete()) {
        editItemResponse.setApiErrorCode(ApiErrorCode.PRODUCT_IS_NOT_ACTIVE);
        return editItemResponse;
      }
    } else {
      product =
          this.productCacheHelperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, productSku);
      checkState(Objects.nonNull(product), ProductServiceImpl.PRODUCT_NOT_FOUND + productSku);
    }
    if (product.isArchived() == doArchive) {
      editItemResponse.setApiErrorCode((doArchive) ?
        ApiErrorCode.TOGGLE_ARCHIVE_FAILED_FOR_SAME_FLAG :
        ApiErrorCode.TOGGLE_UNARCHIVE_FAILED_FOR_SAME_FLAG);
      return editItemResponse;
    }
    else{
      productChangeEventTypes.add(ProductChangeEventType.ARCHIVE_FLAG_CHANGE);
    }
    EditItemResponse itemEditResponse =
      itemService.toggleArchiveByProductSku(storeId, username, productSku, doArchive, source);
    product.setArchived(doArchive);
    if (doArchive) {
      product.setArchived(true);
      product.setCncActivated(false);
      product.setOnline(false);
      if (product.isOff2OnChannelActive()) {
        product.setOff2OnChannelActive(false);
      }
    } else {
      productUnarchiveActions(product, itemEditResponse.isCncActivated(), editItemResponse);
    }
    saveProductWithoutUpdatingSolr(product, productChangeEventTypes, StringUtils.EMPTY);
      saveAndPublishService.publishSolrUpdateEvent(Collections.singletonList(
          objectConverterService.convertToProductAndItemEventModel(
              new ProductAndItemsVO(product, itemEditResponse.getUpdatedItems()))));
    return editItemResponse;
  }

  @Override
  public void doArchivalActionOnProduct(String storeId, Product product, List<Item> updatedItems) {
    checkArgument(StringUtils.isNotBlank(storeId), CommonConstants.STORE_ID_MUST_NOT_BE_BLANK);
    checkState(Objects.nonNull(product), ProductServiceImpl.PRODUCT_NOT_FOUND + product);
    if (product.isSuspended()) {
      product.setArchivedBeforeSuspension(true);
    }
    product.setArchived(true);
    product.setCncActivated(false);
    product.setOnline(false);
    if (product.isOff2OnChannelActive()) {
      product.setOff2OnChannelActive(false);
    }
    saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
    saveAndPublishService.publishSolrUpdateEvent(Collections.singletonList(
        objectConverterService.convertToProductAndItemEventModel(new ProductAndItemsVO(product, updatedItems))));
    updateProductHistory(product, Constants.PRODUCT_ARCHIVE);
  }

  private void updateProductHistory(Product product, String action) {
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(product.getMerchantCode(), Constants.HYPHEN, action, Boolean.toString(!product.isArchived()),
            Boolean.toString(product.isArchived()), null, product.getProductSku(), product.getProductName(),
            Constants.HYPHEN, false);
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(Constants.DEFAULT_USERNAME);
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setAuditTrailResponseList(Collections.singletonList(auditTrailDto));
    log.info(PUBLISHING_THE_EVENT_TO_UPDATE_THE_AUDIT_LOGS, ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }

  private void productUnarchiveActions(Product product, boolean cncActivated,
    EditItemResponse editItemResponse) {
    product.setArchived(false);
    product.setCncActivated(cncActivated);
    editItemResponse.setCncActivated(cncActivated);
  }

  @Override
  public ProductAndItemsVO getProductDetailsByProductSku(String storeId, String requestId, String username,
      String productSku) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(this.skuValidator.isProductSku(productSku),
        ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    Product product = this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
    checkArgument(Objects.nonNull(product), String.format(ErrorMessages.PRODUCT_NOT_FOUND_ERROR, productSku));
    List<Item> items = this.itemService.findItemsByStoreIdAndProductSku(storeId, productSku);
    List<ItemPickupPoint> itemPickupPoints =
        itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(storeId, productSku);
    ProductAndItemsVO productAndItemsVO = this.masterDataConstructorService
        .constructProductAndItemWithMasterData(storeId, username, requestId, product, items, true);
    productAndItemsVO.getProduct().setItemCatalogs(
        this.catalogService.getItemCatalogsWithCategoryHierarchy(username, requestId, productAndItemsVO.getProduct()));
    if (autoHealMissingPPCodesAtL3 && CollectionUtils.isEmpty(product.getPickupPointCodes())) {
      log.info("auto heal missing pp codes for product code : {} ",product.getProductSku());
      Set<String> pickupPointCodes =
          Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()).stream()
              .map(ItemPickupPoint::getPickupPointCode).filter(StringUtils::isNotBlank)
              .collect(Collectors.toCollection(LinkedHashSet::new));
      product.setPickupPointCodes(pickupPointCodes);
      productAndItemsVO.setProduct(saveOperationService.saveProduct(product));
      kafkaPublisher.send(kafkaTopicProperties.getProductDataAutoFixHistory(),product.getProductCode(),
          com.gdn.x.product.service.util.ResponseHelper.convertToProductDataAutoFixHistoryListRequest(
              product.getProductCode(), X_PRODUCT_PP_CODE_L_3_AUTO_HEAL, StringUtils.EMPTY));

    }
    productAndItemsVO.setItemPickupPoints(itemPickupPoints);
    return productAndItemsVO;
  }

  @Override
  public List<String> updateOff2OnFlagByProductSkus(String storeId, Map<String, Boolean> productSkuActivateMap,
      String userName, String accessChannel, String clientId, String requestId, Boolean updateOff2OnHistory) {
    List<String> failedProductSkus = new ArrayList<>();
    List<AuditTrailDto> auditTrailResponseList = new ArrayList<>();
    for (Map.Entry<String, Boolean> productSkuAndOff2OnFlag : productSkuActivateMap.entrySet()) {
      try {
        saveOperationService.changeOff2OnChannelActiveByProductSkus(storeId, productSkuAndOff2OnFlag.getKey(),
            productSkuAndOff2OnFlag.getValue(), userName, auditTrailResponseList, requestId);
      } catch (Exception e) {
        failedProductSkus.add(productSkuAndOff2OnFlag.getKey());
      }
    }
    if (CollectionUtils.isNotEmpty(auditTrailResponseList) && updateOff2OnHistory) {
      AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
      auditTrailListResponse.setAccessChannel(accessChannel);
      auditTrailListResponse.setChangedBy(userName);
      auditTrailListResponse.setClientId(clientId);
      auditTrailListResponse.setRequestId(requestId);
      auditTrailListResponse.setAuditTrailResponseList(auditTrailResponseList);
      log.info(PUBLISHING_THE_EVENT_TO_UPDATE_THE_AUDIT_LOGS, ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY);
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
    }
    return failedProductSkus;
  }

  @Override
  public Page<ProductSkuResponse> getProductSkuListResponse(String storeId, ProductSummaryRequest productSummaryRequest,
      PageRequest pageRequest) {
    checkArgument(StringUtils.isNotBlank(productSummaryRequest.getMerchantCode()),
        ProductServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    ProductSummaryRequestVo productSummaryRequestVo = CommonUtil.getProductSummaryRequestVo(productSummaryRequest);
    //Get L3 details from L3 solr
    Page<ProductSolr> productSolrs =
        productSolrRepository.getProductNameByProductSummaryRequest(storeId, productSummaryRequestVo, pageRequest);
    List<ProductSkuResponse> productSkuResponses = new ArrayList<>();
    if (ResponseHelper.validateSolrResponse(productSolrs)) {
      for (ProductSolr productSolr : productSolrs.getContent()) {
        ProductSkuResponse productSkuResponse =
            ProductSkuResponse.builder().productSku(productSolr.getProductSku()).build();
        productSkuResponses.add(productSkuResponse);
      }
    }
    return new PageImpl<>(productSkuResponses, pageRequest, productSolrs.getTotalElements());
  }



  @Override
  public Page<ProductL3SummaryResponse> getProductL3SummaryResponse(String storeId,
      ProductSummaryRequest productSummaryRequest, PageRequest pageRequest, boolean onlyDefaultViewConfig) {
    checkArgument(StringUtils.isNotBlank(productSummaryRequest.getMerchantCode()),
        ProductServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    List<String> categoryCodes;
    if (StringUtils.isNotBlank(productSummaryRequest.getSizeAttributeCode())) {
      try {
        categoryCodes = productCacheHelperService.getCategoryCodesCachedByAttributeCode(storeId,
            productSummaryRequest.getSizeAttributeCode());
        if (categoryCodes.size() < categoryCodesLimitInSizeChartListing) {
          productSummaryRequest.getCategoryCodes().addAll(categoryCodes);
        }
      } catch (Exception e) {
        log.error("Error while fetching categories with attribute code {} ",
            productSummaryRequest.getSizeAttributeCode());
      }
    }
    ProductSummaryRequestVo productSummaryRequestVo = CommonUtil.getProductSummaryRequestVo(productSummaryRequest);
    //Get L3 details from L3 solr
    Page<ProductSolr> productSolrs =
        productSolrRepository.getL3ProductSummaryByProductSummaryRequest(storeId, productSummaryRequestVo, pageRequest);
    if (ResponseHelper.validateSolrResponse(productSolrs)) {
      Map<String, List<Item>> productSkusToItemMap = new HashMap<>();
      Map<String, ItemPickupPoint> itemPickupPointMap = new HashMap<>();
      Map<String, BusinessPartnerPickupPoint> pickupPointCodeMap = new HashMap<>();
      List<String> productSkus = new ArrayList<>();
      List<String> singleVariantProductSkus = new ArrayList<>();
      List<Product> productsWithScore = new ArrayList<>();
      List<String> zeroVariantProductSkus = new ArrayList<>();
      //Get item and score related info only in case of non suspension list
        zeroVariantProductSkus =
          productSolrs.getContent().stream().filter(productSolr -> productSolr.getL5Count() == 0)
            .map(ProductSolr::getProductSku).collect(Collectors.toList());
      boolean skipItemAndPPCalls =
          skipL4AndL5CallsForSizeChartListing(productSummaryRequestVo.getSizeChartCode(),
              productSummaryRequestVo.getAttributeCode());
        if (CollectionUtils.isNotEmpty(zeroVariantProductSkus) && !skipItemAndPPCalls) {
          setL5CountFromDbFor0Count(storeId, zeroVariantProductSkus, productSolrs);
        }
      if (!Boolean.TRUE.equals(productSummaryRequestVo.getSuspended())) {
        productSolrs.getContent().forEach(productSolr -> {
            if (ONE == productSolr.getL5Count()) {
              singleVariantProductSkus.add(productSolr.getProductSku());
            }
          productSkus.add(productSolr.getProductSku());
        });
        productsWithScore =
            productRepository.findByStoreIdAndProductSkusAndMarkForDeleteFalse(storeId, productSkus);
        if (CollectionUtils.isNotEmpty(singleVariantProductSkus) && !skipItemAndPPCalls) {
          //Get items from cache for all single variant products
          productSkusToItemMap = getItemByProductSkus(storeId, singleVariantProductSkus);
          List<ItemPickupPoint> itemPickupPoints;
            itemPickupPoints =
              this.itemPickupPointService.findByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId,
                productSkusToItemMap.values().stream()
                  .flatMap(items -> items.stream().map(Item::getItemSku))
                  .collect(Collectors.toList()));
          List<String> pickupPointCodes =
              Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()).stream()
                  .map(ItemPickupPoint::getPickupPointCode).distinct().collect(Collectors.toList());
          if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
            pickupPointCodeMap =
              businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(
                  storeId, pickupPointCodes).stream()
                .collect(toMap(BusinessPartnerPickupPoint::getCode, Function.identity()));
            itemPickupPointMap = Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()).stream().collect(
              Collectors.toMap(ItemPickupPoint::getItemSku, Function.identity(), (v1, v2) -> v2));
          }
        }
      }
      setProductCategoryEligibleFlag(storeId, productSummaryRequest, productSolrs, productsWithScore);
      if (!skipItemAndPPCalls) {
        setMainImageUrl(storeId, productSkusToItemMap, productsWithScore,
            productSolrs.getContent());
      }
      return new PageImpl<>(
        convertToProductL3SummaryResponse(productSolrs, productSkusToItemMap, itemPickupPointMap,
          productsWithScore, pickupPointCodeMap, onlyDefaultViewConfig), pageRequest, productSolrs.getTotalElements());
    } else {
      if (externalSearchReindexEnabled) {
        if (StringUtils.isNotBlank(productSummaryRequest.getKeyword()) && productSummaryRequest.getKeyword()
            .startsWith(productSummaryRequest.getMerchantCode()) && skuValidator.isProductSku(
            productSummaryRequest.getKeyword())) {
          kafkaPublisher.send(ProductDomainEventName.TRIGGER_REINDEX_WITH_EXTERNAL_SEARCH,
              productSummaryRequest.getKeyword(),
              ExternalSearchReindexToSolrEventModel.builder().productSku(productSummaryRequest.getKeyword())
                  .storeId(storeId).build());
        }
      }
    }
    return new PageImpl<>(new ArrayList<>(), pageRequest, 0);
  }

  private boolean skipL4AndL5CallsForSizeChartListing(String sizeChartCode,
      String sizeAttributeCode) {
    return (StringUtils.isNotBlank(sizeChartCode) || StringUtils.isNotBlank(sizeAttributeCode));
  }

  private void setProductCategoryEligibleFlag(String storeId, ProductSummaryRequest productSummaryRequest,
      Page<ProductSolr> productSolrs, List<Product> products) {
    if (sizeChartAdditionForProduct && Objects.nonNull(productSummaryRequest.getSizeAttributeCode())) {
      setCategoryEligibleForSizeChartCode(storeId, productSolrs.getContent(),
          productSummaryRequest.getSizeAttributeCode(), products);
      Map<String, Product> productSkuAndProductMap =
          products.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity()));
      List<String> productsWithEligibleFalse =
          productSolrs.stream().filter(Predicate.not(ProductSolr::isProductCategoryEligibleForSizeChart))
              .map(ProductSolr::getProductSku).collect(Collectors.toList());
      List<String> categoriesToBeFetchedFromPCB =
          productsWithEligibleFalse.stream().map(productSkuAndProductMap::get).map(Product::getCategoryCode)
              .filter(StringUtils::isNotBlank).collect(Collectors.toList());
      if (CollectionUtils.isNotEmpty(productsWithEligibleFalse)) {
        Map<String, Boolean> categoryAndSizeChartAddEligibleMap =
            productCategoryBaseOutbound.getCategoryAndEligibleFlagMap(storeId,
                productSummaryRequest.getSizeAttributeCode(), categoriesToBeFetchedFromPCB);
        setProductCategoryEligibleForSizeChartAdditionWithPCBData(productSolrs, productSkuAndProductMap,
            categoryAndSizeChartAddEligibleMap);

      }
    }
  }

  private void setProductCategoryEligibleForSizeChartAdditionWithPCBData(Page<ProductSolr> productSolrs,
      Map<String, Product> productSkuAndProductMap, Map<String, Boolean> categoryAndSizeChartAddEligibleMap) {
    productSolrs.stream().filter(Predicate.not(ProductSolr::isProductCategoryEligibleForSizeChart))
        .forEach(productSolr -> {
          setProductCategoryEligibleFlagForProductSolr(productSkuAndProductMap, categoryAndSizeChartAddEligibleMap,
              productSolr);
        });
  }

  private static void setProductCategoryEligibleFlagForProductSolr(Map<String, Product> productSkuAndProductMap,
      Map<String, Boolean> categoryAndSizeChartAddEligibleMap, ProductSolr productSolr) {
    String productSku = productSolr.getProductSku();
    String categoryCode = productSkuAndProductMap.get(productSku).getCategoryCode();
    Boolean eligibleForSizeChart = categoryAndSizeChartAddEligibleMap.get(categoryCode);
    productSolr.setProductCategoryEligibleForSizeChart(
        BooleanUtils.toBooleanDefaultIfNull(eligibleForSizeChart, false));
  }

  @Override
  public void setMainImageUrl(String storeId, Map<String, List<Item>> productSkusToItemMap, List<Product> products,
      List<ProductSolr> productSolrs) {
    try {
      if (populateProductMainImageInListing) {
        Map<String, Product> productMap =
            products.stream().collect(Collectors.toMap(Product::getProductSku, Function.identity(), (v1, v2) -> v1));
        List<Product> productToReindex = new ArrayList<>();

        List<ProductSolr> productSolrWithoutProductMainImage =
            productSolrs.stream().filter(productSolr -> StringUtils.isBlank(productSolr.getProductMainImage()))
                .collect(Collectors.toList());

        if (CollectionUtils.isNotEmpty(productSolrWithoutProductMainImage)) {
          List<String> productSkuWithoutProductMainImage =
              productSolrWithoutProductMainImage.stream().map(ProductSolr::getProductSku).collect(Collectors.toList());
          productSkuWithoutProductMainImage.removeIf(productSkusToItemMap.keySet()::contains);

          Map<String, String> productSkuAndImageUrlMap = new HashMap<>();
          if (CollectionUtils.isNotEmpty(productSkuWithoutProductMainImage)) {
            List<Item> items =
                itemService.getItemsByStoreIdAndProductSkusOrItemSkusIn(storeId, productSkuWithoutProductMainImage,
                    new ArrayList<>());
            productSkuAndImageUrlMap = items.stream().filter(item -> StringUtils.isNotBlank(item.getMainImageUrl()))
                .collect(Collectors.toMap(Item::getProductSku, Item::getMainImageUrl, (v1, v2) -> v1));
          }

          if (MapUtils.isNotEmpty(productSkuAndImageUrlMap) || MapUtils.isNotEmpty(productSkusToItemMap)) {
            for (ProductSolr productSolr : productSolrWithoutProductMainImage) {
              if (productSkusToItemMap.containsKey(productSolr.getProductSku())) {
                productSolr.setProductMainImage(
                    productSkusToItemMap.get(productSolr.getProductSku()).stream().findFirst().map(Item::getMainImageUrl)
                        .orElse(productSolr.getProductMainImage()));
              } else if (productSkuAndImageUrlMap.containsKey(productSolr.getProductSku())) {
                productSolr.setProductMainImage(productSkuAndImageUrlMap.get(productSolr.getProductSku()));
              }
              if (productMap.containsKey(productSolr.getProductSku())) {
                productToReindex.add(productMap.get(productSolr.getProductSku()));
              }
            }
            productAndItemSolrIndexerService.updateProductDetailsInSolr(productToReindex);
          }
        }
      }
    }  catch (Exception e) {
      log.error("Error while setting main image in solr for product : {} ", productSolrs, e);
    }
  }

  @Override
  public void setCategoryEligibleForSizeChartCode(String storeId, List<ProductSolr> productSolrs,
      String sizeChartAttributeCode, List<Product> products) {
    Map<String, List<ProductAttribute>> productSkuAndDefiningAttributeMap =
        products.stream().collect(Collectors.toMap(Product::getProductSku, Product::getDefiningAttributes));
    for (ProductSolr productSolr : productSolrs) {
      setCategoryEligibleForSizeChartFlagForProductSolr(
          productSkuAndDefiningAttributeMap.get(productSolr.getProductSku()), sizeChartAttributeCode, productSolr);
    }
  }


  private void setCategoryEligibleForSizeChartFlagForProductSolr(List<ProductAttribute> productAttributes, String attributeCode,
      ProductSolr productSolr) {
    if (CollectionUtils.isNotEmpty(productAttributes)) {
      List<String> allAttributeCodes =
          productAttributes.stream().map(ProductAttribute::getProductAttributeDetails).flatMap(List::stream)
              .map(ProductAttributeDetail::getAttributeCode).collect(Collectors.toList());
      productSolr.setProductCategoryEligibleForSizeChart(allAttributeCodes.contains(attributeCode));

    }
  }

  @Override
  public void processFinalSaveAndPublishEventForContentEditOnly(EditProductDetailDTO editProductDetailDTO) {
    if (editProductDetailDTO.isProductUpdated()) {
      Product product = editProductDetailDTO.getProduct();
      List<Item> itemList = new ArrayList<>(editProductDetailDTO.getUpdatedItemMap().values());
      ProductAndItemsVO productAndItemsVO =
          saveOperationService.saveProductAndItemsWithoutPublishingEvent(new ProductAndItemsVO(product, itemList));
      if (CollectionUtils.isNotEmpty(productAndItemsVO.getItems())) {
        //ItemChangeEventType is a transient field , hence publishing event on itemList
        saveAndPublishService.publishListOfItems(itemList);
      }
    }
    List<ItemPickupPoint> itemPickupPointList =
        new ArrayList<>(editProductDetailDTO.getUpdatedItemPickupPointMap().values());
    if (editProductDetailDTO.isFreeSampleToggledOn() && CollectionUtils.isNotEmpty(itemPickupPointList)) {
      itemPickupPointService.saveItemPickupPoint(itemPickupPointList);
      saveAndPublishService.publishItemPickupPointDataChangeEvent(itemPickupPointList, new ArrayList<>(),
        Collections.EMPTY_MAP);
      itemPickupPointList.forEach(itemPickupPoint -> saveAndPublishService.publishViewConfigChange(itemPickupPoint));
    }
    if (Objects.nonNull(editProductDetailDTO.getExistingProductScore())) {
      saveHistoryForProductScoreUpdate(editProductDetailDTO.getProduct(),
          objectConverterService.toProductScore(editProductDetailDTO.getExistingProductScore()),
          editProductDetailDTO.getProduct().getProductScore());
    }
    if (Objects.nonNull(editProductDetailDTO.getPreOrderVO())) {
      Product product = editProductDetailDTO.getProduct();
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_PREORDER_STATUS, product.getProductSku(),
          new ProductPreOrderStatus(product.getProductCode(), product.getProductSku(),
              editProductDetailDTO.getPreOrderVO()));
    }
  }

  private void setL5CountFromDbFor0Count(String storeId, List<String> zeroVariantProductSkus, Page<ProductSolr> productSolrs) {
    Map<String, List<ItemPickupPoint>> productSkuToItemPickupPoint = new HashMap<>();
    for (String productSku : zeroVariantProductSkus) {
      List<ItemPickupPoint> itemPickupPoints =
        this.itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(storeId,
          productSku);
      productSkuToItemPickupPoint.putIfAbsent(productSku, itemPickupPoints);
    }
    for (ProductSolr productSolr : productSolrs) {
      if (productSkuToItemPickupPoint.containsKey(productSolr.getProductSku())) {
        productSolr.setL5Count(productSkuToItemPickupPoint.get(productSolr.getProductSku()).size());
      }
    }
  }

  @Override
  public Map<String, List<Item>> getItemByProductSkus(String storeId, List<String> singleVariantProductSkus) {
    Map<String, List<Item>> productSkusMap = new HashMap<>();
    if (multiGetItemsInL3Listing) {
      productSkusMap = Lists.partition(singleVariantProductSkus, 10).stream().map(HashSet::new).map(
              productSkuSet -> cacheItemHelperService.getCacheableItemsByProductSkusForSuspensionList(storeId,
                  productSkuSet)).flatMap(List::stream)
          .collect(Collectors.groupingBy(Item::getProductSku, Collectors.toList()));
    }
    singleVariantProductSkus.removeAll(productSkusMap.keySet());
    //Get items from db for non cached skus
    if (CollectionUtils.isNotEmpty(singleVariantProductSkus)) {
      for (String productSku : singleVariantProductSkus) {
        productSkusMap.put(productSku, cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, productSku));
      }
    }
    return productSkusMap;
  }

  private List<ProductL3SummaryResponse> convertToProductL3SummaryResponse(Page<ProductSolr> productSolrs,
    Map<String, List<Item>> productSkuItemMap,
    Map<String, ItemPickupPoint> itemPickupPointMap, List<Product> products,
    Map<String, BusinessPartnerPickupPoint> pickupPointCodeMap, boolean onlyDefaultViewConfig) {
    List<ProductL3SummaryResponse> productL3SummaryResponses = new ArrayList<>();
    Map<String, Product> productMap = products.stream().filter(product -> Objects.nonNull(product.getProductScore()))
        .collect(Collectors.toMap(Product::getProductSku, product -> product));
    for (ProductSolr productSolr : productSolrs) {
      String masterCatalog = productSolr.getMasterCatalog();
      ProductL3SummaryResponse productL3SummaryResponse =
        ProductL3SummaryResponse.builder().productSku(productSolr.getProductSku())
          .brand(productSolr.getBrand()).isArchived(
            Objects.nonNull(productSolr.getIsArchived()) ? productSolr.getIsArchived() : false)
          .markForDelete(productSolr.isMarkForDelete()).suspended(productSolr.isSuspended())
          .maxNormalPrice(Objects.nonNull(productSolr.getMaximumListPrice()) ?
            productSolr.getMaximumListPrice().longValue() :
            0).minNormalPrice(Objects.nonNull(productSolr.getMinimumListPrice()) ?
            productSolr.getMinimumListPrice().longValue() :
            0).maxSellingPrice(Objects.nonNull(productSolr.getMaximumSellingPrice()) ?
            productSolr.getMaximumSellingPrice().longValue() :
            0).minSellingPrice(Objects.nonNull(productSolr.getMinimumSellingPrice()) ?
            productSolr.getMinimumSellingPrice().longValue() :
            0).merchantCode(productSolr.getMerchantCode()).variantCount(
            Objects.nonNull(productSolr.getVariantCount()) ? productSolr.getVariantCount() : 0)
          .off2OnChannelActive(
            Objects.nonNull(productMap.get(productSolr.getProductSku())) && productMap.get(
              productSolr.getProductSku()).isOff2OnChannelActive())
          .productCode(productSolr.getProductCode())
          .productMainImage(productSolr.getProductMainImage())
          .productName(productSolr.getProductName()).promoLabels(new ArrayList<>())
          .freeSample(productSolr.isFreeSample()).l5Count(productSolr.getL5Count())
            .pickupPointCodes(productSolr.getPickupPointCodes()).isSynchronized(productSolr.isSynchronized())
            .bundleProduct(productSolr.isBundleProduct()).sizeChartCode(productSolr.getSizeChartCode())
            .productCategoryEligibleForSizeChart(productSolr.isProductCategoryEligibleForSizeChart())
            .inStock(productSolr.getInStock()).build();
      if (productMap.containsKey(productSolr.getProductSku())) {
        Product product = productMap.get(productSolr.getProductSku());
        productL3SummaryResponse.setCreatedDate(product.getCreatedDate());
        productL3SummaryResponse.setCreatedBy(product.getCreatedBy());
        productL3SummaryResponse.setUpdatedBy(product.getUpdatedBy());
        productL3SummaryResponse.setUpdatedDate(product.getUpdatedDate());
        productL3SummaryResponse.setCncActivated(product.isCncActivated());
        productL3SummaryResponse.setFbbActivated(product.isFbbActivated());
        productL3SummaryResponse.setB2cActivated(product.getB2cActivated());
        productL3SummaryResponse.setB2bActivated(product.isB2bActivated());
        productL3SummaryResponse.setBundleProduct(product.isBundleProduct());
        productL3SummaryResponse.setProductType(product.getProductType());
        productL3SummaryResponse.setDimensionsMissing(Optional.ofNullable(product.getDimensionsMissing()).orElseGet(
            () -> Optional.ofNullable(product.getMissingFields()).map(CollectionUtils::isNotEmpty).orElse(null)));
        productL3SummaryResponse.setSizeChartCode(product.getSizeChartCode());
      }
      setGdnParameters(productL3SummaryResponse, productSolr);
      if (StringUtils.isNotBlank(masterCatalog)) {
        String[] strings = masterCatalog.split(solrStringDelimiter);
        productL3SummaryResponse.setCatalogCode(strings[0]);
        productL3SummaryResponse.setCategoryCode(strings[1]);
      }
      if (Objects.nonNull(productMap.get(productSolr.getProductSku()))) {
        ProductScore productScore = productMap.get(productSolr.getProductSku()).getProductScore();
        if (Objects.nonNull(productScore)) {
          ProductScoreResponse productScoreResponse = new ProductScoreResponse();
          BeanUtils.copyProperties(productScore, productScoreResponse);
          productL3SummaryResponse.setProductScore(productScoreResponse);
        }
      }
      if (productSolr.getL5Count() == 1) {
        List<Item> items =
          productSkuItemMap.getOrDefault(productSolr.getProductSku(), new ArrayList<>());
        if(addDeleteVariantSwitch) {
          items = items.stream().filter(item -> !item.isMarkForDelete()).collect(Collectors.toList());
        }
        if (CollectionUtils.isNotEmpty(items) && items.size() == 1) {
          setItemResponse(productSolr, items.get(0), productL3SummaryResponse,
            productMap.get(productSolr.getProductSku()),
            itemPickupPointMap, pickupPointCodeMap, onlyDefaultViewConfig);
        } else if (CollectionUtils.isNotEmpty(items)) {
          productL3SummaryResponse.setVariantCount(items.size());
        }
      } else {
        setPromoLabelsAtL3Level(productSolr, productL3SummaryResponse);
      }
      productL3SummaryResponses.add(productL3SummaryResponse);
    }
    return productL3SummaryResponses;
  }

  private void setGdnParameters(ProductL3SummaryResponse productL3SummaryResponse, ProductSolr productSolr) {
    productL3SummaryResponse.setCreatedDate(productSolr.getCreatedDate());
    productL3SummaryResponse.setUpdatedDate(productSolr.getUpdatedDate());
    productL3SummaryResponse.setStoreId(productSolr.getStoreId());
  }

  private void setPromoLabelsAtL3Level(ProductSolr productSolr, ProductL3SummaryResponse productL3SummaryResponse) {
    if (CollectionUtils.isNotEmpty(productSolr.getPromoItemSkus())) {
      productL3SummaryResponse.getPromoLabels().add(SolrConstants.PROMO_PROMO_TYPE);
    }
    if (CollectionUtils.isNotEmpty(productSolr.getWholesaleItemSkus())) {
      productL3SummaryResponse.getPromoLabels().add(SolrConstants.WHOLESALE_PROMO_TYPE);
    }
    if (productL3SummaryResponse.isOff2OnChannelActive()) {
      productL3SummaryResponse.getPromoLabels().add(SolrConstants.IN_STORE_PROMO_TYPE);
    }
    if (Objects.nonNull(productSolr.getIsPreOrderActive()) && productSolr.getIsPreOrderActive()) {
      productL3SummaryResponse.getPromoLabels().add(SolrConstants.PREORDER_PROMO_TYPE);
    }
    if (productSolr.isFreeSample()) {
      productL3SummaryResponse.getPromoLabels().add(SolrConstants.FREE_SAMPLE_TYPE);
    }
  }

  private void setItemResponse(ProductSolr productSolr, Item item, ProductL3SummaryResponse productL3SummaryResponse,
    Product product, Map<String, ItemPickupPoint> itemPickupPointMap,
    Map<String, BusinessPartnerPickupPoint> pickupPointCodeMap, boolean onlyDefaultViewConfig) {
    ItemL4SummaryResponse itemL4SummaryResponse = new ItemL4SummaryResponse();
    ItemPickupPoint itemPickupPoint = itemPickupPointMap.getOrDefault(item.getItemSku(), null);
    B2bFieldsDTO b2bFieldsDTO = new B2bFieldsDTO();
    if (Objects.isNull(itemPickupPoint)) {
      return;
    }

    itemL4SummaryResponse.setStoreId(item.getStoreId());
    itemL4SummaryResponse.setItemSku(item.getItemSku());
    itemL4SummaryResponse.setMerchantSku(item.getMerchantSku());
    if (!cncForWarehouseFeatureSwitch) {
      itemL4SummaryResponse.setCncActivated(itemPickupPoint.isCncActive());
    } else {
      ItemViewConfig cncItemViewConfig =
          itemPickupPoint.getSingleItemViewConfigByChannelDefaultEmpty(ChannelName.CNC.name());
      if (cncItemViewConfig.isDiscoverable() || cncItemViewConfig.isBuyable()) {
        itemL4SummaryResponse.setCncActivated(true);
      }
    }
    itemL4SummaryResponse.setItemCode(item.getItemCode());
    itemL4SummaryResponse.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
    if (itemPickupPoint.isWholesalePriceExists()) {
      itemL4SummaryResponse.setWholesalePriceActivated(
          CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())
              && itemPickupPoint.getActivePromoBundlings().contains(Constants.WHOLESALE_PRICE));
    }
    CommonUtil.setPromoLabelsAtL4(productSolr, itemPickupPoint, item, itemL4SummaryResponse,
        populateLabelForUpcomingPromo, populateLabelForPwpPromo);
    if (CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())) {
      itemPickupPoint.getActivePromoBundlings().remove(Constants.WHOLESALE_PRICE);
      itemL4SummaryResponse.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    }
    itemL4SummaryResponse.setCreatedDate(item.getCreatedDate());
    itemL4SummaryResponse.setUpdatedDate(item.getUpdatedDate());
    itemL4SummaryResponse.setForceReview(item.isForceReview());
    itemL4SummaryResponse.setIsLateFulfillment(item.isLateFulfillment());
    itemL4SummaryResponse.setMarkForDelete(item.isMarkForDelete());
    itemL4SummaryResponse.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    itemL4SummaryResponse.setForceReview(item.isForceReview());
    itemL4SummaryResponse.setPromoBundling(itemPickupPoint.isPromoBundling());
    itemL4SummaryResponse.setPickupPointCode(itemPickupPoint.getPickupPointCode());
    itemL4SummaryResponse.setDistribution(itemPickupPoint.isDistribution());
    if (Objects.nonNull(pickupPointCodeMap.get(itemPickupPoint.getPickupPointCode()))) {
      itemL4SummaryResponse.setPickupPointName(
        pickupPointCodeMap.get(itemPickupPoint.getPickupPointCode()).getName());
      itemL4SummaryResponse.setPickupPointCncActive(
          pickupPointCodeMap.get(itemPickupPoint.getPickupPointCode()).isCncActivated());
      itemL4SummaryResponse.setPickupPointDeliveryActive(
          pickupPointCodeMap.get(itemPickupPoint.getPickupPointCode()).isDelivery());
    }
    itemL4SummaryResponse.setMerchantSku(itemPickupPoint.getMerchantSku());
    itemL4SummaryResponse.setFreeSample(item.isFreeSample());
    itemL4SummaryResponse.setMerchantPromoDiscountActivated(itemPickupPoint.isMerchantPromoDiscount() && Objects
        .nonNull(itemPickupPoint.getPrice().stream().findFirst().get().getMerchantPromoDiscountPrice()));
    itemL4SummaryResponse.setMerchantPromoDiscount(itemPickupPoint.isMerchantPromoDiscount());
    com.gdn.x.product.service.util.ResponseHelper.updateMerchantPromoDiscountFlags(itemPickupPoint,
        itemL4SummaryResponse, replaceMerchantPromoDiscountBasedOnEndDate,
        ignoreGeneratingMerchantPromoDiscountWhenPromoPriceIsNull);
    itemL4SummaryResponse.setPromoTypes(CommonUtil.getPromoTypesByItem(itemPickupPoint));
    List<ItemViewConfigDTO> itemViewConfigDTOS = new ArrayList<>();
    for (ItemViewConfig itemViewConfig : itemPickupPoint.getAllItemViewConfigs()) {
      ItemViewConfigDTO itemViewConfigDTO = new ItemViewConfigDTO();
      BeanUtils.copyProperties(itemViewConfig, itemViewConfigDTO);
      setViewConfigSchedules(itemViewConfig, itemViewConfigDTO);
      itemViewConfigDTOS.add(itemViewConfigDTO);
    }
    if (onlyDefaultViewConfig) {
      itemViewConfigDTOS = itemViewConfigDTOS.stream()
          .filter(itemViewConfigDTO -> Constants.DEFAULT.equals(itemViewConfigDTO.getChannel()))
          .collect(Collectors.toList());
    }
    itemL4SummaryResponse.setItemViewConfigs(itemViewConfigDTOS);
    if (Objects.nonNull(itemPickupPoint.getB2bFields())){
      BeanUtils.copyProperties(itemPickupPoint.getB2bFields(), b2bFieldsDTO);
    }
    itemL4SummaryResponse.setB2bFieldsDTO(b2bFieldsDTO);
    itemL4SummaryResponse.setPriceEditDisabled(itemService.isPriceEditDisabled(itemPickupPoint));
    if (Objects.nonNull(product) && Objects.nonNull(product.getProductType())) {
      itemL4SummaryResponse.setProductType(String.valueOf(product.getProductType().getCode()));
    }
    if (item.isSynchronized()) {
      itemL4SummaryResponse.setMasterDataItemImages(CommonUtil.toMasterDataItemImageDTO(item.getMainImageUrl()));
      itemL4SummaryResponse.setGeneratedItemName(item.getGeneratedItemName());
    } else {
      if (Objects.nonNull(item.getMasterDataItem())) {
        itemL4SummaryResponse.setMasterDataItemImages(
          Optional.ofNullable(item.getMasterDataItem().getMasterDataItemImages())
            .orElse(new ArrayList<>()).stream().map(this::getMasterDataItemImageDTO)
            .collect(Collectors.toList()));
        itemL4SummaryResponse.setGeneratedItemName(item.getMasterDataItem().getGeneratedItemName());
      }
    }
    List<PriceDTO> priceDTOS = new ArrayList<>();
    setPriceInResponse(priceDTOS, itemPickupPoint);
    itemL4SummaryResponse.setPrice(new HashSet<>(priceDTOS));
    itemL4SummaryResponse.setVersion(itemPickupPoint.getVersion());
    itemL4SummaryResponse.setActivePromoBundlings(itemPickupPoint.getActivePromoBundlings());
    itemL4SummaryResponse.setPromoBundling(itemPickupPoint.isPromoBundling());
    itemL4SummaryResponse.setWholesalePriceExists(itemPickupPoint.isWholesalePriceExists());
    productL3SummaryResponse.setItemL4SummaryResponse(itemL4SummaryResponse);
  }

  private void setViewConfigSchedules(ItemViewConfig itemViewConfig, ItemViewConfigDTO itemViewConfigDTO) {
    if (Objects.nonNull(itemViewConfig.getItemBuyableSchedules()) &&
        !CommonUtil.isScheduleCompleted(itemViewConfig.getItemBuyableSchedules().getEndDateTime())) {
      itemViewConfigDTO.setItemBuyableSchedules(new ItemBuyableScheduleDTO());
      BeanUtils.copyProperties(itemViewConfig.getItemBuyableSchedules(), itemViewConfigDTO.getItemBuyableSchedules());
    }
    if (Objects.nonNull(itemViewConfig.getItemDiscoverableSchedules()) &&
        !CommonUtil.isScheduleCompleted(itemViewConfig.getItemDiscoverableSchedules().getEndDateTime())) {
      itemViewConfigDTO.setItemDiscoverableSchedules(new ItemDiscoverableScheduleDTO());
      BeanUtils.copyProperties(itemViewConfig.getItemDiscoverableSchedules(), itemViewConfigDTO.getItemDiscoverableSchedules());
    }
  }

  private void setPriceInResponse(List<PriceDTO> priceDTOS, ItemPickupPoint itemPickupPoint) {
    for (Price price : itemPickupPoint.getPrice()) {
      PriceDTO priceDTO = new PriceDTO();
      BeanUtils.copyProperties(price, priceDTO, "merchantPromoDiscountPrice", "listOfDiscountPrices");
      if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
        DiscountPriceDTO discountPriceDTO = getDiscountPriceDTO(price.getMerchantPromoDiscountPrice());
        priceDTO.setMerchantPromoDiscountPrice(discountPriceDTO);
      }
      if (CollectionUtils.isNotEmpty(price.getListOfDiscountPrices())) {
        priceDTO.setListOfDiscountPrices(
            price.getListOfDiscountPrices().stream().map(this::getDiscountPriceDTO).collect(Collectors.toList()));
      }
      if (itemPickupPoint.isMerchantPromoDiscount()) {
        setMerchantDiscountPrice(priceDTO);
      }
      if (!replaceOfferPriceWithMerchantPromoPriceInSellerSide) {
        priceDTO.setOfferPrice(price.getOfferPrice());
      }
      priceDTOS.add(priceDTO);
    }
  }

  private DiscountPriceDTO getDiscountPriceDTO(DiscountPrice discountPrice) {
    DiscountPriceDTO discountPriceDTO = new DiscountPriceDTO();
    BeanUtils.copyProperties(discountPrice, discountPriceDTO);
    return discountPriceDTO;
  }

  private void setMerchantDiscountPrice(PriceDTO price) {
    if (Objects.nonNull(price.getMerchantPromoDiscountPrice())) {
      price.setOfferPrice(price.getMerchantPromoDiscountPrice().getDiscountPrice());
    }
  }

  private MasterDataItemImageDTO getMasterDataItemImageDTO(MasterDataItemImage masterDataItemImage) {
    MasterDataItemImageDTO masterDataItemImageDTO = new MasterDataItemImageDTO();
    BeanUtils.copyProperties(masterDataItemImage, masterDataItemImageDTO);
    return masterDataItemImageDTO;
  }

  @Override
  public Page<ProductNameSuggestionResponse> getProductNamesByKeyword(String storeId,
      ProductSummaryRequest productFilter, PageRequest pageRequest) {
    List<ProductNameSuggestionResponse> productSkuAndNameList = new ArrayList<>();
    checkArgument(StringUtils.isNotBlank(productFilter.getMerchantCode()),
        ProductServiceImpl.MERCHANT_CODE_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    ProductSummaryRequestVo productSummaryRequestVo = new ProductSummaryRequestVo();
    BeanUtils.copyProperties(productFilter, productSummaryRequestVo);
    Page<ProductSolr> productSolrPage =
        productSolrRepository.getProductNameByProductSummaryRequest(storeId, productSummaryRequestVo, pageRequest);
    if (ResponseHelper.validateSolrResponse(productSolrPage)) {
      productSkuAndNameList =
          productSolrPage.getContent().stream().map(this::toProductSkuAndName).collect(Collectors.toList());
    }
    return new PageImpl<>(productSkuAndNameList, pageRequest, productSolrPage.getTotalElements());
  }

  private ProductNameSuggestionResponse toProductSkuAndName(ProductSolr productSolr) {
    return new ProductNameSuggestionResponse(productSolr.getProductName(), productSolr.getProductSku());
  }

  @Override
  public List<String> getPromoLabels(ProductAndItemsVO productAndItemsVO, ProductL3Response productL3Response) {
    List<String> promoLabels = new ArrayList<>();
    checkArgument(Objects.nonNull(productAndItemsVO), ProductServiceImpl.PRODUCT_AND_ITEMS_REQUEST_VO_MUST_NOT_BE_NULL);
      List<ItemPickupPoint> itemPickupPoints = itemPickupPointService
          .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(productAndItemsVO.getProduct().getStoreId(),
              productAndItemsVO.getProduct().getProductSku());
      setOtherFbbPickupPointMapped(productL3Response, itemPickupPoints);
      productAndItemsVO.setItemPickupPoints(itemPickupPoints);
      productL3Response.setActiveL5Mapped(CollectionUtils.isNotEmpty(itemPickupPoints));
      itemPickupPoints.stream().filter(
              itemPickupPoint -> CollectionUtils.isNotEmpty(itemPickupPoint.getActivePromoBundlings())
                  && itemPickupPoint.getActivePromoBundlings().contains(Constants.COMPLEMENTARY_FREE_SAMPLE_PRODUCT))
          .map(itemPickupPoint -> true).forEach(productL3Response::setActiveFreeSamplePromo);
    getPromoLabelsFromDbResponse(productAndItemsVO, promoLabels, productL3Response);
    return promoLabels;
  }

  private void setOtherFbbPickupPointMapped(ProductL3Response productL3Response,
      List<ItemPickupPoint> itemPickupPoints) {
    if (productL3Response.isFbbActivated()) {
      Map<String, Boolean> itemSkuAndFbbMappedMap = itemPickupPoints.stream()
          .collect(Collectors.toMap(ItemPickupPoint::getItemSku, itemPickupPoint -> false, (a, b) -> a));
      itemPickupPoints.stream().filter(ItemPickupPoint::isFbbActivated)
          .forEach(itemPickupPoint -> itemSkuAndFbbMappedMap.put(itemPickupPoint.getItemSku(), true));
      productL3Response.setOtherFbbPickupPointCodesMapped(itemSkuAndFbbMappedMap.entrySet().stream()
          .anyMatch(itemSkuAndFbbMap -> Boolean.FALSE.equals(itemSkuAndFbbMap.getValue())));
      if (productL3Response.isOtherFbbPickupPointCodesMapped()) {
        if (Objects.isNull(productL3Response.getFbbPickupPointCodes())) {
          productL3Response.setFbbPickupPointCodes(Collections.singletonList(NO_FBB));
        } else {
          List<String> fbbPickupPointCodes = new ArrayList<>(productL3Response.getFbbPickupPointCodes());
          fbbPickupPointCodes.add(NO_FBB);
          productL3Response.setFbbPickupPointCodes(fbbPickupPointCodes);
        }
      }
    }
  }

  private void getPromoLabelsFromDbResponse(ProductAndItemsVO productAndItemsVO, List<String> promoLabels,
      ProductL3Response productL3Response) {
    if (productAndItemsVO.getItemPickupPoints().stream().anyMatch(
        itemPickupPoint -> CommonUtil.isItemAddedToPromo(itemPickupPoint, populateLabelForUpcomingPromo,
            populateLabelForPwpPromo))) {
        promoLabels.add(SolrConstants.PROMO_PROMO_TYPE);
      }
      if (productAndItemsVO.getItemPickupPoints().stream().anyMatch(CommonUtil::isItemAddedToWholesalePrice)) {
        productL3Response.setWholesalePriceActivated(true);
        if (productAndItemsVO.getItems().size() == ONE) {
          promoLabels.add(Constants.WHOLESALE_PRICE);
        } else {
          promoLabels.add(SolrConstants.WHOLESALE_PROMO_TYPE);
        }
      }
    productL3Response.setWholesalePriceExists(
        productAndItemsVO.getItemPickupPoints().stream().anyMatch(ItemPickupPoint::isWholesalePriceExists));
    if (Objects.nonNull(productAndItemsVO.getProduct())) {
      if (Objects.nonNull(productAndItemsVO.getProduct().getPreOrder()) && Objects
          .nonNull(productAndItemsVO.getProduct().getPreOrder().getIsPreOrder()) && Boolean.TRUE
          .equals(productAndItemsVO.getProduct().getPreOrder().getIsPreOrder())) {
        promoLabels.add(SolrConstants.PREORDER_PROMO_TYPE);
      }
      if (productAndItemsVO.getProduct().isOff2OnChannelActive()) {
        promoLabels.add(SolrConstants.IN_STORE_PROMO_TYPE);
      }
      if (productAndItemsVO.getProduct().isFreeSample()) {
        promoLabels.add(SolrConstants.FREE_SAMPLE_TYPE);
      }
    }
  }

  @Override
  public Long getProductsCountByCategory(String storedId, CategoryBrandRequest categoryBrandRequest,
      String merchantCode) {
    List<List<String>> categoryCodesInBatches = new ArrayList<>();
    if (Objects.isNull(categoryBrandRequest) || CollectionUtils.isEmpty(categoryBrandRequest.getCategory())) {
      categoryBrandRequest = new CategoryBrandRequest();
      categoryCodesInBatches.add(null);
    } else {
      List<String> categoryCodes = productCategoryBaseOutbound.getCnCategoryCodes(categoryBrandRequest.getCategory());
      if (CollectionUtils.isEmpty(categoryCodes)) {
        return Constants.LONG_ZERO;
      }
      String batchSize = systemParameterService
          .findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterNames.SOLR_CATEGORY_LIST_BATCH_SIZE)
          .getValue();
      categoryCodesInBatches = Lists.partition(categoryCodes, Integer.valueOf(batchSize));
    }
    if (CollectionUtils.isEmpty(categoryBrandRequest.getBrand())) {
      categoryBrandRequest.setBrand(null);
    }
    return solrRepository
        .getCountByStoreIdAndCategoryAndBrandAndMarkForDeleteFalse(storedId, categoryCodesInBatches,
            categoryBrandRequest.getBrand(), merchantCode);
  }

  @Override
  public Map<String, PreOrder> getPreOrderStatusByProductSkus(List<String> productSkus) {
    if (CollectionUtils.isEmpty(productSkus)) {
      return new HashMap<>();
    }
    log.info("Fetching preorder details for productskus : {}", productSkus);
    Map<String, PreOrder> preOrderMap = new HashMap<>();
    for (String productSku : productSkus) {
      Product product =
          productCacheHelperService.findProductByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, productSku);
      if (Objects.nonNull(product)) {
        preOrderMap.put(product.getProductSku(), product.getPreOrder());
      }
    }
    return preOrderMap;
  }

  @Override
  public EditProductDetailDTO generateEditProductDetailDTO(boolean updateCategory, String productSku, String requestId,
      EditChangeType editChangeType) throws Exception {
    EditProductDetailDTO editProductDetailDTO = new EditProductDetailDTO();
    Product currProduct = this.getProduct(Constants.DEFAULT_STORE_ID, productSku);
    checkState(Objects.nonNull(currProduct), ProductServiceImpl.PRODUCT_NOT_FOUND);
    editProductDetailDTO.setProduct(currProduct);
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> allItemPickupPointList = new ArrayList<>();
    ProductDetailResponse productDetailResponse = null;
    items = cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(currProduct.getStoreId(),
        currProduct.getProductSku());
    checkState(CollectionUtils.isNotEmpty(items), ProductServiceImpl.ITEM_NOT_FOUND);
    editProductDetailDTO.setAllItemMap(items.stream()
        .collect(Collectors.toMap(Item::getItemSku, Function.identity(), (oldValue, newValue) -> newValue)));
    editProductDetailDTO.setCncItemCount(items.stream().filter(Item::isCncActivated).count());
    allItemPickupPointList =
        itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(currProduct.getStoreId(),
            currProduct.getProductSku());
    checkState(CollectionUtils.isNotEmpty(allItemPickupPointList), ProductServiceImpl.ITEM_PICKUP_POINT_NOT_FOUND);
    editProductDetailDTO.setAllItemPickupPointMap(allItemPickupPointList.stream().collect(
        Collectors.toMap(ItemPickupPoint::getOfflineItemId, Function.identity(), (oldValue, newValue) -> newValue)));
    productDetailResponse =
        productCategoryBaseOutbound.getProductDetailByProductCode(requestId, Constants.DEFAULT_USERNAME,
            currProduct.getProductCode());
    checkState(Objects.nonNull(productDetailResponse), ProductServiceImpl.PRODUCT_DETAIL_RESPONSE_NOT_FOUND);
    editProductDetailDTO.setProductDetailResponse(productDetailResponse);
    editProductDetailDTO.setUpdateCategory(updateCategory);
    editProductDetailDTO.setEditChangeType(editChangeType);
    return editProductDetailDTO;
  }

  @Override
  public EditProductDetailDTO updateEditedProduct(String requestId, Product product,
      Map<String, ItemViewConfig> itemViewConfigMap, boolean updateCategory,
      boolean isCombinedEditRequest, EditProductDetailDTO editProductDetailDTO,
      ProductEditRequest productEditRequest)
      throws Exception {
    Product currProduct = null;
    List<Item> items = new ArrayList<>();
    List<ItemPickupPoint> allItemPickupPointList = new ArrayList<>();
    ProductDetailResponse productDetailResponse = null;
    Map<String, String> itemCodeAndOmniChannelSkuMap =
        Optional.ofNullable(productEditRequest.getOmniChannelSkuUpdateRequests()).map(
                omniChannelSkuUpdateRequests -> omniChannelSkuUpdateRequests.stream().collect(
                    Collectors.toMap(OmniChannelSkuUpdateRequest::getItemCode,
                        OmniChannelSkuUpdateRequest::getOmniChannelSku, (existing, replacement) -> replacement)))
            .orElse(new HashMap<>());
    if (isCombinedEditRequest) {
      currProduct = editProductDetailDTO.getProduct();
      items = new ArrayList<>(editProductDetailDTO.getAllItemMap().values());
      allItemPickupPointList = new ArrayList<>(editProductDetailDTO.getAllItemPickupPointMap().values());
      productDetailResponse = editProductDetailDTO.getProductDetailResponse();
    } else {
      currProduct = this.getProduct(Constants.DEFAULT_STORE_ID, productEditRequest.getProductSku());
      checkState(Objects.nonNull(currProduct), ProductServiceImpl.PRODUCT_NOT_FOUND);
    }
    ProductScore existingProductScore = new ProductScore();
    ProductPreOrderStatus productPreOrderStatus = new ProductPreOrderStatus();
    boolean freeSampleChanged = false;
    boolean off2OnActiveChanged = false;
    boolean preOrderChanged = false;
    if (Objects.nonNull(currProduct.getProductScore())) {
      BeanUtils.copyProperties(currProduct.getProductScore(), existingProductScore);
    }
    if (Objects.nonNull(product)) {
      List<Boolean> productAndSpecialAttributeChanged = compareAndUpdateProductFields(product, currProduct);
      boolean specialAttributeChanged = productAndSpecialAttributeChanged.get(1);
      Map<String, Boolean> stringBooleanMap = compareProductFlagsChanged(product, currProduct);
      off2OnActiveChanged = stringBooleanMap.getOrDefault(Constants.OFF_2ON_ACTIVE_CHANGED,false);
      freeSampleChanged = stringBooleanMap.getOrDefault(Constants.FREE_SAMPLE_CHANGED,false);
      preOrderChanged = stringBooleanMap.getOrDefault(Constants.PREORDER_DETAILS_CHANGED, false);
      if (Objects.nonNull(currProduct.getProductScore())) {
        try {
          productDetailResponse =
              generateAndSetProductScore(requestId, currProduct, specialAttributeChanged, items, isCombinedEditRequest,
                  productDetailResponse);
        } catch (Exception e) {
          LOG.error("Error while setting product score for : {} ", product.getProductSku(), e);
        }
      }
      if(off2OnActiveChanged){
        Map<String, Boolean> off2OnFLagByProductSkuMap = new HashMap<>();
        off2OnFLagByProductSkuMap.put(product.getProductSku(), product.isOff2OnChannelActive());
        if(isCombinedEditRequest){
          editProductDetailDTO.setOff2OnActiveChanged(true);
          items = updateOff2OnChannelForCombinedEdit(currProduct, items, product.isOff2OnChannelActive(),
              editProductDetailDTO);
        } else {
          updateOff2OnFlagByProductSkus(product.getStoreId(), off2OnFLagByProductSkuMap,
              GdnMandatoryRequestParameterUtil.getUsername(), GdnMandatoryRequestParameterUtil.getChannelId(),
              GdnMandatoryRequestParameterUtil.getClientId(), requestId, false);
          currProduct.setOff2OnChannelActive(product.isOff2OnChannelActive());
        }
      }
        if(freeSampleChanged){
        currProduct.setFreeSample(product.isFreeSample());
      }
        if(stringBooleanMap.getOrDefault(Constants.SIZE_CHART_CHANGED, false)){
          currProduct.setSizeChartCode(product.getSizeChartCode());
        }
      if (preOrderChanged && Objects.nonNull(product.getPreOrder())) {
        currProduct.setPreOrder(product.getPreOrder());
        PreOrderVO preOrderVO = new PreOrderVO();
        BeanUtils.copyProperties(product.getPreOrder(), preOrderVO);
        productPreOrderStatus =
          ProductPreOrderStatus.builder().productCode(currProduct.getProductCode())
            .productSku(currProduct.getProductSku()).preOrder(preOrderVO).build();
      }
      else if (preOrderChanged) {
        productPreOrderStatus = ProductPreOrderStatus.builder().productCode(currProduct.getProductCode())
          .productSku(currProduct.getProductSku()).preOrder(null).build();
      }
      updateMissingNonMandatoryFieldsForProduct(currProduct, product);
      Product updatedProduct = currProduct;
      ProductScore updatedProductScore = new ProductScore();
      if (Objects.nonNull(updatedProduct.getProductScore())) {
        BeanUtils.copyProperties(updatedProduct.getProductScore(), updatedProductScore);
      }
      if (!GdnObjects.equals(updatedProductScore, existingProductScore)) {
        if (isCombinedEditRequest) {
          editProductDetailDTO.setProductScoreChanged(true);
          editProductDetailDTO.setExistingProductScore(
              objectConverterService.toProductScoreVoFromProductScore(existingProductScore));
        } else {
          saveHistoryForProductScoreUpdate(currProduct, existingProductScore, updatedProductScore);
        }
      }
      product = updatedProduct;
    } else {
      product = currProduct;
    }

    if(!isCombinedEditRequest){
    items =
        itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID,
            productEditRequest.getProductSku());
    }
    Map<String, String> existingItemCodeAndOmniChannelSkuMap =
        items.stream().filter(item -> StringUtils.isNotBlank(item.getOmniChannelSku())).collect(
            Collectors.toMap(Item::getItemCode, Item::getOmniChannelSku,
                (oldValue, newValue) -> oldValue));
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    Map<String, List<ItemPickupPoint>> itemSkuAndItemPickupPointsMap =
        allItemPickupPointList.stream().collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
    List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
    for (Item item : items) {
      item.setItemChangeEventTypes(new ArrayList<>());
      item.setOmniChannelSku(itemCodeAndOmniChannelSkuMap.getOrDefault(item.getItemCode(), item.getOmniChannelSku()));
      if (isCombinedEditRequest) {
        itemPickupPointList = itemSkuAndItemPickupPointsMap.getOrDefault(item.getItemSku(), new ArrayList<>());
      } else {
        itemPickupPointList =
            itemPickupPointService.findByStoreIdAndItemSku(Constants.DEFAULT_STORE_ID, item.getItemSku());
      }
      item.setContentChanged(productEditRequest.isContentChanged());
      item.setFreeSample(product.isFreeSample());
      if (off2OnActiveChanged) {
        item.setOff2OnChannelActive(product.isOff2OnChannelActive());
        item.getItemChangeEventTypes().add(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE);
      }
      if (Objects.nonNull(productEditRequest.getLatefulfillment())) {
        item.setLateFulfillment(productEditRequest.getLatefulfillment());
      }

      Pair<Item, List<ItemPickupPoint>> itemAndUpdatedItemPickupPointList =
          processFreeSampleProduct(product, item, itemPickupPointList, freeSampleChanged, isCombinedEditRequest,
              editProductDetailDTO);
      item = itemAndUpdatedItemPickupPointList.getLeft();
      updatedItemPickupPointList.addAll(itemAndUpdatedItemPickupPointList.getRight());

      if (!freeSampleChanged) {
        objectConverterService
          .overrideL4DetailsFromL5(Collections.singletonList(item), itemPickupPointList);
      }
      if (productEditRequest.isContentChanged() || preOrderChanged) {
        item.getItemChangeEventTypes().add(ItemChangeEventType.ITEM_DATA_CHANGE);
        if (Objects.nonNull(item.getPristineDataItem())) {
          item.setPristineDataItem(null);
          item.getItemChangeEventTypes().add(ItemChangeEventType.PRISTINE_MAPPING_CHANGE);
          log.info("Removing the pristine mapping for itemSku {} because of content change", item.getItemSku());
        }
        item.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
        item.setUpdatedDate(new Date());
      }

      if (productEditRequest.isForceReview()) {
        if (Objects.nonNull(itemViewConfigMap.get(item.getItemSku()))) {
          item.setForceReview(true);
          item.setMarkForDelete(true);
          item.setArchivedBeforeEdit(item.isArchived());
          item.setArchived(true);
          item.getItemChangeEventTypes().add(ItemChangeEventType.ARCHIVED_FLAG_CHANGE);
          item = this.productHelperService
              .updateItemViewConfigForExistingChannel(item, itemViewConfigMap.get(item.getItemSku()));
          item.setUpdatedBy(GdnMandatoryRequestParameterUtil.getUsername());
          item.setUpdatedDate(new Date());
        }
      }
    }
    if (productEditRequest.isForceReview()) {
      product.setForceReview(true);
      product.setMarkForDelete(true);
      product.setTakenDown(true);
    }
    if (isCombinedEditRequest) {
      setMasterDataDetailsInProductAndItemsOnEdit(Constants.DEFAULT_STORE_ID, product, items, updateCategory,
          productDetailResponse);
      //saveProductAndItems after updating product content
      editProductDetailDTO.setProduct(product);
      editProductDetailDTO.setProductUpdated(true);
      CommonUtil.setUpdatedItemSkuAndItemMap(items, editProductDetailDTO);
      if (preOrderChanged) {
        editProductDetailDTO.setPreOrderVO(productPreOrderStatus.getPreOrder());
      }
      //publish view config change for updated L5 after saving pickup point
    } else {
      log.info("Updating edit product sku: {} , off2OnFlag: {} , freeSample: {} ", product.getProductSku(),
          product.isOff2OnChannelActive(), product.isFreeSample());
      log.info("item going for save is {} ", items);
      setMasterDataDetailsInProductAndItemsOnEdit(Constants.DEFAULT_STORE_ID, product, items, updateCategory,
          productDetailResponse);
      this.saveOperationService.saveProductAndItems(new ProductAndItemsVO(product, items), new ArrayList<>());
      if (preOrderChanged) {
        kafkaPublisher.send(ProductDomainEventName.PRODUCT_PREORDER_STATUS, productPreOrderStatus.getProductSku(),
            productPreOrderStatus);
      }
      updatedItemPickupPointList.forEach(
          pickupPointItem -> saveAndPublishService.publishViewConfigChange(pickupPointItem));
      List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
      CommonUtil.getHistoryListForOmniChannelSkuUpdate(product,
          existingItemCodeAndOmniChannelSkuMap, itemCodeAndOmniChannelSkuMap, items,
          auditTrailDtoList);
      if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
        saveAndPublishService.publishHistoryEvent(auditTrailDtoList);
      }
    }
    return editProductDetailDTO;
  }


  private void updateMissingNonMandatoryFieldsForProduct(Product currProduct, Product product) {
    if(instoreNewFlowEnabled){
      Set<String> missingFieldsSaved =
        Optional.of(currProduct).map(Product::getMissingFields).orElse(new HashSet<>());
      Set<String> missingFieldsFromRequest =
        Optional.of(product).map(Product::getMissingFields).orElse(new HashSet<>());
      if(!missingFieldsSaved.equals(missingFieldsFromRequest)){
        currProduct.getMissingFields().clear();
        currProduct.getMissingFields().addAll(missingFieldsFromRequest);
        //change in the request from PBP, log the saved data and update PBP's Request
        log.info("Updating MissingFields for product : {} , missing fields before save : {}",
          currProduct.getProductSku(), missingFieldsSaved);
      }
    }
  }

  @Override
  public void saveHistoryForProductScoreUpdate(Product currProduct, ProductScore existingProductScore,
      ProductScore updatedProductScore) {
    try {
      LOG.info(
          "Updating the product score history change on productUpdate for : {} with oldValues : {}, newValues : {}",
          currProduct.getProductSku(), existingProductScore, updatedProductScore);
      this.productScoreHistoryL3Service
          .saveProductScoreHistoryL3(currProduct.getStoreId(), currProduct.getProductSku(), existingProductScore,
              updatedProductScore);
    } catch (Exception e) {
      LOG.error("Error while updating the product score history for :{} ", currProduct.getProductSku(), e);
    }
  }

  private Pair<Item, List<ItemPickupPoint>> processFreeSampleProduct(Product product, Item item,
      List<ItemPickupPoint> itemPickupPointList, boolean freeSampleChanged, boolean isCombinedEditRequest,
      EditProductDetailDTO editProductDetailDTO) {
    List<ItemPickupPoint> updatedItemPickupPointList = new ArrayList<>();
    if (freeSampleChanged && product.isFreeSample()) {
      ItemViewConfig itemViewConfig = new ItemViewConfig();
      itemViewConfig.setDiscoverable(false);
      itemViewConfig.setBuyable(false);
      itemViewConfig.setChannel(channelService.getDefaultChannel());
      for (ItemPickupPoint itemPickupPointToSave : itemPickupPointList) {
        Set<ItemViewConfig> itemViewConfigSet = ImmutableSet.of(itemViewConfig);
        Set<ItemViewConfig> checkItemViewConfigSet =
            itemPickupPointToSave.getItemViewConfigByChannel(Constants.DEFAULT_CHANNEL);
        isDiscoverableFlagChanged(checkItemViewConfigSet, itemPickupPointToSave);
        updatedItemPickupPointList.add(itemPickupPointToSave);
        this.itemPickupPointService.updateItemViewConfigForExistingChannel(itemPickupPointToSave, itemViewConfigSet);
        if (!isCombinedEditRequest) {
          ItemPickupPoint savedItemPickupPoint =
              itemPickupPointService.saveItemPickupPoint(itemPickupPointToSave);
          objectConverterService.overrideL4DetailsFromL5(Collections.singletonList(item),
              Collections.singletonList(savedItemPickupPoint));
          cacheEvictItemService.evictFindL5ByItemSku(Constants.STORE_ID, itemPickupPointToSave.getItemSku());
        } else {
          objectConverterService.overrideL4DetailsFromL5(Collections.singletonList(item),
              Collections.singletonList(itemPickupPointToSave));
        }
      }
      if (CollectionUtils.isNotEmpty(updatedItemPickupPointList)) {
        if (isCombinedEditRequest) {
          editProductDetailDTO.setFreeSampleToggledOn(true);
          if (EditChangeType.CONTENT.equals(editProductDetailDTO.getEditChangeType())) {
            editProductDetailDTO.getProduct().setOnline(false);
          }
          CommonUtil.setOfflineItemIdToItemPickupPointMap(updatedItemPickupPointList, editProductDetailDTO);
        } else {
          saveAndPublishService.publishItemPickupPointDataChangeEvent(updatedItemPickupPointList, new ArrayList<>(),
            Collections.EMPTY_MAP);
        }
      }
      item = this.productHelperService.updateItemViewConfigForExistingChannel(item, itemViewConfig);
    }
    return Pair.of(item, updatedItemPickupPointList);
  }


  private void isDiscoverableFlagChanged(Set<ItemViewConfig> checkItemViewConfigSet,
      ItemPickupPoint itemPickupPointToSave) {
    boolean isDiscoverable = CollectionUtils.isNotEmpty(checkItemViewConfigSet) && checkItemViewConfigSet.stream()
        .anyMatch(ItemViewConfig::isDiscoverable);
    boolean isDiscoverableScheduleChanged =
        CollectionUtils.isNotEmpty(checkItemViewConfigSet) && checkItemViewConfigSet.stream().anyMatch(
            itemDiscoverableSchedule -> Objects.nonNull(itemDiscoverableSchedule.getItemDiscoverableSchedules()));
    boolean isBuyableScheduleChanged =
        CollectionUtils.isNotEmpty(checkItemViewConfigSet) && checkItemViewConfigSet.stream()
            .anyMatch(itemBuyableSchedule -> Objects.nonNull(itemBuyableSchedule.getItemBuyableSchedules()));
    Set<String> itemPickupPointDataChangeType = new HashSet<>();
    if (isDiscoverable) {
      itemPickupPointDataChangeType.add(ItemPickupPointChangeEventType.DISCOVERABLE_FLAG_CHANGE.getName());
    }
    if (isDiscoverableScheduleChanged) {
      itemPickupPointDataChangeType.add(ItemPickupPointChangeEventType.DISCOVERABLE_SCHEDULE_CHANGE.getName());
    }
    if (isBuyableScheduleChanged) {
      itemPickupPointDataChangeType.add(ItemPickupPointChangeEventType.BUYABLE_SCHEDULE_CHANGE.getName());
    }
    itemPickupPointToSave.setItemPickupPointDataChangeType(new ArrayList<>(itemPickupPointDataChangeType));
  }

  private List<Item> updateOff2OnChannelForCombinedEdit(Product currProduct, List<Item> itemList,
      boolean isOff2OnChannelActive, EditProductDetailDTO editProductDetailDTO) {
    List<Item> allItemList = new ArrayList<>();
    List<Item> updatedItemList = new ArrayList<>();
    itemList.forEach(item -> {
      if (item.isOff2OnChannelActive() != isOff2OnChannelActive) {
        item.setOff2OnChannelActive(isOff2OnChannelActive);
        //updating L4 with changed in off 2 on flag at L3
        item.getItemChangeEventTypes().add(ItemChangeEventType.OFFLINE_ITEM_FLAG_CHANGE);
        updatedItemList.add(item);
      }
      allItemList.add(item);
    });
    int itemCount = isOff2OnChannelActive ? updatedItemList.size() : 0;
    //History update
    //Product publish
    //item publish with change type OFFLINE_ITEM_FLAG_CHANGE
    //solr update
    //All should be done after saving entities
    currProduct.setOff2OnItemCount(itemCount);
    currProduct.setOff2OnChannelActive(isOff2OnChannelActive);
    CommonUtil.setUpdatedItemSkuAndItemMap(updatedItemList, editProductDetailDTO);
    editProductDetailDTO.setProduct(currProduct);
    editProductDetailDTO.setProductUpdated(true);
    return allItemList;
  }

  @Override
  public ActivateNeedRevisionResponse activateProductAndItemsOnNeedCorrection(String storeId, String username,
      String requestId, NeedCorrectionProductActivationRequest request) throws Exception {
    boolean isPreOrderChanged = false;
    Product product =
        this.productCacheHelperService.findProductByStoreIdAndProductSku(storeId, request.getProductSku());
    if (Objects.isNull(product)) {
      return objectConverterService.convertActivateNeedRevisionResponseList(true, null, null);
    }
    if (makeL5OfflineForArchivedProducts) {
      updateArchivedProducts(storeId, product, request);
    }
    if (skipMfdUpdateForSuspendedProduct && !product.isSuspended()) {
      product.setMarkForDelete(false);
    }
    product.setTakenDown(false);
    product.setForceReview(false);
    product.setProductType(request.getProductType());
    product.setFbbActivated(request.isFbbActivated());
    product.setOnline(request.isOnline());
    product.setB2bActivated(request.isB2bActivated());
    product.setB2cActivated(request.isB2cActivated());
    product.setBundleProduct(request.isBundleProduct());
    product.setSizeChartCode(request.getSizeChartCode());
    if (Objects.nonNull(request.getPreOrder())) {
      PreOrder preOrder = new PreOrder();
      BeanUtils.copyProperties(request.getPreOrder(), preOrder);
      isPreOrderChanged = isPreOrderChanged(preOrder, product.getPreOrder());
      product.setPreOrder(preOrder);
    }
    ProductDetailResponse productDetailResponse =
        this.masterDataService.getProductDetailFromMasterData(username, requestId, product.getProductCode());
    CommonUtil.updateMissingNonMandatoryFieldsForProductActivation(productDetailResponse,
      product);
    List<Item> listOfItems = new ArrayList<>();
    List<ItemPickupPoint> newlyAddedL5s = new ArrayList<>();
    List<ItemPickupPoint> allUpdatedItemPickupPoints = new ArrayList<>();
    boolean isFreeSampleOrOfflineProduct = request.isFreeSample() || !request.isOnline();
    EditItemResponse editItemResponse =
        itemService.activateItemsOnNeedCorrectionWithMpp(storeId, request.getProductSku(), product.getMerchantCode(), request.getItemRequest(), isFreeSampleOrOfflineProduct);
    listOfItems = editItemResponse.getUpdatedItems();
    CommonUtil.setDistributionStatus(product, editItemResponse.getAddOrDeletePPPerformed(),
        Optional.ofNullable(editItemResponse.getAllUpdatedItemPickupPoints()).orElse(new ArrayList<>()).stream()
            .filter(Predicate.not(ItemPickupPoint::isMarkForDelete)).toList(), distributionSellerList);
    updateDimensionsMissingForProduct(storeId, username, requestId, productDetailResponse, product, listOfItems);
    updateCNCForNeedRevisionActivationRequest(request, listOfItems);
    product.setCncActivated(listOfItems.stream().filter(Predicate.not(Item::isMarkForDelete)).anyMatch(Item::isCncActivated));
    listOfItems.stream().forEach(item -> item.setNewData(true));
    newlyAddedL5s = editItemResponse.getUpdatedItemPickupPoints();
    allUpdatedItemPickupPoints = editItemResponse.getAllUpdatedItemPickupPoints();
    updateLateFulfillmentFlagAtItemLevel(request.getProductType(), listOfItems);
    updateFreeSample(request.isFreeSample(), product, listOfItems);
    updateOff2OnChannelActive(request.isOff2OnChannelActive(), product, listOfItems);
    updateAndSave(product, listOfItems, productDetailResponse, allUpdatedItemPickupPoints,
        Collections.singletonList(ProductChangeEventType.FORCE_REVIEW_FLAG_CHANGE));
    if (isPreOrderChanged) {
      PreOrderVO preOrderVO = new PreOrderVO();
      BeanUtils.copyProperties(product.getPreOrder(), preOrderVO);
      ProductPreOrderStatus productPreOrderStatus =
          ProductPreOrderStatus.builder().productCode(product.getProductCode()).productSku(product.getProductSku())
              .preOrder(preOrderVO).build();
      kafkaPublisher.send(ProductDomainEventName.PRODUCT_PREORDER_STATUS, productPreOrderStatus.getProductSku(),
          productPreOrderStatus);
    }
    saveAndPublishService.publishProductBundleOneToOneMappingEvent(listOfItems);
    return objectConverterService.convertActivateNeedRevisionResponseList(false, listOfItems, newlyAddedL5s);
  }

  private void updateCNCForNeedRevisionActivationRequest(
    NeedCorrectionProductActivationRequest request, List<Item> listOfItems) {
    if (bopisCNCRestrictionEnabled) {
      // After migration products will be made Regular yet dimensions will be Zero
      boolean nonPhysicalProduct =
        ProductType.BOPIS.equals(request.getProductType()) || CommonUtil.isAllDimensionsAreZero(
          listOfItems.stream().findFirst().orElse(new Item()));
      if (nonPhysicalProduct) {
        // Bopis product cannot be CNC Active ,  will update CNC at L5
        request.setCncActivated(false);
      }
    }
  }

  private void updateDimensionsMissingForProduct(String storeId, String username, String requestId,
    ProductDetailResponse result, Product product, List<Item> listOfItems) {
    if (bopisCategoryRestrictionEnabled) {
      boolean bopisNotEligibleForCategory = Boolean.FALSE.equals(
        Optional.of(result).map(ProductDetailResponse::getProductCategoryResponses)
          .orElse(Collections.emptyList()).stream().map(ProductCategoryResponse::getCategory)
          .reduce((first, second) -> second).map(CategoryResponse::isBopisEligible).orElse(null));
      //Validation needed only for valid merchantTypes and CN is not eligible for non BOPIS
      if(ProductType.BOPIS.equals(product.getProductType()) && bopisNotEligibleForCategory) {
        ProfileResponse profileResponse =
          xbpOutbound.getBusinessPartnerDetails(storeId, requestId, username, product.getMerchantCode());
        product.setDimensionsMissing(CommonUtil.fetchDimensionsMissingForBopisProduct(
          listOfItems.stream().findFirst().orElse(new Item()), profileResponse,
          bopisCategoryValidationMerchantTypes, true));
      }
    }
  }

  private void updateArchivedProducts(String storeId, Product product, NeedCorrectionProductActivationRequest request) {
    BusinessPartner businessPartner =
        businessPartnerService.getBusinessPartnerByBusinessPartnerCode(storeId, product.getMerchantCode());
    String merchantStatus = businessPartner.getMerchantStatus();
    if ((!(Constants.ACTIVE).equals(merchantStatus) || product.isArchived()) && Objects.nonNull(
        request.getItemRequest())) {
      List<NeedCorrectionItemActivationRequest> itemRequests = request.getItemRequest();
      for (NeedCorrectionItemActivationRequest itemRequest : itemRequests) {
        itemRequest.setBuyable(false);
        itemRequest.setDiscoverable(false);
        if (Objects.nonNull(itemRequest.getB2bFields())) {
          Set<ItemViewConfig> itemViewConfigs =
              Optional.ofNullable(itemRequest.getB2bFields().getB2bItemViewConfigs()).orElse(new HashSet<>());
          itemViewConfigs.forEach(itemViewConfig -> {
            itemViewConfig.setBuyable(false);
            itemViewConfig.setDiscoverable(false);
          });
        }
      }
    }
  }

  @Override
  public ProductTypeResponse getProductTypeByProductCode(String storeId, String requestId, String username,
      String productCode) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(requestId), ProductServiceImpl.REQUEST_ID_MUST_NOT_BE_BLANK);
    checkArgument(StringUtils.isNotBlank(productCode), ProductServiceImpl.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    List<Product> products = this.productRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    if (CollectionUtils.isNotEmpty(products)) {
      if (products.size() == 1) {
        return new ProductTypeResponse(products.get(0).getProductType());
      } else {
        throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE, ErrorMessages.PRODUCT_TYPE_ERROR_MESSAGE);
      }
    }
    return null;
  }

  @Override
  public void updateProductAndItemDetails(ProductDomainEventModel productDomainEventModel, boolean isMigration) {
    List<Product> productList = this.findByStoreIdAndProductCode(productDomainEventModel.getStoreId(),
        productDomainEventModel.getProductCode());
    productList.forEach(product -> updateProductDetails(product, productDomainEventModel, isMigration));
  }

  private void updateProductDetails(Product product, ProductDomainEventModel productDomainEventModel,
      boolean isMigration) {
    try {
      List<ItemPickupPoint> itemPickupPoints = new ArrayList<>();
      List<Item> updatedItems = new ArrayList<>();
      if (Constants.UPC_MIGRATION.equalsIgnoreCase(
        Optional.ofNullable(productDomainEventModel.getMigrationType()).orElse(StringUtils.EMPTY))) {
        Set<String> itemCodes = productDomainEventModel.getProductItems().stream()
          .map(ProductItemDomainEventModel::getSkuCode).collect(Collectors.toSet());
        List<Item> itemList = itemService.findItemsByStoreIdAndItemCodeInAndMarkForDeleteFalse(
          productDomainEventModel.getStoreId(), itemCodes);
        Map<String, String> itemUpcCodeMap = productDomainEventModel.getProductItems().stream()
          .collect(Collectors.toMap(ProductItemDomainEventModel::getSkuCode,
            ProductItemDomainEventModel::getUpcCode));
        for (Item item : itemList) {
          item.setUpcCode(itemUpcCodeMap.getOrDefault(item.getItemCode(), ""));
          updatedItems.add(item);
        }
        List<Item> savedItemList = new ArrayList<>(itemRepository.saveAll(itemList));
        savedItemList.forEach(item -> {
          kafkaPublisher.send(ProductDomainEventName.ITEM_CACHE_CLEAR_EVENT,
            ItemCacheClearModel.builder().itemSku(item.getItemSku())
              .productSku(item.getProductSku()).storeId(item.getStoreId()).build());
        });
      }
      else {
        product.setProductName(productDomainEventModel.getName());
        product.setBrand(productDomainEventModel.getBrand());
        itemPickupPoints =
          itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(product.getStoreId(),
            product.getProductSku());
        product.setPickupPointCodes(CommonUtil.getDistinctPickupPointCodes(itemPickupPoints));
        product.setCncActivated(itemPickupPoints.stream().anyMatch(ItemPickupPoint::isCncActive));
        product.setCategoryCode(
          productDomainEventModel.getProductCategories().stream().filter(Objects::nonNull).filter(
              productCategoryDomainEventModel -> Objects.nonNull(productCategoryDomainEventModel.getCategory()))
            .map(ProductCategoryDomainEventModel::getCategory).map(CategoryDomainEventModel::getCategoryCode).findFirst().orElse(null));
        List<Item> listOfItems =
          itemService.findItemsByStoreIdAndProductSku(productDomainEventModel.getStoreId(), product.getProductSku());
        if (CollectionUtils.isNotEmpty(productDomainEventModel.getProductItems())) {
          Map<String, Item> itemMap =
            Optional.ofNullable(listOfItems).orElse(new ArrayList<>()).stream().filter(Objects::nonNull).collect(Collectors.toMap(Item::getItemCode, Function.identity()));
          for (ProductItemDomainEventModel productItemDomainEventModel : productDomainEventModel.getProductItems()) {
            Item item = Optional.ofNullable(itemMap).orElse(new HashMap<>()).get(productItemDomainEventModel.getSkuCode());
            if (Objects.nonNull(item)) {
              item.setGeneratedItemName(productItemDomainEventModel.getGeneratedItemName());
              item.setCategoryCode(productDomainEventModel.getProductCategories().get(0).getCategory().getCategoryCode());
              item.setLength(productDomainEventModel.getLength());
              item.setWeight(productDomainEventModel.getWeight());
              item.setShippingWeight(productDomainEventModel.getShippingWeight());
              item.setWidth(productDomainEventModel.getWidth());
              item.setHeight(productDomainEventModel.getHeight());
              item.setDangerousLevel(productItemDomainEventModel.getDangerousGoodsLevel());
              item.setMainImageUrl(
                  productItemDomainEventModel.getImages().stream().filter(ImageDomainEventModel::isMainImage).map(ImageDomainEventModel::getLocationPath).findFirst().orElse(StringUtils.EMPTY));
              item.setDefiningAttributes(productDomainEventModel.getProductAttributes().stream().filter(
                  productAttributeDomainEventModel -> CommonUtil.isDefiningOrVariantCreating(
                      productAttributeDomainEventModel)).map(productAttributeDomainEventModel -> objectConverterService.toProductAttributeDetail(
                  productAttributeDomainEventModel)).collect(Collectors.toList()));
              item.setBrand(productDomainEventModel.getBrand());
              updatedItems.add(item);
            }
          }
        }
        log.info("Saving the updated product details from master data product : {} and items : {}",
          product, updatedItems);
        saveOperationService.saveProductAndItemsWithoutPublishingEventAndWithoutReindexing(
          new ProductAndItemsVO(product, updatedItems));
        if (isMigration) {
          productAndItemSolrIndexerService.updatePickUpPointAndVariantCountAndCncActivation(
            new ProductAndItemsVO(product, updatedItems), itemPickupPoints);
        }
      }
    } catch (Exception e) {
      log.error("Error while updating master data for the product code : {}", product.getProductCode(), e);
    }
  }

  @Override
  public void setSellerPromoBundlings(String storeId, List<ProductItemsVo> productItemsVoList) {
    if (!sellerPromoSwitchOn(storeId)) {
      return;
    }
    List<String> businessPartnerCodeList = productItemsVoList.stream()
      .map(productItemsVo -> productItemsVo.getProductVo().getMerchantCode())
      .distinct().collect(Collectors.toList());
    Map<String, BusinessPartnerPromo> businessPartnerAndCodeMap =
      getBusinessPartnerPromoMap(storeId, businessPartnerCodeList);
    for (ProductItemsVo productItemsVo : productItemsVoList) {
      BusinessPartnerPromo businessPartnerPromo = businessPartnerAndCodeMap.getOrDefault(
        productItemsVo.getProductVo().getMerchantCode(), null);
      if (Objects.nonNull(businessPartnerPromo)) {
        productItemsVo.getItemVoList().forEach(itemVo -> itemVo.setSellerActivePromoBundlings(
          businessPartnerPromo.getActivePromoBundlings()));
      }
    }
  }

  private Map<String, BusinessPartnerPromo> getBusinessPartnerPromoMap(String storeId, List<String> businessPartnerCodeList) {
    return Optional.ofNullable(
        this.businessPartnerPromoService.findByStoreIdAndBusinessPartnerList(storeId,
          businessPartnerCodeList)).orElse(new ArrayList<>()).stream()
      .collect(Collectors.toMap(BusinessPartnerPromo::getBusinessPartnerCode, Function.identity()));
  }

  private boolean sellerPromoSwitchOn(String storeId) {
    return Boolean.parseBoolean(this.systemParameterService.findValueByStoreIdAndVariable(storeId,
      SystemParameterNames.SELLER_PROMO_BUNDLINGS_SWITCH).getValue());
  }

  private void updateAndSave(Product product, List<Item> listOfItems, ProductDetailResponse productDetailResponse,
      List<ItemPickupPoint> allUpdatedItemPickupPoints, List<String> productChangeTypes)
      throws Exception {
    checkArgument(productDetailResponse != null,
        ProductServiceImpl.PRODUCT_CODE_NOT_FOUND_ON_MASTER_DATA_WITH_PRODUCT_CODE + product.getProductCode());
    List<ProductSpecialAttribute> productSpecialAttributes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      for (ProductAttributeResponse attribute : productDetailResponse.getProductAttributeResponses()) {
        AttributeResponse attributeDetail = attribute.getAttribute();
        if (attributeDetail.isSkuValue()) {
          ProductSpecialAttribute productSpecialAttribute = new ProductSpecialAttribute();
          productSpecialAttribute.setAttributeCode(attributeDetail.getAttributeCode());
          productSpecialAttribute.setAttributeName(attributeDetail.getName());
          String value = "-";
          if (CollectionUtils.isNotEmpty(attribute.getProductAttributeValues())) {
            for (ProductAttributeValueResponse productAttributeValueResponse : attribute.getProductAttributeValues()) {
              if (Objects.nonNull(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
                if (com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE
                    .equals(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
                  value = productAttributeValueResponse.getDescriptiveAttributeValue();
                } else if (com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.PREDEFINED
                    .equals(productAttributeValueResponse.getDescriptiveAttributeValueType()) && Objects
                    .nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
                  value = productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue();
                } else if (com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.NONE
                    .equals(productAttributeValueResponse.getDescriptiveAttributeValueType())) {
                  value = productAttributeValueResponse.getAllowedAttributeValue().getValue();
                }
              }
            }
          }
          productSpecialAttribute.setAttributeValue(value);
          productSpecialAttributes.add(productSpecialAttribute);
        }
      }
      product.setProductSpecialAttributes(productSpecialAttributes);
      if (CollectionUtils.isNotEmpty(productDetailResponse.getProductItemResponses())) {
        Map<String, Item> itemActivationRequestMap =
            new HashMap<>(listOfItems.stream().collect(Collectors.toMap(Item::getItemCode, Function.identity())));
        for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
          Item item = itemActivationRequestMap.get(productItemResponse.getSkuCode());
          if (Objects.nonNull(item)) {
            item.setContentChanged(productItemResponse.isContentChanged());
          }
        }
      }
      List<ItemPickupPoint> itemPickupPointList = this.itemPickupPointService
          .getItemPickupPointsByProductSkuAndMarkForDeleteFalse(Constants.DEFAULT_STORE_ID, product.getProductSku());
      product.setPickupPointCodes(CommonUtil.getDistinctPickupPointCodes(itemPickupPointList));
      product.setFbbActivated(
          allUpdatedItemPickupPoints.stream().filter(Predicate.not(ItemPickupPoint::isMarkForDelete))
              .anyMatch(ItemPickupPoint::isFbbActivated));
      this.saveOperationService.saveProductAndItems(new ProductAndItemsVO(product, listOfItems), productChangeTypes);
    }
  }

  @Override
  public void updateDistinctPickupPointCodesAndL5Count(String storeId, String username, String productSku,
      Set<ItemPickupPoint> itemPickupPointList, int l5Count) {
    boolean isFbbActivated =
      itemPickupPointList.stream().anyMatch(ItemPickupPoint::isFbbActivated);
    Set<String> pickupPointCodes = CommonUtil.getDistinctPickupPointCodes(new ArrayList<>(itemPickupPointList));
    Product product = productRepository.updatePickupPointCodes(storeId, username, productSku, pickupPointCodes,
      isFbbActivated);
    cacheEvictHelperService.evictProductData(storeId, product);
    if (eventBasedSolrUpdateEnable) {
      productAndItemSolrIndexerService.updateDistinctPickupPointCodesAndL5Count(productSku,
        pickupPointCodes, l5Count, isFbbActivated, product.getMerchantCode());
    } else {
      productAndItemSolrIndexerService.updateProductDetailsInSolr(Collections.singletonList(product));
    }

    Map<String, List<ItemPickupPoint>> itemPickupPointItemSkuMap =
        itemPickupPointList.stream().collect(Collectors.groupingBy(ItemPickupPoint::getItemSku));
    for (Map.Entry<String, List<ItemPickupPoint>> itemSkuAndItemPickupPoints : itemPickupPointItemSkuMap.entrySet()) {
      List<ItemPickupPoint> cncActiveItemPickupPoints =
          itemSkuAndItemPickupPoints.getValue().stream().filter(ItemPickupPoint::isCncActive)
              .collect(Collectors.toList());
      List<OfflineItem> offlineItems = CommonUtil.getOfflineItemsByItemPickupPoint(cncActiveItemPickupPoints, false,
          new OfflineItemHistoryDetailVO());
      productAndItemSolrIndexerService.offlineItemPriceAtomicUpdate(itemSkuAndItemPickupPoints.getKey(), offlineItems);
    }
  }

  private void updateFreeSample(boolean freeSample, Product product, List<Item> itemList) {
    product.setFreeSample(freeSample);
    itemList.forEach(item -> item.setFreeSample(freeSample));
  }

  private void updateLateFulfillmentFlagAtItemLevel(ProductType productType, List<Item> itemList) {
    itemList.forEach(item -> item.setLateFulfillment(!ProductType.REGULAR.equals(productType)));
  }

  private void updateOff2OnChannelActive(boolean off2OnChannelActive, Product product, List<Item> itemList) {
    product.setOff2OnChannelActive(off2OnChannelActive);
    itemList.forEach(item -> item.setOff2OnChannelActive(off2OnChannelActive));
  }

  @Override
  public List<Product> getProductsByStoreIdAndMerchantCodeAndIsSuspended(String storeId,
      String merchantCode, boolean suspended) {
    return productRepository.findProductsByStoreIdAndMerchantCodeAndIsSuspended(storeId, merchantCode, suspended);
  }

  @Override
  public List<Product> findByStoreIdAndProductSkuInAndMarkForDeleteFalse(String storeId, List<String> skus) {
    return CollectionUtils.isNotEmpty(skus) ?
        this.productRepository.findProductByStoreIdAndProductSkuInAndMarkForDeleteFalse(storeId, new HashSet<>(skus)) :
        Collections.emptyList();
  }

  @Override
  public List<ProductBasicResponse> findProductBasicDetailsByItemSkus(String storeId,
      SimpleListStringRequest productSkuList) {
    checkArgument(Objects.nonNull(productSkuList), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(productSkuList.getValue()), ErrorMessages.ITEM_LIST_NOT_EMPTY);
    List<Item> itemsByStoreIdAndItemSkus =
        itemService.getItemsByStoreIdAndItemSkus(storeId, new HashSet<>(productSkuList.getValue()));
    if(CollectionUtils.isNotEmpty(itemsByStoreIdAndItemSkus)) {
      return findProductBasicDetailsByProductSku(storeId, new SimpleListStringRequest(
          itemsByStoreIdAndItemSkus.stream().map(Item::getProductSku).distinct().collect(Collectors.toList())), false);
    }
    return new ArrayList<>();
  }

  @Override
  public List<ProductBasicResponse> findProductBasicDetailsByProductSku(String storeId,
      SimpleListStringRequest productSkuList, boolean needSalesCategoryData) {
    checkArgument(Objects.nonNull(productSkuList), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(CollectionUtils.isNotEmpty(productSkuList.getValue()), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    List<ProductBasicResponse> productBasicResponseList = new ArrayList<>();
    Set<String> salesCategorySet = new HashSet<>();
    for (String productSku : productSkuList.getValue()) {
      ProductBasicResponse productBasicResponse = new ProductBasicResponse();
      productBasicResponse.setProductSku(productSku);
      Product product = productCacheHelperService.findProductByStoreIdAndProductSku(storeId, productSku);
      if (Objects.nonNull(product)) {
        productBasicResponse = ProductBasicResponse.builder().productSku(product.getProductSku())
            .tradingProduct(product.isTradingProduct()).archived(product.isArchived()).suspended(product.isSuspended())
            .productExists(true).bundleProduct(product.isBundleProduct()).markForDelete(product.isMarkForDelete())
            .categoryCode(product.getCategoryCode()).productType(product.getProductType())
            .productName(product.getProductName()).build();
        if (Objects.nonNull(product.getPreOrder())) {
          PreOrderDTO preOrderDTO = new PreOrderDTO();
          BeanUtils.copyProperties(product.getPreOrder(),preOrderDTO);
          productBasicResponse.setPreOrder(preOrderDTO);
        }
      }
      if (needSalesCategoryData) {
        List<String> salesCategoryList = this.objectConverterService.getSalesCategoryCodesFromSalesCatalogs(product.getSalesCatalogs());
        productBasicResponse.setSalesCategoryCodes(salesCategoryList);
        salesCategorySet.addAll(salesCategoryList);
      }
      productBasicResponseList.add(productBasicResponse);
    }
    if (CollectionUtils.isNotEmpty(salesCategorySet)) {
      Map<String, List<CategoryResponse>> parentCategoriesFromDbAndCache =
          cachedService.getParentCategoriesFromDbAndCache(Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
              salesCategorySet);
      objectConverterService.toSalesCategoryHierarchy(productBasicResponseList, parentCategoriesFromDbAndCache);
    }
    return productBasicResponseList;
  }

  @Override
  public void makeProductBundleFromBundleRecipe(String storeId, String itemSku,
      ProductBundleCreationRequest productBundleCreationRequest) throws Exception {
    Item parentItem = cacheItemHelperService.findCacheableByStoreIdAndItemSku(storeId, itemSku);
    GdnPreconditions.checkArgument(Objects.nonNull(parentItem), ErrorMessages.ITEM_NOT_FOUND);

    Set<String> itemSkusToFetch =
        productBundleCreationRequest.getBundleRecipeList().stream().map(BundleRecipeVo::getItemSku)
            .collect(Collectors.toSet());
    List<Item> items = itemRepository.findItemsByStoreIdAndItemSkuInAndMarkForDeleteFalse(storeId, itemSkusToFetch);
    Map<String, String> itemSkuAndItemCodeMap =
        items.stream().collect(Collectors.toMap(Item::getItemSku, Item::getItemCode, (v1, v2) -> v2));
    boolean updateInWms =
        validateBundleRecipeForSharedProducts(storeId, productBundleCreationRequest, parentItem, itemSkuAndItemCodeMap);

    Set<BundleRecipe> bundleRecipes = CommonUtil.toBundleRecipes(productBundleCreationRequest);
    CreateUpdateBillOfMaterialRecipeCommandRequest createUpdateBillOfMaterialRecipeRequest =
        CommonUtil.toCreateUpdateBillOfMaterialRecipeRequest(parentItem, productBundleCreationRequest,
            itemSkuAndItemCodeMap);

    if (warehouseBomActivated && updateInWms) {
      warehouseItemMasterOutbound.createAndUpdateProductBundle(createUpdateBillOfMaterialRecipeRequest);
    }

    Product product = updateBundleFlagAtProductLevel(storeId, parentItem);
    updateBundleRecipeAtItemLevel(parentItem, bundleRecipes);
    publishBundleMappingAndSolrReindexEvent(parentItem, product);
  }

  private boolean validateBundleRecipeForSharedProducts(String storeId,
      ProductBundleCreationRequest productBundleCreationRequest, Item parentItem,
      Map<String, String> itemSkuAndItemCodeMap) {
    if (sharedProductBundleRecipeEditEnabled) {
      List<SharedProductBundleRecipeResponse> sharedProductBundleRecipeResponses =
          itemService.getBundleRecipeForSharedItems(storeId, Set.of(parentItem.getItemCode()));
      Optional<Set<SkuCodeBundleRecipeResponse>> skuCodeBundleRecipeResponse =
          sharedProductBundleRecipeResponses.stream().map(sharedProductBundleRecipeResponse -> Optional.ofNullable(
              sharedProductBundleRecipeResponse.getBundleRecipe()).orElse(new HashSet<>())).findFirst();
      if (skuCodeBundleRecipeResponse.isPresent() && CollectionUtils.isNotEmpty(skuCodeBundleRecipeResponse.get())) {
        Set<SkuCodeBundleRecipeResponse> newBundleRecipe = productBundleCreationRequest.getBundleRecipeList().stream()
            .map(bundleRecipeVo -> new SkuCodeBundleRecipeResponse(
                itemSkuAndItemCodeMap.get(bundleRecipeVo.getItemSku()), bundleRecipeVo.getQuantity()))
            .collect(Collectors.toSet());
        return !newBundleRecipe.equals(skuCodeBundleRecipeResponse.get());
      }
    }
    return true;
  }

  private void publishBundleMappingAndSolrReindexEvent(Item parentItem, Product product) {
    itemService.publishUpdateToSolrEvent(product, Collections.singletonList(parentItem));
    saveAndPublishService.publishProductBundleOneToOneMappingEvent(Collections.singleton(parentItem));
  }

  private void updateBundleRecipeAtItemLevel(Item parentItem, Set<BundleRecipe> bundleRecipeList) throws Exception {
    parentItem.setBundleRecipe(bundleRecipeList);
    saveAndPublishService.saveItems(Collections.singletonList(parentItem));
    cacheEvictHelperService.evictItemCache(parentItem.getStoreId(), parentItem);
    itemService.updateRecipeForSharedProducts(parentItem.getStoreId(), List.of(parentItem));
  }

  private Product updateBundleFlagAtProductLevel(String storeId, Item parentItem) {
    Product product = productRepository.findByStoreIdAndProductSku(storeId, parentItem.getProductSku());
    if (Objects.nonNull(product)) {
      product.setBundleProduct(true);
      saveAndPublishService.saveProducts(Collections.singletonList(product));
      cacheEvictHelperService.evictProductData(product.getStoreId(), product);
    }
    return product;
  }

  @Override
  public List<Product> getAllProducts(String storeId, Set<String> productSkus, boolean readFromPrimary) {
    checkArgument(StringUtils.isNotBlank(storeId), ProductServiceImpl.STORE_ID_MUST_NOT_BE_BLANK);
    checkArgument(productSkus != null && !productSkus.isEmpty(), ProductServiceImpl.PRODUCT_SKU_MUST_NOT_BE_BLANK);
    Optional.ofNullable(productSkus).orElse(new HashSet<>()).forEach(productSku -> {
      checkArgument(this.skuValidator.isProductSku(productSku),
          ProductServiceImpl.INVALID_PRODUCT_SKU_FORMAT + productSku);
    });
    return this.productRepository.findProductByStoreIdAndProductSkuIn(storeId, new ArrayList<>(productSkus), readFromPrimary);
  }

  @Override
  public Product findByProductCodeAndSellerCode(String productCode, String sellercode) {
    return productRepository.findByProductCodeAndMerchantCode(productCode, sellercode);
  }

  @Override
  public List<ItemPickupPointCodeResponse> reconcileProductVariants(String storeId, String requestId, String username,
      AddDeleteVariantRetryRequest addDeleteVariantRetryRequest) throws Exception {

    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(addDeleteVariantRetryRequest.getProductCode()),
        ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(addDeleteVariantRetryRequest.getProductSku()),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);

    // step 1 - get the data from pcb with inAllProducts = true to get deleted product info as well.
    ProductDetailResponse productDetailResponse =
        getProductMasterDataFromPcb(requestId, username, addDeleteVariantRetryRequest.getProductCode());

    //step 2 - get the l3, l4 and l5 data using storeId and productSku
    ProductCollectionsVo productCollectionsVo =
        getProductAndItemsAndItemPickupPoints(storeId, addDeleteVariantRetryRequest.getProductSku());

    //step 3 - delete the l4 and l4 for which item don't exists in  PCB or its MFD true and also delete the l5s from x-inventory
    deleteExtraItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo);

    //step 4 - create missing l4s and l5s.
    List<ItemPickupPoint> newlyAddedItemPickupPoints =
        AddDeleteVariantReconcileUtil.createMissingItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo,
            addDeleteVariantRetryRequest);

    //step 5 - update defining attribute at l3 level
    updateProductDefiningAttributes(productDetailResponse, productCollectionsVo);

    //step 6 - update flag at l3 and l4 and l5 level
    updateCncInItemPickupPoint(storeId, productCollectionsVo.getItemPickupPoints());
    AddDeleteVariantReconcileUtil.setCncAndOnlineAndB2cAndB2bActivatedAndPickupPointCodesFlag(productCollectionsVo);

    //step 7 - update master data by calling product score flow
    setMasterDataDetailsInProductAndItemsOnEdit(storeId, productCollectionsVo.getProducts().stream().findFirst().get(),
        productCollectionsVo.getItems(), false, productDetailResponse);

    //step 8 - update in db, clear cache, reindex and publish downstream events
    updateAndEvictCacheProductAndItemAndItemPickupPoint(storeId, productCollectionsVo);

    return com.gdn.x.product.service.util.ResponseHelper.toItemPickupPointCodeResponses(
        newlyAddedItemPickupPoints, productCollectionsVo);
  }

  private ProductDetailResponse getProductMasterDataFromPcb(String requestId, String username, String productCode)
      throws Exception {
    MasterDataCacheVo masterDataCacheVo =
        pcbMasterDataService.getProductDetailByProductCodeForAllProductsWithoutCache(requestId, username, productCode,
            true);

    GdnPreconditions.checkArgument(
        Optional.ofNullable(masterDataCacheVo).map(MasterDataCacheVo::getProductResponse).isPresent(),
        ErrorMessages.PRODUCT_NOT_FOUND_ERROR);

    return masterDataCacheVo.getProductResponse();
  }

  private ProductCollectionsVo getProductAndItemsAndItemPickupPoints(String storeId, String productSku) {
    Product product = productRepository.findByStoreIdAndProductSku(storeId, productSku);
    GdnPreconditions.checkArgument(Objects.nonNull(product), ErrorMessages.CANNOT_FIND_PRODUCT_BY_PRODUCT_SKU);

    List<Item> items = itemService.findItemsByStoreIdAndProductSku(storeId, productSku);

    List<ItemPickupPoint> itemPickupPoints = itemPickupPointService.findByStoreIdAndProductSku(storeId, productSku);

    return new ProductCollectionsVo(Collections.singletonList(product), Optional.ofNullable(items).orElse(new ArrayList<>()),
        Optional.ofNullable(itemPickupPoints).orElse(new ArrayList<>()));
  }

  private void deleteExtraItemsAndItemPickupPoints(ProductDetailResponse productDetailResponse,
      ProductCollectionsVo productCollectionsVo) {
    List<ItemPickupPoint> deletedItemPickupPoints =
        AddDeleteVariantReconcileUtil.deleteExtraItemsAndItemPickupPoints(productDetailResponse, productCollectionsVo);
    if (CollectionUtils.isNotEmpty(deletedItemPickupPoints)) {
      itemPickupPointService.deleteItemPickupPointsFromInventory(deletedItemPickupPoints);
    }
  }

  private void updateProductDefiningAttributes(ProductDetailResponse productDetailResponse,
      ProductCollectionsVo productCollectionsVo) {
    Product product = productCollectionsVo.getProducts().stream().findFirst().get();
    Map<String, MasterDataItem> masterDataItems =
        this.objectConverterService.convertToMasterDataItems(productDetailResponse.getProductItemResponses(),
            productDetailResponse.getProductCode());
    product.getDefiningAttributes().clear();
    for (Item item : productCollectionsVo.getItems()) {
      String itemCode = item.getItemCode();
      if (masterDataItems.containsKey(itemCode) && !item.isPermanentDelete()) {
        MasterDataItem masterDataItem = masterDataItems.get(itemCode);
        this.productHelperService.addItemAttributeToProductAttribute(
            product, item.getItemSku(),
            masterDataItem.getMasterDataItemAttributeValues());
      }
    }
  }

  private void updateCncInItemPickupPoint(String storeId, List<ItemPickupPoint> itemPickupPoints) {
    List<String> pickupPointCodes =
        itemPickupPoints.stream().filter(ItemPickupPoint::isCncActive).map(ItemPickupPoint::getPickupPointCode)
            .distinct().collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(pickupPointCodes)) {
      List<BusinessPartnerPickupPoint> businessPartnerPickupPointList =
          businessPartnerPickupPointService.getBusinessPartnerPickupPointByPickupPointCodes(storeId, pickupPointCodes);
      Map<String, Boolean> cncActiveMap = businessPartnerPickupPointList.stream()
          .collect(Collectors.toMap(BusinessPartnerPickupPoint::getCode, BusinessPartnerPickupPoint::isCncActivated));
      itemPickupPoints.stream().filter(ItemPickupPoint::isCncActive).forEach(
          itemPickupPoint -> itemPickupPoint.setCncActive(
              cncActiveMap.getOrDefault(itemPickupPoint.getPickupPointCode(), false)));
    }
  }

  private void updateAndEvictCacheProductAndItemAndItemPickupPoint(String storeId,
      ProductCollectionsVo productCollectionsVo) {
    saveProductAndItemsAndItemPickupPoints(productCollectionsVo);

    clearProductAndItemAndItemPickupPointCache(storeId, productCollectionsVo);

    publishItemAndItemPickupPointDataChangeEventAndSolrReindexEvent(productCollectionsVo);
  }

  private void saveProductAndItemsAndItemPickupPoints(ProductCollectionsVo productCollectionsVo) {
    productRepository.saveAll(productCollectionsVo.getProducts());
    itemService.saveItems(productCollectionsVo.getItems());
    itemPickupPointService.saveItemPickupPoint(productCollectionsVo.getItemPickupPoints());
  }

  private void clearProductAndItemAndItemPickupPointCache(String storeId, ProductCollectionsVo productCollectionsVo) {
    // clear item pickup point cache
    cacheEvictHelperService.evictItemPickupPointCache(storeId, productCollectionsVo.getItems(),
        productCollectionsVo.getItemPickupPoints());

    //clear item cache
    for (Item item : productCollectionsVo.getItems()) {
      cacheEvictHelperService.evictItemData(storeId, item);
    }

    // clear product cache
    for (Product product : productCollectionsVo.getProducts()) {
      cacheEvictHelperService.evictProductData(storeId, product);
    }
  }

  private void publishItemAndItemPickupPointDataChangeEventAndSolrReindexEvent(
      ProductCollectionsVo productCollectionsVo) {
    saveAndPublishService.publishItemDataChangeEvent(productCollectionsVo.getItems());
    saveAndPublishService.publishItemPickupPointDataChangeEvent(productCollectionsVo.getItemPickupPoints(),
        new ArrayList<>(), Collections.EMPTY_MAP);
    productAndItemSolrIndexerService.updateProductAndItemDetailsInSolr(
        productCollectionsVo.getProducts().stream().findFirst().get(), productCollectionsVo.getItems(), false);
  }

  @Override
  public SimpleBooleanResponse isSharedProduct(String storeId, String productCode, boolean findByProductSku,
    String sellerCode) {
    if (findByProductSku) {
      Product product = productRepository.findByStoreIdAndProductSku(storeId, productCode);
      checkArgument(Objects.nonNull(product), PRODUCT_MUST_NOT_BE_NULL);
      productCode = product.getProductCode();
      if (StringUtils.isBlank(productCode)) {
        return new SimpleBooleanResponse(false);
      }
    }
    List<Product> products =
        productRepository.findFirst2ByStoreIdAndProductCode(storeId, productCode);
    return new SimpleBooleanResponse(CommonUtil.isSharedProduct(products, sellerCode));
  }

  @Override
  public Map<String, Boolean> getProductCodeAndSharedProductMap(String storeId, Set<String> productCodes) {
    List<Product> products = productRepository.findByStoreIdAndProductCodeIn(storeId, productCodes);
    Map<String, Boolean> productCodeAndSharedProductMap = new HashMap<>();
    Map<String, List<Product>> productCodeAndProductsMap =
        products.stream().collect(Collectors.groupingBy(Product::getProductCode));
    for (Map.Entry<String, List<Product>> entry : productCodeAndProductsMap.entrySet()) {
      String productCode = entry.getKey();
      List<Product> productList = entry.getValue();
      productCodeAndSharedProductMap.put(productCode, productList.size() > Constants.ONE);
    }
    return productCodeAndSharedProductMap;
  }

  @Override
  public Page<ProductSkuSizeChartResponse> getActiveProductsByStoreIdAndSizeChartCode(String sizeChartCode, int page,
      int size) {
    checkArgument(StringUtils.isNotBlank(sizeChartCode), ErrorMessages.SIZE_CHART_CODE_MUST_NOT_BE_EMPTY);
    return productSolrRepository.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
  }

  @Override
  public void migrateProductAndL5DetailByProductSku(String storeId, ProductAndL5MigrationRequest request)
    throws JsonProcessingException {
    checkArgument(Objects.nonNull(request), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    checkArgument(Objects.nonNull(request.getProductSku()), PRODUCT_SKU_MUST_NOT_BE_BLANK);
    Map<String, Set<String>> L5XUpdatedFeildMap = new HashMap<>();
    Product product = productCacheHelperService.findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(storeId,
        request.getProductSku(), request.isIncludeMfd());
    checkState(Objects.nonNull(product), ProductServiceImpl.PRODUCT_NOT_FOUND);
    List<Item> items = getItemsForLatefulfillmentUpdate(storeId, request);
    List<ItemPickupPoint> itemPickupPointList = getItemPickupPointsForStatusUpdate(storeId, request);
    boolean isDimensionsMissingChanged = CommonUtil.checkAndSetDimensionsMissing(product, request);
    boolean isProductTypeChanged = CommonUtil.checkAndSetProductType(product, request);
    boolean isLateFulfillmentChanged = CommonUtil.checkAndSetLateFulfillment(items, request);
    boolean isCncAtL3Changed = CommonUtil.checkAndSetCncActivatedAtL3(product, request);
    if (CommonUtil.isProductUpdated(isDimensionsMissingChanged, isProductTypeChanged, isCncAtL3Changed)) {
      saveOperationService.saveProduct(product);
    }
    if (isLateFulfillmentChanged) {
      saveOperationService.saveItemsWithoutUpdatingSolr(items);
    }
    if (request.isL5Updated()) {
      List<ItemPickupPoint> updatedItemPickupPoints =
          getUpdatedItemPickupPointsForStatusAndCncUpdate(request, itemPickupPointList, L5XUpdatedFeildMap);
      if (CollectionUtils.isNotEmpty(updatedItemPickupPoints)) {
        itemPickupPointService.saveItemPickupPoint(updatedItemPickupPoints);
        saveAndPublishService.publishItemPickupPointDataChangeEvent(updatedItemPickupPoints, new ArrayList<>(),
          Collections.EMPTY_MAP);
        updatedItemPickupPoints.forEach(
            itemPickupPoint -> saveAndPublishService.publishViewConfigChange(itemPickupPoint));
        publishL5HistoryForMigratedData(L5XUpdatedFeildMap, itemPickupPointList);
      }
    }
  }

  @Override
  public PromoEligibilityResponse isPromoItemAvailable(String storeId,
      Map<String, Set<String>> businessPartnerProductSkuList) {
    PromoEligibilityResponse promoEligibilityResponse = new PromoEligibilityResponse();
    try {
      checkArgument(MapUtils.isNotEmpty(businessPartnerProductSkuList),
          ProductServiceImpl.BUSINESS_PARTNER_PRODUCT_SKU_MAP_MUST_NOT_BE_EMPTY);
      Map<String, Boolean> promoAvailabilityMap = new HashMap<>();
      for (Map.Entry<String, Set<String>> entry : businessPartnerProductSkuList.entrySet()) {
        String merchantCode = entry.getKey();
        Set<String> productSkuList = entry.getValue();
        List<ProductSolr> productSolrList =
            productSolrRepository.findByProductSkuListForMFDTrueAndFalse(storeId, merchantCode, productSkuList);
        promoAvailabilityMap.putAll(productSolrList.stream().filter(Objects::nonNull).collect(
            Collectors.toMap(ProductSolr::getProductSku,
                productSolr -> CollectionUtils.isNotEmpty(productSolr.getPromoItemSkus()))));
      }
      promoEligibilityResponse.setPromoEligibility(promoAvailabilityMap);
    } catch (Exception e) {
      log.error("Error while fetching products from solr for product list : {} ",
          businessPartnerProductSkuList, e);
    }
    return promoEligibilityResponse;
  }

  @Override
  public void updateFinalVideoData(
    CompressedVideoUpdateEventModel compressedVideoUpdateEventModel) {
    if (clientIdForVideoCompression.equalsIgnoreCase(
      compressedVideoUpdateEventModel.getClientId())) {
      String productSku =
        compressedVideoUpdateEventModel.getAdditionalFields().get(Constants.PRODUCT_SKU);
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(productSku),
        ErrorMessages.PRODUCT_SKU_EMPTY);
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(compressedVideoUpdateEventModel.getVideoId()),
        ErrorMessages.VIDEO_ID_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(
        StringUtils.isNotBlank(compressedVideoUpdateEventModel.getFinalUrl()),
        ErrorMessages.FINAL_URL_MUST_NOT_BE_BLANK);
      Product product =
        productRepository.findByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID, productSku);
      GdnPreconditions.checkArgument(Objects.nonNull(product),
        ErrorMessages.PRODUCT_NOT_FOUND_ERROR + productSku);
      updateVideoData(compressedVideoUpdateEventModel, productSku, product);
      saveAndPublishEvents(product);
    }
  }

  @Override
  public Page<ReelProductDetailResponse> getProductDetailsByReelProductListingRequest(String storeId,
      ReelProductListingRequest reelProductListingRequest, PageRequest pageRequest) {
    ReelProductListingRequestVo reelProductListingRequestVo =
        CommonUtil.toReelProductListingRequestVo(reelProductListingRequest);
    Page<ProductSolr> productSolrs = Optional.ofNullable(
        productSolrRepository.getL3ProductsForReelsByReelProductListingRequest(storeId,
            reelProductListingRequestVo, pageRequest)).orElse(Page.empty(pageRequest));
    return new PageImpl<>(CommonUtil.toReelProductDetailResponses(productSolrs.getContent()),
        pageRequest, productSolrs.getTotalElements());
  }

  private void updateVideoData(CompressedVideoUpdateEventModel compressedVideoUpdateEventModel,
    String productSku, Product product) {
    Video existingVideo = product.getVideo();
    if (Objects.nonNull(existingVideo)) {
      existingVideo.setFinalUrl(compressedVideoUpdateEventModel.getFinalUrl());
    } else {
      existingVideo = new Video();
      log.info("Video Data missing for product : {} , creating new video object", productSku);
      existingVideo.setFinalUrl(compressedVideoUpdateEventModel.getFinalUrl());
      existingVideo.setVideoId(compressedVideoUpdateEventModel.getVideoId());
      existingVideo.setCoverImagePath(Optional.of(compressedVideoUpdateEventModel)
        .map(CompressedVideoUpdateEventModel::getCoverImagePath).orElse(StringUtils.EMPTY));
      existingVideo.setVideoName(Optional.of(compressedVideoUpdateEventModel)
        .map(CompressedVideoUpdateEventModel::getVideoName).orElse(StringUtils.EMPTY));
      existingVideo.setSourceUrl(Optional.of(compressedVideoUpdateEventModel)
        .map(CompressedVideoUpdateEventModel::getSourceUrl).orElse(StringUtils.EMPTY));
    }
    product.setVideo(existingVideo);
  }

  private void saveAndPublishEvents(Product product) {
    productRepository.save(product);
    saveAndPublishService.publishProduct(product, new ArrayList<>());
    cacheEvictHelperService.evictProductData(Constants.DEFAULT_STORE_ID, product);
    List<Item> items =
      cacheItemHelperService.findCacheableByStoreIdAndProductSku(Constants.DEFAULT_STORE_ID,
        product.getProductSku());
    items.forEach(
      item -> item.setItemChangeEventTypes(List.of(ItemChangeEventType.ITEM_DATA_CHANGE)));
    saveAndPublishService.publishItemDataChangeEvent(items, StringUtils.EMPTY, false);
  }

  private void publishL5HistoryForMigratedData(Map<String, Set<String>> l5XUpdatedFieldMap,
    List<ItemPickupPoint> updatedItemPickupPoints) throws JsonProcessingException {
    List<AuditTrailDto> auditTrailResponseList = new ArrayList<>();
    for (ItemPickupPoint l5 : updatedItemPickupPoints) {
        Set<String> fieldMap = l5XUpdatedFieldMap.getOrDefault(l5.getOfflineItemId(), Collections.emptySet());
        ItemViewConfig defaultViewConfig = getDefaultViewConfig(l5);

        for (String fieldChange : fieldMap) {
          handleFieldChange(fieldChange, l5, defaultViewConfig, auditTrailResponseList);
      }
    }
    sendAuditTrailResponse(auditTrailResponseList);
  }

  public void handleFieldChange(String fieldChange, ItemPickupPoint l5,
    ItemViewConfig defaultViewConfig, List<AuditTrailDto> auditTrailResponseList)
    throws JsonProcessingException {
    ItemPickupPointChangeEventType changeEventType =
      ItemPickupPointChangeEventType.findEnumByValue(fieldChange);
    if(Objects.isNull(changeEventType)){
      return;
    }
    switch (changeEventType) {
        case DISCOVERABLE_FLAG_CHANGE:
          addDiscoverableAndBuyableFlagChanges(auditTrailResponseList, l5, defaultViewConfig);
          break;
        case DISCOVERABLE_SCHEDULE_CHANGE:
          addDiscoverableScheduleChange(auditTrailResponseList, l5, defaultViewConfig);
          break;
        case BUYABLE_SCHEDULE_CHANGE:
          addBuyableScheduleChange(auditTrailResponseList, l5, defaultViewConfig);
          break;
      default:
        log.info("No Valid change field for Migration History for L5 {} ", l5.getOfflineItemId());
    }
  }

  private ItemViewConfig getDefaultViewConfig(ItemPickupPoint l5) {
    return l5.getItemViewConfig().stream().findFirst().orElse(new ItemViewConfig());
  }

  private void addDiscoverableAndBuyableFlagChanges(List<AuditTrailDto> auditTrailResponseList,
    ItemPickupPoint l5, ItemViewConfig defaultViewConfig) {
    auditTrailResponseList.add(createAuditTrailDto(l5, Constants.BUYABLE_FALG_UPDATE,
      Boolean.toString(!defaultViewConfig.isBuyable()),
      Boolean.toString(defaultViewConfig.isBuyable())));
    auditTrailResponseList.add(createAuditTrailDto(l5, Constants.DISPLAYABLE_FLAG_UPDATE,
      Boolean.toString(!defaultViewConfig.isDiscoverable()),
      Boolean.toString(defaultViewConfig.isDiscoverable())));
  }

  private void addDiscoverableScheduleChange(List<AuditTrailDto> auditTrailResponseList,
    ItemPickupPoint l5, ItemViewConfig defaultViewConfig) throws JsonProcessingException {
    auditTrailResponseList.add(new AuditTrailDto(l5.getMerchantCode(), l5.getItemSku(),
      Constants.DISCOVERABLE_SCHEDULE_ACTION,
      objectMapper.writeValueAsString(defaultViewConfig.getItemDiscoverableSchedules()),
      Constants.HYPHEN, null, l5.getProductSku(), null, l5.getPickupPointCode(), false));
  }

  private void addBuyableScheduleChange(List<AuditTrailDto> auditTrailResponseList,
    ItemPickupPoint l5, ItemViewConfig defaultViewConfig) throws JsonProcessingException {
    auditTrailResponseList.add(
      new AuditTrailDto(l5.getMerchantCode(), l5.getItemSku(), Constants.BUYABLE_SCHEDULE_ACTION,
        objectMapper.writeValueAsString(defaultViewConfig.getItemBuyableSchedules()),
        Constants.HYPHEN, null, l5.getProductSku(), null, l5.getPickupPointCode(), false));
  }

  private AuditTrailDto createAuditTrailDto(ItemPickupPoint l5, String actionType, String newValue,
    String oldValue) {
    return new AuditTrailDto(l5.getMerchantCode(), l5.getItemSku(), actionType, newValue, oldValue,
      null, l5.getProductSku(), null, l5.getPickupPointCode(), false);
  }

  private void sendAuditTrailResponse(List<AuditTrailDto> auditTrailResponseList) {
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(mandatoryParameterHelper.getUsername());
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(mandatoryParameterHelper.getRequestId());
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setAuditTrailResponseList(auditTrailResponseList);

    log.info(PUBLISHING_THE_EVENT_TO_UPDATE_THE_AUDIT_LOGS,
      ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }


  private static List<ItemPickupPoint> getUpdatedItemPickupPointsForStatusAndCncUpdate(
    ProductAndL5MigrationRequest request, List<ItemPickupPoint> itemPickupPointList,
    Map<String, Set<String>> l5XUpdatedFeildMap) {
    List<ItemPickupPoint> updatedItemPickupPoints = new ArrayList<>();
    for (ItemPickupPoint itemPickupPoint : itemPickupPointList) {
      boolean statusUpdated;
      statusUpdated = CommonUtil.updateCncAtL5(request, itemPickupPoint);
      Set<ItemViewConfig> defaultExistingViewConfig = itemPickupPoint.getItemViewConfig();
      Set<ItemViewConfig> b2bExistingViewConfig = itemPickupPoint.getItemViewConfigByChannel(Constants.B2B);
      Set<String> itemPickupPointDataChangeType = new HashSet<>();
      for (ItemViewConfig itemViewConfig : defaultExistingViewConfig) {
        statusUpdated =
            CommonUtil.checkAndUpdateStatusInItemViewConfig(request, statusUpdated, itemPickupPointDataChangeType,
                itemViewConfig);
      }
      for (ItemViewConfig itemViewConfig : b2bExistingViewConfig) {
        statusUpdated =
            CommonUtil.checkAndUpdateStatusInItemViewConfig(request, statusUpdated, itemPickupPointDataChangeType,
                itemViewConfig);
      }
      if (statusUpdated) {
        itemPickupPoint.setItemPickupPointDataChangeType(new ArrayList<>(itemPickupPointDataChangeType));
        updatedItemPickupPoints.add(itemPickupPoint);
        l5XUpdatedFeildMap.put(itemPickupPoint.getOfflineItemId(),
          itemPickupPointDataChangeType);
      }
    }
    return updatedItemPickupPoints;
  }


  private List<ItemPickupPoint> getItemPickupPointsForStatusUpdate(String storeId,
      ProductAndL5MigrationRequest request) {
    List<ItemPickupPoint> itemPickupPointList = new ArrayList<>();
    if (request.isL5Updated()) {
        itemPickupPointList = itemPickupPointService.getItemPickupPointsByProductSkuAndMarkForDeleteFalse(storeId,
            request.getProductSku());
      checkState(CollectionUtils.isNotEmpty(itemPickupPointList), ProductServiceImpl.ITEM_PICKUP_POINT_NOT_FOUND);
    }
    return itemPickupPointList;
  }

  private List<Item> getItemsForLatefulfillmentUpdate(String storeId, ProductAndL5MigrationRequest request) {
    List<Item> items = new ArrayList<>();
    if (ProductType.REGULAR.equals(request.getProductType())) {
      if (!request.isIncludeMfd()) {
        items = cacheItemHelperService.findCacheableByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
            request.getProductSku());
      } else {
        items = cacheItemHelperService.findCacheableByStoreIdAndProductSku(storeId, request.getProductSku());
      }
      checkState(CollectionUtils.isNotEmpty(items), ProductServiceImpl.ITEM_NOT_FOUND);
    }
    return items;
  }

  @Override
  public BulkDownloadProductBasicInfoResponse getProductBasicInfoByProductSkus(String storeId,
      ProductLevel3SummaryRequest request) {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse = new BulkDownloadProductBasicInfoResponse();
    List<ProductBasicInfoResponse> productBasicInfoResponseList = new ArrayList<>();
    ValidationUtil.validateProductBasicInfoBatchSize(request.getProductSkuList(), productBasicInfoFetchBatchSize);
    List<Product> productList = new ArrayList<>();
    Map<String, String> exceptionMap = new HashMap<>();
    List<String> productSkuList = request.getProductSkuList();
    try {
      for (String productSku : productSkuList) {
        Product product =
            productCacheHelperService.findProductByStoreIdAndProductSkuAndIncludeMarkForDelete(storeId, productSku,
                false);
        if (Objects.nonNull(product)) {
          productList.add(product);
        } else {
          exceptionMap.put(productSku, PRODUCT_NOT_FOUND);
        }
      }
    } catch (Exception e) {
      for (String productSku : productSkuList) {
        exceptionMap.put(productSku, PRODUCT_NOT_FOUND);
      }
      log.error("Exception while fetching data: {} ", productSkuList, e);
    }
    Set<String>productCodeSet = productList.stream().map(Product::getProductCode).collect(Collectors.toSet());
    List<BasicInfoProductResponse> basicInfoProductResponses = this.productCategoryBaseOutbound.getBasicInfoProductDetailsListByProductCodes(new ArrayList<>(productCodeSet));
    Map<String, BasicInfoProductResponse>basicInfoProductResponseMap = basicInfoProductResponses.stream().collect(Collectors.toMap(BasicInfoProductResponse::getProductCode, Function.identity(), (oldValue, newValue) -> newValue));

    for (Product product : productList) {
      BasicInfoProductResponse basicInfoProductResponse = basicInfoProductResponseMap.get(product.getProductCode());
      if (Objects.nonNull(basicInfoProductResponse)) {
        ProductBasicInfoResponse productBasicInfoResponse =
            CommonUtil.getProductBasicInfoResponse(product, basicInfoProductResponse);
        productBasicInfoResponseList.add(productBasicInfoResponse);
      } else {
        exceptionMap.put(product.getProductSku(), PRODUCT_NOT_FOUND);
      }
    }
    bulkDownloadProductBasicInfoResponse.setExceptionMap(exceptionMap);
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(productBasicInfoResponseList);
    return bulkDownloadProductBasicInfoResponse;
  }

  @Override
  public void updateMasterDataInfo(String storeId, String requestId, String username,
      ProductBasicMasterFieldsRequest request) throws Exception {
    log.info("Starting updateMasterDataInfo for storeId: {}, requestId: {}, username: {}, productSku: {} ", storeId,
        requestId, username, request.getProductSku());

    Product product = productCacheHelperService.findProductByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId,
        request.getProductSku());
    if (Objects.isNull(product)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, PRODUCT_NOT_FOUND + request.getProductSku());
    }
    log.debug("Fetched product: {} ", product);
    boolean productDetailUpdated = false;
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();

    boolean off2OnChannelActiveChanged = false;
    if (product.isOff2OnChannelActive() != request.isInstore()) {
      productDetailUpdated = true;
      off2OnChannelActiveChanged = true;
      log.info("Updating Off2OnChannelActive from {} to {} for productSku: {} ", product.isOff2OnChannelActive(),
          request.isInstore(), request.getProductSku());
      updateOff2OnChannelActive(request, product, auditTrailDtoList);
    }

    if (!StringUtils.equals(product.getProductType().name(), request.getProductType().name())) {
      productDetailUpdated = true;
      log.info("Updating ProductType from {} to {} for productSku: {} ", product.getProductType().name(),
          request.getProductType().name(), request.getProductSku());
      updateProductType(request, product, auditTrailDtoList);
    }

    //StringUtils.defaultString(str) returns "" when str is null; otherwise it returns the original string unchanged.
    if (!StringUtils.equals(StringUtils.defaultString(product.getSizeChartCode()),
        StringUtils.defaultString(request.getSizeChartCode()))) {
      productDetailUpdated = true;
      log.info("Updating SizeChartCode from {} to {} for productSku: {} ", product.getSizeChartCode(),
          request.getSizeChartCode(), request.getProductSku());
      updateSizeChartCode(request, product, auditTrailDtoList);
    }

    updateLateFulfillmentAndInstoreAtItem(storeId, request, off2OnChannelActiveChanged, product);

    if (request.isGenerateProductScoreNeeded()) {
      log.info("Generating product score for productSku: {} ", request.getProductSku());
      this.generateProductScoreByProductSku(storeId, request.getProductSku(), null, requestId, username, false,
          product);
    } else if (productDetailUpdated) {
      log.info("Saving updated product without updating Solr for productSku: {} ", request.getProductSku());
      product = saveOperationService.saveProductWithoutUpdatingSolr(product, Collections.emptyList(), StringUtils.EMPTY, Collections.EMPTY_MAP);

      ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO(product, new ArrayList<>());
      ProductAndItemEventModel eventModel = objectConverterService.convertToProductAndItemEventModel(productAndItemsVO);

      log.info("Publishing Solr update event for productSku: {}, eventModal: {} ", request.getProductSku(), eventModel);
      saveAndPublishService.publishSolrUpdateEvent(Collections.singletonList(eventModel));

    }
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      publishEventForHistory(auditTrailDtoList, username);
    }

    log.info("Completed updateMasterDataInfo for productSku: {} ", request.getProductSku());
  }

  private void updateLateFulfillmentAndInstoreAtItem(String storeId, ProductBasicMasterFieldsRequest request,
      boolean off2OnChannelActiveChanged, Product product) {
    if (Objects.nonNull(request.getLateFulfillment()) || off2OnChannelActiveChanged) {
      List<Item> itemList =
          itemService.findItemsByStoreIdAndProductSkuAndMarkForDeleteFalse(storeId, product.getProductSku());
      if (CollectionUtils.isNotEmpty(itemList)) {
        for (Item item : itemList) {
          if (Objects.nonNull(request.getLateFulfillment())) {
            item.setLateFulfillment(request.getLateFulfillment());
          }
          if (off2OnChannelActiveChanged) {
            item.setOff2OnChannelActive(request.isInstore());
          }
        }
        itemService.saveItems(itemList);
        for (Item item : itemList) {
          this.cacheEvictHelperService.evictItemData(item.getStoreId(), item);
        }
      }
    }
  }

  private static void updateSizeChartCode(ProductBasicMasterFieldsRequest request, Product product,
      List<AuditTrailDto> auditTrailDtoList) {
    String historyMessage = getHistoryMessage(request, product);
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(product.getMerchantCode(), Constants.DEFAULT, historyMessage, product.getSizeChartCode(),
            request.getSizeChartCode(), null, product.getProductSku(), product.getProductName());
    auditTrailDtoList.add(auditTrailDto);
    product.setSizeChartCode(request.getSizeChartCode());
  }

  private static void updateProductType(ProductBasicMasterFieldsRequest request, Product product,
      List<AuditTrailDto> auditTrailDtoList) {
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(product.getMerchantCode(), Constants.DEFAULT, Constants.PRODUCT_TYPE_CHANGED,
            product.getProductType().name(), request.getProductType().name(), null, product.getProductSku(),
            product.getProductName());
    auditTrailDtoList.add(auditTrailDto);
    product.setProductType(request.getProductType());
  }

  private static void updateOff2OnChannelActive(ProductBasicMasterFieldsRequest request, Product product,
      List<AuditTrailDto> auditTrailDtoList) {
    AuditTrailDto auditTrailDto =
        new AuditTrailDto(product.getMerchantCode(), Constants.DEFAULT, Constants.OFF_2ON_CHANNEL_CHANGED,
            String.valueOf(product.isOff2OnChannelActive()), String.valueOf(request.isInstore()), null,
            product.getProductSku(), product.getProductName());
    auditTrailDtoList.add(auditTrailDto);
    product.setOff2OnChannelActive(request.isInstore());
  }

  public static String getHistoryMessage(ProductBasicMasterFieldsRequest request, Product product) {
    String historyMessage;
    if (StringUtils.isEmpty(request.getSizeChartCode())) {
      historyMessage = Constants.SIZE_CHART_DELETED;
    } else if (StringUtils.isEmpty(product.getSizeChartCode())) {
      historyMessage = Constants.SIZE_CHART_ADDED;
    } else {
      historyMessage = Constants.SIZE_CHART_UPDATED;
    }
    return historyMessage;
  }

  private void publishEventForHistory(List<AuditTrailDto> auditTrailDtoList, String changedBy) {
    AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
    auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
    auditTrailListResponse.setAccessChannel(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setChangedBy(changedBy);
    auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID_X_PRODUCT);
    auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
    auditTrailListResponse.setUpdateDirectly(true);
    auditTrailListResponse.setUpdateDirectlyToDB(true);
    kafkaPublisher.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
  }
}
