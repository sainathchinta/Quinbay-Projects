package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkState;
import static com.gdn.partners.pbp.commons.constants.Constants.DEFAULT_STORE_ID;
import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toMap;

import java.beans.PropertyDescriptor;
import java.time.Duration;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gda.mta.product.dto.AuditTrailDto;
import com.gda.mta.product.dto.CogsDataResponse;
import com.gda.mta.product.dto.CogsUpdateDtoRequest;
import com.gda.mta.product.dto.CogsUpdateRequests;
import com.gda.mta.product.dto.DistributionInfoRequest;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gda.mta.product.dto.response.DistributionInfo;
import com.gdn.mta.product.config.RedisProperties;
import com.gdn.x.product.rest.web.model.dto.CategoryDTO;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gda.mta.product.dto.MasterDataUpdateRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityRequest;
import com.gda.mta.product.dto.NeedRevisionEligibilityResponse;
import com.gda.mta.product.dto.ProductMasterDataEditRequest;
import com.gdn.mta.product.entity.ProductSkuBusinessPartnerDTO;
import com.gdn.mta.product.enums.BrandApprovalStatus;
import com.gdn.mta.product.enums.L3InfoUpdateChangeType;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.partners.pbp.dao.InventoryDetailInfoResponseV2DTO;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryOutbound;
import com.gdn.partners.pbp.outbound.xProduct.feign.PromoEligibilityRequest;
import com.gdn.partners.pbp.workflow.product.CreateProductWorkflowWorkerBean;
import com.gdn.x.product.rest.web.model.request.CogsUpdateRequest;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.mta.product.valueobject.EligibilitySchedulesForEdit;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.response.ValidOmniChannelSkuResponse;
import com.google.common.collect.Lists;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.ListUtils;
import org.apache.commons.lang.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.product.util.BeanUtils;

import org.springframework.beans.BeanWrapper;
import org.springframework.beans.BeanWrapperImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AttributeCodeValueValueTypeDetails;
import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.CategoryDetailDto;
import com.gda.mta.product.dto.DeletedProductItems;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.FbbCreatePickupPointRequest;
import com.gda.mta.product.dto.FbbCreatePickupPointResponse;
import com.gda.mta.product.dto.ItemErrorListResponse;
import com.gda.mta.product.dto.ItemL5ListingRequest;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ItemPriceStockQuickUpdateResponse;
import com.gda.mta.product.dto.ItemSkuPpCodeRequest;
import com.gda.mta.product.dto.PickupPointDeleteRequest;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductEditValidationDTO;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gda.mta.product.dto.ProductL3CommonImageResponse;
import com.gda.mta.product.dto.ProductL3ListingRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductLevel3PriceRequest;
import com.gda.mta.product.dto.ProductLevel3QuickEditV2Request;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductLevel3UpdateRequest;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;
import com.gda.mta.product.dto.response.AuditTrailListRequest;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.ItemSummaryL4Response;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.ProductL3BasicResponse;
import com.gda.mta.product.dto.response.ProductL3ListingResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailResponse;
import com.gda.mta.product.dto.response.ProductLevel3DetailsV2Response;
import com.gda.mta.product.dto.response.ProductSuspensionHistoryResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.commons.constant.ProductLevel3SummaryCriteria;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.config.ApplicationProperties;
import com.gdn.mta.product.entity.ItemSkuToItemIdMapping;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductFieldHistory;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.entity.ProductLevel3Summary;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.AutoApprovalType;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.repository.BrandRepository;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.PickupPointRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductDistributionTaskRepository;
import com.gdn.mta.product.repository.ProductLevel3Repository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.converter.UpdateProductItemLevel3ModelConverter;
import com.gdn.mta.product.service.exception.ApiIncorrectInputDataException;
import com.gdn.mta.product.service.util.ApproveProductUtils;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.mta.product.util.ValidateUrlUtil;
import com.gdn.mta.product.util.ValueTypeUtil;
import com.gdn.mta.product.util.validator.DescriptiveFieldCharacterValidator;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.mta.product.valueobject.InventoryStockInfoDTO;
import com.gdn.mta.product.valueobject.UpdateProductItemLevel3Model;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.commons.constants.SaveHistoryConstants;
import com.gdn.partners.pbp.dto.productlevel3.ProductLevel3CountResponse;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.partners.pbp.outbound.AGPQuery.AGPQueryFeign;
import com.gdn.partners.pbp.outbound.campaign.CampaignOutbound;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.pickuppoint.PickupPointOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutboundBean;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.warehouse.WareHouseOutBound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1HistoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3Helper;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3WipService;
import com.gdn.partners.pbp.util.OfflineItemInventoryUtil;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.pbp.property.MandatoryParameterHelper;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.campaign.rest.web.model.dto.UpdateDiscountDTO;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailStockInfoRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.mta.distributiontask.rest.model.request.ChangeBrandRequest;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.enums.ProductType;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.L3VersionResponse;
import com.gdn.x.product.rest.web.model.NewlyAddedL5Response;
import com.gdn.x.product.rest.web.model.dto.ItemBuyableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemDiscoverableScheduleDTO;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.PriceDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointQuickEditRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductCountResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.product.rest.web.model.response.ProductL3SummaryResponse;
import com.gdn.x.product.rest.web.model.response.ProductL5DetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;
import com.gdn.x.product.rest.web.model.response.ViewConfigResponse;
import com.gdn.x.productcategorybase.dto.BaseDTOResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryNamesResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.google.api.services.youtube.YouTube;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductLevel3V2ServiceImpl implements ProductLevel3V2Service {

  private static final int PRODUCT_NAME_LIMIT = 150;
  public static final Integer MAXIMUM_UNIQUE_SELLING_POINT_LENGTH = 400;
  private static final String COMMA_SEPARATOR = ",";
  public static final Integer MAXIMUM_DESCRIPTION_LENGTH = 5000;
  private static final String REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE = "([^\\x00-\\x7F])|(/ +/ig)";
  private static final String REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
    "(\\<.*?\\>|&\\w+.;)|(/\\r\\n|\\n|\\r/gm)|([\\•|\\)]\\s+)";
  private static final Pattern PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
    Pattern.compile(REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT);
  private static final Pattern PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE =
    Pattern.compile(REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE);
  private static final String REGEX_FOR_EXTRA_SPACE = "\\s+";
  private static final Pattern PATTERN_FOR_EXTRA_SPACE = Pattern.compile(REGEX_FOR_EXTRA_SPACE);
  private static final Double GMS_TO_KG_FACTOR = 1000.00;
  public static final int WHOLESALE_DISCOUNT_MAX_THRESHOLD = 100;
  public static final int WHOLESALE_DISCOUNT_MIN_THRESHOLD = 0;
  private final String[] CAMPAIGN_REQUEST = {"price"};
  private final String[] INVENTORY_REQUEST = {"deltaStock", "useWarehouseStock"};
  private final String[] PRODUCT_REQUEST =
    {"cncActive", "sellerSku", "off2OnActiveFlag", "wholeSaleActivated", "status"};
  private final String OFF2ON_ACTIVE_FLAG = "off2OnActiveFlag";
  private final String WHOLESALE_ACTIVATED = "wholeSaleActivated";
  private final String ACTIVE = "ACTIVE";
  private final String NEED_CORRECTION = "NEED_CORRECTION";
  private final String IN_PROGRESS = "IN_PROGRESS";
  private final String DELETED = "DELETED";
  public static final String SHIPPING_WEIGHT = "Shipping Weight";
  public static final String WEIGHT = "Weight";
  public static final String HEIGHT = "Height";
  public static final String LENGTH = "Length";
  public static final String WIDTH = "Width";
  public static final String ITEM_SKU = "itemSku";
  public static final String NEW_UPC_CODE = "newUpcCode";
  public static final String OLD_UPC_CODE = "oldUpcCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ITEM_NAME = "itemName";
  public static final String INVALID_EAN_UPC_FORMAT = "EAN UPC code is not in valid format";

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private CampaignOutbound campaignOutbound;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3Helper productLevel3Helper;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Autowired
  private UpdatedProductHistoryService updatedProductHistoryService;

  @Autowired
  private ProductLevel3WipService productLevel3WipService;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private PickupPointRepository pickupPointRepository;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Autowired
  private UpdateProductItemLevel3ModelConverter updateProductItemLevel3ModelConverter;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  @Lazy
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private YouTube youTube;

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private AGPQueryFeign agpQueryFeign;

  @Autowired
  private AGPQueryOutbound agpQueryOutbound;

  @Autowired
  private ProductOutboundBean productOutboundBean;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Autowired
  private DistributionInfoService distributionInfoService;

  @Autowired
  private StringRedisTemplate stringRedisTemplate;

  @Autowired
  private RedisProperties redisProperties;

  @Autowired
  private ApplicationContext applicationContext;

  @Value(value = "${feature.product.limit.redis.enabled}")
  private boolean productLimitRedisEnabled;

  @Value(value = "${feature.product.limit.redis.increment.enabled}")
  private boolean productLimitRedisIncrementEnabled;

  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value(value = "${upc.code.validate}")
  private boolean upcCodeValidate;

  @Value("${add.delete.warehouse.feature.switch}")
  private boolean addDeleteForWarehouseSwitch;

  @Value("${validate.attribute.at.product.and.category.level.switch}")
  private boolean validateAttributeAtProductAndCategory;

  @Value("${product.name.edit.validation.switch}")
  private boolean productNameEditValidationSwitch;

  @Value("${fetch.l3.detail.plp.pdp.edit}")
  private boolean fetchL3DetailForPLPAndPDPEdit;

  @Value("${max.characters.in.description}")
  private int maximumCharactersInDescription;

  @Value("${max.characters.without.formatting.description}")
  private int maximumCharactersWithoutFormattingDescription;

  @Value("${remove.all.extra.space.before.name.edit.validation}")
  private boolean removeAllExtraSpaceBeforeNameEditValidation;

  @Value("${migrate.product.in.edit.logistic.update.flow}")
  private boolean migrateProductInEditLogisticUpdateFlow;

  @Value("${fetch.l4.based.on.mfd.in.need.revision}")
  private boolean fetchL4BasedOnMfdInNrFlow;

  @Value("${validate.ean.upc.code.format}")
  private boolean validateEanUpcCodeFormat;

  @Value("${ean.upc.valid.length}")
  private List<Integer> eanUpcValidLength;

  @Value("${variant.restricted.values}")
  private List<String> variantRestrictedValues;

  @Value("${variant.restricted.values.flag}")
  private boolean variantRestrictedValuesFlag;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${unique.value.type.addition.enabled}")
  private boolean uniqueValueTypeAdditionEnabled;

  @Value("${instore.new.flow.enabled}")
  private boolean instore2FlowSwitch;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Autowired
  private ApplicationProperties applicationProperties;

  @Autowired
  private WareHouseOutBound wareHouseOutBound;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private PickupPointOutbound pickupPointOutbound;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private BrandRepository brandRepository;

  @Autowired
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ProductL3Service productL3Service;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Autowired
  private ProductLevel3ServiceBean productLevel3ServiceBean;

  @Autowired
  private ProductLevel3Repository productLevel3Repository;

  @Autowired
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Autowired
  private ProductLevel1HistoryService productLevel1HistoryService;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Value("${add.delete.variants.validation.switch}")
  private boolean addDeleteVariantsValidationSwitch;

  @Value("${delete.variant.validation.switch}")
  private boolean deleteVariantValidationSwitch;

  @Value("${system.parameter.max.stock.value}")
  private long maxStockLimit;

  @Value("${validate.edit.pickup.points}")
  private boolean validateEditPickupPoints;

  @Value("${validate.edit.shipping.dimensions}")
  private boolean validateEditShippingAndDimensions;

  @Value("${validate.product.descriptive.fields.characters}")
  private boolean validateProductDescriptiveFieldCharacters;

  @Value("${validate.product.descriptive.exclusion.list}")
  private String validateProductDescriptiveFieldExclusionList;

  @Value("${override.online.with.b2c}")
  private boolean overrideOnlineWithB2CFlag;

  @Value("${max.product.dimension.limit}")
  private int maxProductDimensionLimit;

  @Value("${combine.l3.and.l5.validation}")
  private boolean combineL3AndL5Validation;

  @Value("${preOrder.maximum.days}")
  private int preOrderMaximumDays;

  @Value("${preOrder.working.maximum.week}")
  private int preOrderMaximumWeek;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${validate.youtube.url.new.flow}")
  private boolean validateYoutubeUrlNewFlow;

  @Value("${product.name.character.limit}")
  private int productNameCharacterLimit;

  @Autowired
  private ObjectMapper objectMapper;


  @Value("${process.variant.image.newly.added.items}")
  private boolean processVariantImageNewlyAddedItems;

  @Value(value = "${enable.product.code.image.location.validation}")
  private boolean productCodeLocationValidate;

  @Value(value = "${validate.image.path.enabled}")
  private boolean validateImagePathEnabled;

  @Value(value = "${return.immediately.if.price.edit.not.allowed}")
  private boolean returnImmediatelyIfPriceEditNotAllowed;

  @Value(value = "${validate.new.product.item.l5.request}")
  private boolean validateNewProductItemL5Request;

  @Value(value = "${validate.new.product.item.image.request}")
  private boolean validateNewProductItemImageRequest;

  @Value(value = "${sanitize.product.name.and.seller.sku}")
  private boolean sanitizeProductNameAndSellerSku;

  @Value(value = "${sync.product.data.on.master.data.update}")
  private boolean syncProductDataOnMasterDataUpdate;

  @Value(value = "${schedules.add.edit.enabled}")
  private boolean schedulesAddEditEnabled;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${size.chart.addition.for.product}")
  private boolean sizeChartAdditionForProduct;

  @Value("${set.waiting.deletion.for.delete.pickup.point}")
  private boolean setWaitingDeletionForDeletePickupPoint;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCNCRestrictionEnabled;

  @Value("${bopis.category.validation.for.merchant.types}")
  private String bopisUnsupportedMerchantTypes;

  @Value("${merge.stock.value.sync.stock.update}")
  private boolean mergeStockValueAndSyncStockUpdate;

  @Value("${need.revision.deletion.threshold}")
  private int needRevisionDeletionThreshold;

  @Value("${youtube.regex}")
  private String youtubeRegex;

  @Value("${seller.penalty.enabled.phase2}")
  private boolean sellerPenaltyEnabledPhase2;

  @Autowired
  private ProductBusinessPartnerServiceBean productBusinessPartnerServiceBean;

  @Autowired
  private CreateProductWorkflowWorkerBean createProductWorkflowWorkerBean;

  @Value("${brand.category.edit.enabled.for.external}")
  private boolean brandCategoryEditEnabledForExternal;


  @Override
  public ApiErrorCode productQuickEditV2(String storeId, String productSku,
    ProductLevel3QuickEditV2Request request, boolean isExternalOnly) throws Exception {
    Map<String, QuickEditV2Request> salePriceChangeEditRequestMap = new HashMap<>();
    Map<String, Boolean> wholeSaleActivatedMap = new HashMap<>();
    sanitizeSellerSkuV2(request);
    request.getQuickEditV2Requests().forEach(
      quickEditV2Request -> quickEditV2Request.setItemPickupPointId(
        CommonUtils.toL5Id(quickEditV2Request.getItemSku(),
          quickEditV2Request.getPickupPointCode())));
    Map<String, ItemSummaryListResponse> itemSummaryResponseMap =
      fetchAndGenerateItemSummaryListResponseMap(request.getQuickEditV2Requests());
    ProfileResponse businessPartner =
        this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
            itemSummaryResponseMap.entrySet().iterator().next().getValue().getMerchantCode());
    List<String> archivedProducts =
      itemSummaryResponseMap.values().stream().filter(ItemSummaryListResponse::isArchived)
        .map(ItemSummaryListResponse::getProductSku).collect(toList());
    if (CollectionUtils.isNotEmpty(archivedProducts)) {
      log.error("ProductSku : {} are archived at L3 ", archivedProducts);
      return ApiErrorCode.PRODUCT_IS_ARCHIVED;
    }

    ApiErrorCode preOrderValidationResult =
      validateOmgPreOrderStockUpdate(businessPartner, request, productSku);
    if (Objects.nonNull(preOrderValidationResult)) {
      return preOrderValidationResult;
    }

    for (QuickEditV2Request quickEditV2Request : request.getQuickEditV2Requests()) {
      ItemSummaryListResponse itemSummaryListResponse =
        itemSummaryResponseMap.get(quickEditV2Request.getItemPickupPointId());
      if (Objects.isNull(itemSummaryListResponse)) {
        log.error("ItemPickupPoint not found for id : {}", quickEditV2Request.getItemPickupPointId());
        return ApiErrorCode.MISSING_ITEM_PICKUP_POINT;
      }
      try {
        validateVersionAndPriceForListingUpdate(quickEditV2Request, itemSummaryListResponse,
          salePriceChangeEditRequestMap);
      } catch (ApplicationRuntimeException e) {
        if ((ErrorCategory.VALIDATION.getMessage()
          + ApiErrorCode.PRICE_UPDATE_FAILED.getDesc()).equals(e.getErrorMessage())) {
          return ApiErrorCode.PRICE_UPDATE_FAILED;
        }
        log.error("Exception while productQuickEdit v1, request : {}", quickEditV2Request, e);
        throw e;
      }
      validateStatusAndCncUpdateForBopisProduct(quickEditV2Request,businessPartner,itemSummaryListResponse);
      CommonUtils.validateSyncStockForFaasSeller(quickEditV2Request, businessPartner,
          itemSummaryListResponse, faasFeatureSwitch);
      if (CommonUtils.validateStockIncrementForSellerPenalty(businessPartner,
          sellerPenaltyEnabledPhase2, quickEditV2Request.getDeltaStock())) {
        log.error("Stock update failed:Not allowed to increase stock due to seller penalty "
            + "restrictions for item sku : {} ", quickEditV2Request.getItemSku());
        return ApiErrorCode.SELLER_PENALTY_RESTRICTION;
      }
    }
    if(instore2FlowSwitch) {
      validateStatusAndCncUpdateForInstoreProduct(request,
        itemSummaryResponseMap);
    }
    try {
      updateItemPickupPointListing(itemSummaryResponseMap, wholeSaleActivatedMap, request,
        productSku, storeId, businessPartner);
      updateCampaignPrice(salePriceChangeEditRequestMap,
        itemSummaryResponseMap.values().stream().map(ItemSummaryListResponse::getMasterCategoryCode)
          .findFirst().get());
    } catch (Exception e) {
      log.error("Exception while updating details in x-product , productSku : {} ", productSku, e);
      throw e;
    }
    return null;
  }

  private ApiErrorCode validateOmgPreOrderStockUpdate(ProfileResponse businessPartner,
    ProductLevel3QuickEditV2Request request, String productSku) {
    if (preOrderConfig.isPoQuotaFeatureSwitch() && CommonUtils.getBusinessPartnerFlagValue(
        businessPartner, Constants.BLIBLI_OMG) && request.getQuickEditV2Requests().stream()
        .anyMatch(quickEditV2Request -> Objects.nonNull(quickEditV2Request.getDeltaStock())
          && quickEditV2Request.getDeltaStock() != 0)) {
      PreOrderDTO preOrder = this.xProductOutbound.getBasicProductInfoV2(productSku).getPreOrder();
      if (CommonUtils.isPreOrderDateValid(
          Optional.ofNullable(preOrder).orElse(new PreOrderDTO()).getPreOrderDate())) {
        return ApiErrorCode.INVALID_STOCK_UPDATE_PREORDER_OMG;
      }
    }
    return null;
  }

  private void validateStatusAndCncUpdateForInstoreProduct(ProductLevel3QuickEditV2Request request,
    Map<String, ItemSummaryListResponse> itemSummaryListResponseMap) {
    Optional<String> productCode = itemSummaryListResponseMap.values().stream().findFirst()
      .map(ItemSummaryListResponse::getProductCode);
    boolean isL5NotOffline = request.getQuickEditV2Requests().stream().anyMatch(
      item -> item.getStatus() != ProductLevel3Status.OFFLINE
        || item.getCncStatus() != ProductLevel3Status.OFFLINE);
    if (productCode.isPresent() && isL5NotOffline) {
      boolean isMissingFieldsPresent =
        itemSummaryListResponseMap
          .values()
          .stream()
          .map(ItemSummaryListResponse::getMissingFields)
          .filter(CollectionUtils::isNotEmpty)
          .flatMap(Set::stream)
          .anyMatch(Constants.MISSING_FIELDS_SET::contains);
      if (isMissingFieldsPresent) {
        log.error("Error updating status instore product : {} ",
          request.getQuickEditV2Requests().stream().findFirst().get().getItemSku());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INSTORE_STATUS_CHANGE_ERROR.getDesc());
      }
    }
  }

  private void sanitizeSellerSkuV2(ProductLevel3QuickEditV2Request request) {
    if (sanitizeProductNameAndSellerSku && CollectionUtils.isNotEmpty(request.getQuickEditV2Requests())) {
      for (QuickEditV2Request quickEditV2Request : request.getQuickEditV2Requests()) {
        if (StringUtils.isNotBlank(quickEditV2Request.getSellerSku())) {
          quickEditV2Request.setSellerSku(ValidationUtil.validateDataForProductName(quickEditV2Request.getSellerSku()));
        }
      }
    }
  }

  @Override
  public void validateProductEditInfo(ProductL3UpdateRequest request, ProductL3Response savedProductData) throws Exception {
    if (Objects.nonNull(request.getUniqueSellingPoint())) {
      String uspWithoutTags = getFilteredUSPAndDescription(request.getUniqueSellingPoint());
      GdnPreconditions.checkArgument(uspWithoutTags.length() <= MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
        ErrorMessages.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBusinessPartnerCode()),
      ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isBlank(request.getCategoryCode()),
        ErrorMessages.CATEGORY_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions
      .checkArgument(!request.getAttributes().isEmpty(), ErrorMessages.ATTRIBUTE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getSpecificationDetail()),
      ErrorMessages.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK);
    GdnPreconditions
      .checkArgument(!StringUtils.isEmpty(request.getAttributes().get(0).getAttributeCode()),
        ErrorMessages.ATTRIBUTE_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions
      .checkArgument(!StringUtils.isEmpty(request.getAttributes().get(0).getAttributeType()),
        ErrorMessages.ATTRIBUTE_TYPE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!request.getAttributes().get(0).getValues().isEmpty(),
      ErrorMessages.ATTRIBUTE_VALUE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(request.getAttributes().get(0).getSkuValue() != null,
      ErrorMessages.ATTRIBUTE_SKU_VALUE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getBrand()),
      ErrorMessages.BRAND_MUST_NOT_BE_BLANK);
    request.setPureInstoreProduct(CommonUtils.isPureInstoreProduct(request.isOff2OnChannelActive(),
      request.getB2cActivated(), instore2FlowSwitch));
    ValidationUtil.validateProductNameAndDescription(productNameCharacterLimit, request);
    validateDescriptionLength(request.getDescription());
    validateUpcCode(request);
    for (com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest :
        request.getAddPickupPoints()) {
      validateCncViewConfig(itemPickupPointRequest);
    }
    for (ProductVariantPriceStockAndImagesRequest items : request.getProductItems()) {
      for (com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest : items.getModifiedItemPickupPoints()) {
        validateCncViewConfig(itemPickupPointRequest);
        if (CommonUtils.isPureInstoreProduct(request.isOff2OnChannelActive(),
          request.getB2cActivated(), instore2FlowSwitch)) {
          boolean defaultStatus =
            itemPickupPointRequest.isBuyable() || itemPickupPointRequest.isDisplay();
          boolean cncStatus =
            itemPickupPointRequest.isCncBuyable() || itemPickupPointRequest.isCncDisplay();
          GdnPreconditions.checkArgument(!defaultStatus && !cncStatus,
            ErrorMessages.STATUS_CAN_NOT_BE_OTHER_THAN_OFFLINE_FOR_PURE_INSTORE_PRODUCT);
        }
        if (Objects.nonNull(itemPickupPointRequest.getB2bFields())) {
          GdnPreconditions.checkArgument(Objects.nonNull(itemPickupPointRequest.getB2bFields().getPrice()),
              ErrorMessages.BASE_PRICE_SHOULD_NOT_BE_NULL);
          Double minimumPrice = getMinimumPrice(Constants.DEFAULT_STORE_ID).doubleValue();
          GdnPreconditions.checkArgument(itemPickupPointRequest.getB2bFields().getPrice() >= minimumPrice,
              String.format(ErrorMessages.B2B_BASE_PRICE_MUST_BE_GREATER_THAN_MINIMUM_PRICE, minimumPrice));
        }
      }
    }
    if (!validateYoutubeUrlNewFlow) {
      validateYoutubeUrlIsValid(request, savedProductData);
    }

    DescriptiveFieldCharacterValidator.validateProductLevel3Request(request,
        Arrays.asList(validateProductDescriptiveFieldExclusionList.split(Constants.COMMA)),
        validateProductDescriptiveFieldCharacters);
    if (StringUtils.isNotBlank(request.getSizeChartCode()) && sizeChartAdditionForProduct) {
      ApiErrorCode apiErrorCodeForSizeChart =
          productService.validateSizeChart(request.getSizeChartCode(), request.getStoreId());
      GdnPreconditions.checkArgument(Objects.isNull(apiErrorCodeForSizeChart),
          ApiErrorCode.SIZE_CHART_CODE_INVALID.getDesc());
    }

  }

  @Override
  public void validateDescriptionLength(String description) {
    String descriptionWithoutTags = getFilteredUSPAndDescription(description);
    GdnPreconditions.checkArgument(descriptionWithoutTags.length() <= maximumCharactersInDescription,
      String.format(ErrorMessages.DESCRIPTION_MUST_NOT_BE_MORE_THAN_MAX_CHARACTERS, maximumCharactersInDescription));
    GdnPreconditions.checkArgument(description.length() <= maximumCharactersWithoutFormattingDescription,
      String.format(ErrorMessages.CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING,
        maximumCharactersWithoutFormattingDescription));
  }


  @Override
  public <T extends MasterDataUpdateRequest> void validateYoutubeUrlIsValid(T request,
    ProductL3Response savedProductData) throws Exception {
    if (isUrlChanged(request, savedProductData)) {
      if (Objects.isNull(ValidateUrlUtil.checkIfUrlIsValid(request.getUrl(), youtubeRegex))) {
        log.error("Removing invalid url {} from product : {}", request.getUrl(),
          request.getProductCode());
        request.setUrl(StringUtils.EMPTY);
      } else {
        ProductSystemParameter productSystemParameter =
          productSystemParameterService.findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
            Constants.YOUTUBE_URL_VALIDATION_SWITCH);

        String[] apiKeys = youTubeDataApiKey.split(COMMA_SEPARATOR);
        int apiKeyIndex = (int) (Math.random() * apiKeys.length);
        String youTubeApiKey = apiKeys[apiKeyIndex];

        log.debug("Using youtube api key index: {} for url: {}", apiKeyIndex, request.getUrl());

        boolean youTubeUrlResponse =
          ValidateUrlUtil.validateYouTubeUrl(request.getUrl(), youTubeApiKey, youTube,
            Boolean.parseBoolean(productSystemParameter.getValue()), youtubeRegex);

        if (!youTubeUrlResponse) {
          request.setUrl(StringUtils.EMPTY);
        }
      }
    }
  }

  private static <T extends MasterDataUpdateRequest> boolean isUrlChanged(T request,
    ProductL3Response savedProductData) {
    boolean urlChanged = false;
    if (StringUtils.isNotBlank(request.getUrl())) {
      if (Objects.nonNull(savedProductData)) {
        // Case 1: savedProductData is present → check if URL has changed
        urlChanged = !StringUtils.equals(request.getUrl(),
          Optional.ofNullable(savedProductData.getUrl()).orElse(StringUtils.EMPTY));
      } else {
        // Case 2: No savedProductData, but change type indicates URL update
        urlChanged = request.getMasterDataEditChangeTypes()
          .contains(L3InfoUpdateChangeType.YOUTUBE_URL_UPDATE);
      }
    }
    return urlChanged;
  }


  private void validateCncViewConfig(com.gda.mta.product.dto.ItemPickupPointRequest itemPickupPointRequest) {
    if (cncForWarehouseFeatureSwitch) {
      boolean defaultStatus =
          itemPickupPointRequest.isBuyable() || itemPickupPointRequest.isDisplay();
      boolean cncStatus =
          itemPickupPointRequest.isCncBuyable() || itemPickupPointRequest.isCncDisplay();
      if (cncStatus && defaultStatus) {
        GdnPreconditions.checkArgument(
            itemPickupPointRequest.isBuyable() == itemPickupPointRequest.isCncBuyable()
                && itemPickupPointRequest.isDisplay() == itemPickupPointRequest.isCncDisplay(),
            ApiErrorCode.INVALID_DELIVERY_CNC_VIEW_CONFIG_STATES.getDesc());
      }
    }
  }

  private void validateUpcCode(ProductL3UpdateRequest request) {
    if (upcCodeValidate) {
      validateEanUpcCode(request);
      List<String> upcCodeForNewItems = getUpcCodesForNewItems(request, new ArrayList<>());
      List<String> deletedItemCodes = getDeletedItemCodes(request, new ArrayList<>());
      List<String> filteredItems = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(upcCodeForNewItems)) {
        ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest =
            generateProductItemUpcCodesSkuCodesRequest(request, upcCodeForNewItems);
        List<String> itemCodes = new ArrayList<>();
        itemCodes = productOutbound
            .getItemCodesByUpcCodeAndProductCode(request.getProductCode(), productItemUpcCodesSkuCodesRequest);
        for (String itemCode : itemCodes) {
          if (!deletedItemCodes.contains(itemCode)) {
            filteredItems.add(itemCode);
          }
        }
        if (CollectionUtils.isNotEmpty(filteredItems)) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc());
        }
      }
    }
  }

  private void validateEanUpcCode(ProductL3UpdateRequest request) {
    if (validateEanUpcCodeFormat) {
      ValidationUtil.validateFormatAndDuplicateUpcCodes(request, eanUpcValidLength);
    } else {
      checkDuplicateUpcCodes(request);
    }
  }

  @Override
  public ProductL3UpdateRequest editRequestToBillOfMaterialRecipeRequest(ProductL3UpdateRequest request,
      ProductL3Response savedProductData) {
    Map<String, String> itemSkuAndItemCodeMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(request.getProductBundleRecipe())) {
      try {
        List<ItemBasicDetailV2Response> itemBasicDetailV2Response = xProductOutbound.getItemBasicDetailV2Response(
            ConverterUtil.getDistinctRecipeItemSkus(request.getProductBundleRecipe()), false);
        ConverterUtil.addItemSkuAndItemCodeToMap(itemBasicDetailV2Response, itemSkuAndItemCodeMap);
        if (isProductItemsPresentInMap(request, itemSkuAndItemCodeMap)) {
          getItemSkuAndItemCodeMapForTheEditedProduct(request, savedProductData, itemSkuAndItemCodeMap);
        }
      } catch (Exception e) {
        log.error("Error while fetching details of itemSkus from x-product with error message : ", e);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.INVALID_ITEM_SKU.getDesc());
      }
      Map<String, String> finalItemSkuAndItemCodeMap = itemSkuAndItemCodeMap;
      List<ProductBundleRecipeRequest> productBundleRecipe =
          productLevel3Helper.validateShareProductRecipe(request, finalItemSkuAndItemCodeMap);
      for (ProductBundleRecipeRequest productBundleRecipeRequest : productBundleRecipe) {
        CreateUpdateBillOfMaterialRecipeCommandRequest createUpdateBillOfMaterialRecipeRequest =
            new CreateUpdateBillOfMaterialRecipeCommandRequest();
        createUpdateBillOfMaterialRecipeRequest.setBillOfMaterialSetup(
            RequestHelper.getBillOfMaterialSetup(productBundleRecipeRequest.getBundleRecipe(),
                finalItemSkuAndItemCodeMap));
        createUpdateBillOfMaterialRecipeRequest.setItemCode(
            finalItemSkuAndItemCodeMap.get(productBundleRecipeRequest.getItemSku()));
        wareHouseOutBound.createAndUpdateProductBundle(createUpdateBillOfMaterialRecipeRequest);
      }
    }
    return request;
  }

  private boolean isProductItemsPresentInMap(ProductL3UpdateRequest request,
      Map<String, String> itemSkuAndItemCodeMap) {
    return Optional.ofNullable(request.getProductBundleRecipe()).orElseGet(() -> new ArrayList<>()).stream()
        .map(ProductBundleRecipeRequest::getItemSku).anyMatch(Predicate.not(itemSkuAndItemCodeMap::containsKey));
  }

  private void getItemSkuAndItemCodeMapForTheEditedProduct(ProductL3UpdateRequest request,
      ProductL3Response savedProductData, Map<String, String> itemSkuAndItemCodeMap) throws Exception {
    Map<String, String> itemMap = Optional.ofNullable(savedProductData)
      .map(ProductL3Response::getItemSkuItemCodeMap)
      .orElse(Collections.emptyMap());
    if (MapUtils.isNotEmpty(itemMap)) {
      Optional.ofNullable(savedProductData).map(ProductL3Response::getItemSkuItemCodeMap)
        .filter(MapUtils::isNotEmpty).ifPresent(itemSkuAndItemCodeMap::putAll);
    } else {
      ProductDetailResponse productDetailResponse =
          productRepository.findProductDetailByProductCode(request.getProductCode());
      if (Objects.nonNull(productDetailResponse)) {
        itemSkuAndItemCodeMap.putAll(productItemBusinessPartnerService.getItemSkuAndItemCodeMappingUsingItemIds(DEFAULT_STORE_ID,
            ConverterUtil.getItemIdToItemCodeMap(productDetailResponse)));
      } else {
        log.error("Error while fetching details of product from PCB with productCode : {} ", request.getProductCode());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.PRODUCT_NOT_PRESENT.getDesc());
      }
    }
  }

  private ProductItemUpcCodesSkuCodesRequest generateProductItemUpcCodesSkuCodesRequest(ProductL3UpdateRequest request,
      List<String> upcCodeForNewItems) {
    ProductItemUpcCodesSkuCodesRequest productItemUpcCodesSkuCodesRequest =
        new ProductItemUpcCodesSkuCodesRequest();
    productItemUpcCodesSkuCodesRequest.setSkuCodes(
        request.getProductItems().stream().map(ProductVariantPriceStockAndImagesRequest::getSkuCode)
            .filter(StringUtils::isNotEmpty)
            .collect(toList()));
    productItemUpcCodesSkuCodesRequest.setUpcCodes(upcCodeForNewItems);
    return productItemUpcCodesSkuCodesRequest;
  }

  public List<String> getUpcCodesForNewItems(ProductL3UpdateRequest request, List<String> upcCodeForNewItems) {
    if (CollectionUtils.isNotEmpty(request.getProductItems())) {
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : request
          .getProductItems()) {
        if (productVariantPriceStockAndImagesRequest.isNewlyAddedItem() && StringUtils
            .isNotEmpty(productVariantPriceStockAndImagesRequest.getUpcCode())) {
          upcCodeForNewItems.add(productVariantPriceStockAndImagesRequest.getUpcCode());
        }
      }
    }
    return upcCodeForNewItems;
  }

  private List<String> getDeletedItemCodes(ProductL3UpdateRequest request, List<String> deletedItemCodes) {
    if (CollectionUtils.isNotEmpty(request.getDeletedProductItems())) {
      deletedItemCodes =
          request.getDeletedProductItems().stream().map(DeletedProductItems::getItemCode).collect(toList());
    }
    return deletedItemCodes;
  }

  public void checkDuplicateUpcCodes(ProductL3UpdateRequest request) {
    if (CollectionUtils.isNotEmpty(request.getProductItems())) {
      List<String> upcCodes =
          request.getProductItems().stream().map(ProductVariantPriceStockAndImagesRequest::getUpcCode)
              .filter(StringUtils::isNotEmpty).collect(toList());
      if (upcCodes.stream().distinct().count() != upcCodes.size()) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.UPC_CODE_UPDATE_FAILED.getDesc());
      }
    }
  }

  @Override
  public ProductLevel3 generateProductLevel3(ProductL3UpdateRequest request) {
    return productLevel3Helper.generateProductLevel3(request);
  }

  @Override
  public ApiErrorCode updateLogistics(ProductLevel3UpdateRequest product, boolean isOnlyExternal,
      boolean isNeedCorrection, boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate) throws Exception {
    ApiErrorCode errorCode = null;
    if (isNeedCorrection) {
      errorCode = productLevel3Service.updateLogisticsForNeedRevision(product);
    } else {
      errorCode =
          productLevel3Service.updateLogistics(product, isOnlyExternal, null, combineContentAndLogisticsPcbUpdate,
              combinePreOrderUpdate, migrateProductInEditLogisticUpdateFlow);
    }
    return errorCode;
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse updateProductItemImages(
    ProductL3UpdateRequest l3UpdateRequest, ProductVariantUpdateRequest variantUpdateRequest) throws Exception {
    return productLevel3Service.updateProductItemImages(l3UpdateRequest,variantUpdateRequest);
  }

  @Override
  public EditProductResponse updateEditInfo(ProductLevel3 request, boolean isOnlyExternal,
      boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate , ProfileResponse profileResponse , ProductL3Response productL3Response,
    boolean newImagesAdded, ProductCollection productCollection, ProductL3UpdateRequest productL3UpdateRequest) throws Exception {
    EditProductResponse editProductResponse =
        productLevel3Service.updateEditInfo(request, isOnlyExternal, false, combineContentAndLogisticsPcbUpdate,
            profileResponse, combinePreOrderUpdate,productL3Response,
            newImagesAdded, productCollection, productL3UpdateRequest);
    if (Objects.nonNull(editProductResponse)) {
      editProductResponse.setProfileResponse(profileResponse);
    }
    return editProductResponse;
  }

  @Override
  public void takeDownProduct(String storeId, String productSku, String productName)
    throws Exception {
    ProductL3Response savedProductData =
      xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    productLevel3Service.takeDownOrReactivateProduct(storeId, productSku, true,
      productName, savedProductData);
  }

  @Override
  public ProductLevel3DetailsV2Response fetchL3ProductDetailsByProductSku(String storeId,
    String productSku, boolean concatValueAndValueTypes, boolean needInventoryData) throws Exception {
    log.info("Fetch L3 Product Details by ProductSku : " + "{}, isNeedCorrection : {}", productSku);

    String categoryCode, merchantCode, itemSku, productCode;
    ProductL3Response productL3Response = new ProductL3Response();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
    ProductCollection productCollection = new ProductCollection();
    InventoryStockInfoDTO inventoryL3 = new InventoryStockInfoDTO();
    ProductSummaryResponseV2 productSummaryResponseV2 = new ProductSummaryResponseV2();
    ProductSummaryRequestV2 productSummaryRequestV2 = new ProductSummaryRequestV2();

    if (Objects.isNull(productBusinessPartner)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND);
    }
    else if(productBusinessPartner.getState().equals(DELETED)){
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ApiErrorCode.PRODUCT_DELETED_STATE.getDesc());
    }
    else if (productBusinessPartner.getState().equals(NEED_CORRECTION) || productBusinessPartner
      .getState().equals(IN_PROGRESS)) {
      productCode = productCollectionRepository.getProductCodeByGdnSku(productSku);
      productDetailResponse =
        productOutbound.getProductDetailByProductCode(productCode, true, false);
      categoryCode = productBusinessPartner.getCategoryCode();
      merchantCode = productBusinessPartner.getBusinessPartnerId();
      itemSku = Optional.ofNullable(productBusinessPartner.getProductItemBusinessPartners())
        .orElse(new ArrayList<>()).stream().filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).findFirst().get().getGdnProductItemSku();
      productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
    } else {
      productL3Response = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
      checkState(Objects.nonNull(productL3Response), ErrorMessages.PRODUCT_SKU_NOT_FOUND);
      checkState(Objects.nonNull(productL3Response.getMasterCatalog()),
        ErrorMessages.MASTER_CATALOG_NOT_FOUND);
      categoryCode = productL3Response.getMasterCatalog().getCategory().getCategoryCode();
      merchantCode = productL3Response.getMerchantCode();
      itemSku = productL3Response.getDefaultItemSku();
      productCode = productL3Response.getProductCode();
      productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(storeId, productCode);
      Map<String, InventoryStockInfoDTO> inventoryStockInfoDTOMap = new HashMap<>();
      if (needInventoryData) {
        inventoryStockInfoDTOMap = fetchProductSkuToL3InventoryDetailMap(Collections.singletonList(productSku),
            productL3Response.getMerchantCode());
      }
      inventoryL3 = inventoryStockInfoDTOMap.getOrDefault(productSku, new InventoryStockInfoDTO());
      productSummaryRequestV2.setMerchantCode(merchantCode);
      productSummaryRequestV2.setProductSkuList(Collections.singletonList(productSku));

      List<ProductSummaryResponseV2> summaryResponseV2 =
          xProductOutbound.getProductSummary(0, 1, productSummaryRequestV2).getContent();
      if (CollectionUtils.isNotEmpty(summaryResponseV2)) {
        productSummaryResponseV2 = summaryResponseV2.get(0);
      }
    }
    List<CategoryResponse> categoriesData = null;
    List<ProductLevel3Logistics> productLevel3Logistics = null;
    ProfileResponse profileResponse = null;
    categoriesData = this.categoryRepository.findHierarchyByCategoryCode(categoryCode);
    productLevel3Logistics = new ArrayList<>();
    profileResponse = this.businessPartnerRepository.filterDetailByBusinessPartnerCode(merchantCode);
    if (org.apache.commons.lang3.StringUtils
      .isNotBlank(profileResponse.getCompany().getMerchantDeliveryType())) {
      productLevel3Logistics = productLevel3LogisticsService
        .findLogisticsByItemSku(itemSku, merchantCode,
          profileResponse.getCompany().getMerchantDeliveryType());
    }
    if (!productBusinessPartner.getState().equals(ACTIVE)) {
      return generateProductLevel3DetailsV2ByProductDetailResponse(productDetailResponse,
        categoriesData, productLevel3Logistics, profileResponse, productBusinessPartner, productSku,
        productCode, productCollection, concatValueAndValueTypes);
    } else {
      return generateProductLevel3V2Detail(productL3Response, categoriesData,
        productLevel3Logistics, profileResponse, productCollection, inventoryL3,
        productSummaryResponseV2, concatValueAndValueTypes);
    }
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse editPriceStockVariantsInfo(String storeId, ProductLevel3 productLevel3,
      ProductVariantUpdateRequest productVariantUpdateRequest, EditProductResponse editResponse)
    throws Exception {
    return productService
      .editPriceStockVariantsInfo(storeId, productLevel3, productVariantUpdateRequest,
        editResponse, true);
  }

  @Override
  public ProductVariantUpdateRequest toProductVariantUpdateRequest(
    ProductL3UpdateRequest l3UpdateRequest, EditProductResponse editResponse) {

    ProductVariantUpdateRequest productVariantUpdateRequest = new ProductVariantUpdateRequest();
    if (CollectionUtils.isEmpty(l3UpdateRequest.getProductItems())) {
      l3UpdateRequest.setProductItems(new ArrayList<>());
    }
    if (CollectionUtils.isEmpty(l3UpdateRequest.getAddPickupPoints())) {
      l3UpdateRequest.setAddPickupPoints(new ArrayList<>());
    }
    if (CollectionUtils.isEmpty(l3UpdateRequest.getDeletePickupPoints())) {
      l3UpdateRequest.setDeletePickupPoints(new ArrayList<>());
    }
    if (CollectionUtils.isEmpty(l3UpdateRequest.getProductBundleRecipe())) {
      l3UpdateRequest.setProductBundleRecipe(new ArrayList<>());
    }
    productVariantUpdateRequest
      .setCopyToAllVariantImages(toCopyToAllVariants(l3UpdateRequest.getCommonImages()));
    BeanUtils.copyProperties(l3UpdateRequest, productVariantUpdateRequest, "commonImages", "productItems");
    productVariantUpdateRequest.setProductItems(new ArrayList<>(l3UpdateRequest.getProductItems()));
    if (CollectionUtils.isNotEmpty(l3UpdateRequest.getAttributes())) {
      Map<String, String> attributeCodeAndName = l3UpdateRequest.getAttributes().stream().collect(
          toMap(ProductLevel3AttributeRequest::getAttributeCode, ProductLevel3AttributeRequest::getAttributeName,
              (a, b) -> b));
      editResponse.setAttributeCodeAndName(attributeCodeAndName);
    }
    if (overrideOnlineWithB2CFlag && (Boolean.FALSE.equals(l3UpdateRequest.getB2cActivated()))) {
      productVariantUpdateRequest.setOnline(l3UpdateRequest.getB2cActivated());
    }
    return productVariantUpdateRequest;
  }

  @Override
  public Page<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(String storeId,
    String username, String requestId, int page, int size,
    ItemL5ListingRequest itemL5ListingRequest, boolean needInventoryData) throws Exception {
    boolean needCorrection = false;
    GdnPreconditions.checkArgument(CommonUtils.isProductSku(itemL5ListingRequest.getProductSku()),
        ErrorMessages.PRODUCT_SKU_NOT_VALID);
    if (StringUtils.isNotBlank(itemL5ListingRequest.getItemSku())) {
      GdnPreconditions.checkArgument(CommonUtils.isItemSku(itemL5ListingRequest.getItemSku()),
          ErrorMessages.ITEM_SKU_NOT_VALID);
    }
    ProductBusinessPartner productBusinessPartner =
      productBusinessPartnerRepository.findFirstByGdnProductSku(itemL5ListingRequest.getProductSku());
    if(Objects.isNull(productBusinessPartner)){
      return Page.empty();
    }
    if (DELETED.equals(productBusinessPartner.getState())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ApiErrorCode.PRODUCT_DELETED_STATE.getDesc());
    } else {
      if (!productBusinessPartner.isMarkForDelete()) {
        needCorrection = NEED_CORRECTION.equals(productBusinessPartner.getState()) || IN_PROGRESS.equals(
            productBusinessPartner.getState());
      }
    }
    ItemPickupPointListingL3Request itemPickupPointListingL3Request =
      new ItemPickupPointListingL3Request();
    BeanUtils.copyProperties(itemL5ListingRequest, itemPickupPointListingL3Request);
    itemPickupPointListingL3Request.setNeedCorrection(needCorrection);
    return productL3Service.getItemPickupPointL3Listing(storeId, username, requestId, page, size,
      itemPickupPointListingL3Request, false, false, needInventoryData);
  }


  @Override
  public Page<ItemSummaryL4Response> getItemSummaryL4Response(String storeId, String productSku, int page, Integer size)
      throws Exception {
    ProductBusinessPartner productBusinessPartner = productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
    List<ItemBasicDetailV2Response> itemBasicDetailV2ResponseList;
    Page<ItemSkuToItemIdMapping> itemSkuToItemIdMappingPage = null;
    List<ItemSkuToItemIdMapping> itemSkuToItemIdMappings;
    Map<String, ItemBasicDetailV2Response> itemBasicDetailV2ResponseMap = new HashMap<>();
    Pageable pageable = null;
    if (Objects.nonNull(size)) {
      itemSkuToItemIdMappingPage = fetchL4BasedOnMfdInNrFlow ?
          productBusinessPartnerRepository.findItemSkuToItemIdMappingByProductBusinessPartnerIdAndMarkForDeleteFalse(
              storeId, productBusinessPartner.getId(), PageRequest.of(page, size)) :
          productBusinessPartnerRepository.findItemSkuToItemIdMappingByProductBusinessPartnerId(storeId,
              productBusinessPartner.getId(), PageRequest.of(page, size));
    } else {
      itemSkuToItemIdMappings = productBusinessPartnerRepository
          .findAllItemSkuToItemIdMappingByProductBusinessPartnerId(storeId, productBusinessPartner.getId());
      pageable = PageRequest.of(page, itemSkuToItemIdMappings.size());
      itemSkuToItemIdMappingPage = convertListToPage(itemSkuToItemIdMappings, pageable);
    }
    com.gdn.x.productcategorybase.dto.response.ProductDetailResponse productDetailResponse =
            productRepository.findDetailById(productBusinessPartner.getProductId());
    Map<String, ProductItemResponse> itemIdToProductItemResponseMap =
            productDetailResponse.getProductItemResponses().stream()
                    .collect(toMap(BaseDTOResponse::getId, Function.identity()));
    if (productBusinessPartner.isBundleProduct()) {
      Set<String> itemSkus = new HashSet<>();
      List<String> bundleRecipeStrings = productBusinessPartner.getProductItemBusinessPartners().stream()
          .map(ProductItemBusinessPartner::getBundleRecipe).filter(StringUtils::isNotBlank).collect(toList());
      bundleRecipeStrings.forEach(bundleRecipe -> {
        try {
          List<ProductBundleRecipe> productBundleRecipes = objectMapper.readValue(bundleRecipe, new TypeReference<>() {
          });
          itemSkus
              .addAll(productBundleRecipes.stream().map(ProductBundleRecipe::getItemSku).collect(Collectors.toSet()));
        } catch (JsonProcessingException e) {
          log.error("Error while deserialising bundle Recipe {} :  {} ", bundleRecipe, e);
          throw new ApplicationRuntimeException();
        }
      });
      itemBasicDetailV2ResponseList = xProductOutbound.getItemBasicDetailV2Response(new ArrayList<>(itemSkus), true);
      itemBasicDetailV2ResponseMap =
          itemBasicDetailV2ResponseList.stream().filter(Predicate.not(ItemBasicDetailV2Response::isPermanentDelete))
              .collect(Collectors.toMap(ItemBasicDetailV2Response::getItemSku, Function.identity(),
                  (oldValue, newValue) -> newValue));
    }
    return new PageImpl<>(ConverterUtil
        .toItemSummaryL4Response(productBusinessPartner, itemSkuToItemIdMappingPage.getContent(),
            itemIdToProductItemResponseMap, itemBasicDetailV2ResponseMap),
        PageRequest.of(page, itemSkuToItemIdMappingPage.getPageable().getPageSize()),
        itemIdToProductItemResponseMap.size());
  }

  public Page<ItemSkuToItemIdMapping> convertListToPage(List<ItemSkuToItemIdMapping> itemList, Pageable pageable) {
    int start = (int) pageable.getOffset();
    int end = Math.min((start + pageable.getPageSize()), itemList.size());
    return new PageImpl<>(itemList.subList(start, end), pageable, itemList.size());
  }

  private static List<ProductLevel3SummaryDetailsImageRequest> toCopyToAllVariants(
    List<ProductLevel3SummaryDetailsImageRequest> commonImages) {
    List<ProductLevel3SummaryDetailsImageRequest> imageRequestList = new ArrayList<>();
    for (ProductLevel3SummaryDetailsImageRequest imageRequest : commonImages) {
      ProductLevel3SummaryDetailsImageRequest detailsImageRequest =
        new ProductLevel3SummaryDetailsImageRequest();
      BeanUtils.copyProperties(imageRequest, detailsImageRequest);
      imageRequestList.add(detailsImageRequest);
    }
    return imageRequestList;
  }

  private static String getFilteredUSPAndDescription(String uniqueSellingPoint) {
    String uspWithoutTagsAndNewLine = getTextWithoutTags(PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE,
      getTextWithoutTags(PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT, uniqueSellingPoint,
        StringUtils.EMPTY), StringUtils.EMPTY);
    return getTextWithoutTags(PATTERN_FOR_EXTRA_SPACE, uspWithoutTagsAndNewLine, StringUtils.SPACE);
  }

  private static String getTextWithoutTags(Pattern pattern, String usp, String replace) {
    return pattern.matcher(usp).replaceAll(replace);
  }

  private void updateCampaignPrice(
    Map<String, QuickEditV2Request> salePriceChangeItemSummaryMap, String categoryCode) throws Exception {
    for (Map.Entry<String, QuickEditV2Request> quickEditV2RequestEntry :
      salePriceChangeItemSummaryMap.entrySet()) {
      this.updateOfferPriceInCampaign(
        quickEditV2RequestEntry.getValue().getItemSku(),
        quickEditV2RequestEntry.getValue().getPickupPointCode(),
        quickEditV2RequestEntry.getValue().getPrice().getSalePrice(),
        categoryCode);
    }
  }

  private Map<String, ItemSummaryListResponse> fetchAndGenerateItemSummaryListResponseMap(
    List<QuickEditV2Request> quickEditV2Requests) {
    List<ItemSummaryListResponse> itemSummaryListResponseList =
      xProductOutbound.getItemPickupPointSummary(quickEditV2Requests.stream().map(
        quickEditV2Request -> new ItemPickupPointRequest(quickEditV2Request.getItemSku(),
          quickEditV2Request.getPickupPointCode())).collect(toList()), Constants.ALL);
    validateL5SummaryResponse(quickEditV2Requests, itemSummaryListResponseList);
    return itemSummaryListResponseList.stream().collect(Collectors.toMap(
      itemSummaryListResponse -> CommonUtils.toL5Id(itemSummaryListResponse.getItemSku(),
        itemSummaryListResponse.getPickupPointCode()), Function.identity()));
  }

  private static void validateL5SummaryResponse(List<QuickEditV2Request> quickEditV2Requests,
    List<ItemSummaryListResponse> itemSummaryListResponseList) {
    if (CollectionUtils.isEmpty(itemSummaryListResponseList)) {
      Set<String> offlineItemIds =
        quickEditV2Requests.stream().map(QuickEditV2Request::getItemPickupPointId)
          .collect(Collectors.toSet());
      log.error("itemSummaryListResponseList was found to be empty for offlineItemIds : {} ",
        offlineItemIds);
      throw new ApiIncorrectInputDataException(
        String.format(ErrorMessages.L5_DETAILS_NOT_VALID, offlineItemIds),
        ApiErrorCode.ITEM_PICKUP_POINT_NOT_FOUND);
    }
  }

  private void updateItemPickupPointListing(
    Map<String, ItemSummaryListResponse> itemSummaryListResponseMap,
    Map<String, Boolean> wholeSaleActivatedMap,
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request, String productSku,
    String storeId, ProfileResponse businessPartner) throws Exception {
    List<ItemPickupPointQuickEditRequest> itemPickupPointQuickEditRequests =
      new ArrayList<>();
    List<ProductLevel3Inventory> inventoryData = new ArrayList<>();
    List<String> stockUpdatedL4s = new ArrayList<>();
    ItemSummaryListResponse firstItemSummaryListResponse = itemSummaryListResponseMap.get(
      productLevel3QuickEditV2Request.getQuickEditV2Requests().get(0).getItemPickupPointId());
    ProductType productType = firstItemSummaryListResponse.getProductType();
    Map<String, ProductLevel3Summary> productLevel3SummaryMap = new HashMap<>();
    Map<String, ProductLevel3Inventory> inventoryMap =
      getInventoryMap(productLevel3QuickEditV2Request, businessPartner, inventoryData);
    for (QuickEditV2Request quickEditV2Request : productLevel3QuickEditV2Request.getQuickEditV2Requests()) {
      ItemSummaryListResponse itemSummaryListResponse =
        itemSummaryListResponseMap.get(quickEditV2Request.getItemPickupPointId());
      if (Objects.nonNull(itemSummaryListResponse)) {
        quickEditV2Request.setFbbActivated(itemSummaryListResponse.isFbbActivated());
        productLevel3SummaryMap.put(CommonUtils.toL5Id(itemSummaryListResponse.getItemSku(), itemSummaryListResponse
                .getPickupPointCode()), generateProductLevel3SummaryFromItemPickupPoint(itemSummaryListResponse.getMerchantCode(),
            itemSummaryListResponse, businessPartner));
        Boolean wholesaleActivated =
          validateAndUpdateWholesaleFlagForL5(storeId, quickEditV2Request, itemSummaryListResponse);
        if (cncForWarehouseFeatureSwitch) {
          validateCncStatus(quickEditV2Request);
        }
        if (productLevel3Helper.isProductItemDetailChangedForL5ListingUpdate(quickEditV2Request,
          itemSummaryListResponse, wholesaleActivated)) {
          ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
            ConverterUtil.toItemPickupPointListingUpdateRequestVo(quickEditV2Request);
          if (!cncForWarehouseFeatureSwitch || StringUtils.isBlank(itemPickupPointQuickEditRequest.getCncStatus())) {
            itemPickupPointQuickEditRequest.setCncStatus(itemPickupPointQuickEditRequest.isCncActivated() ?
                ProductLevel3Status.ONLINE.name() :
                ProductLevel3Status.OFFLINE.name());
          }
          itemPickupPointQuickEditRequest.setWholeSaleActivated(wholesaleActivated);
          CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse,
            itemPickupPointQuickEditRequest, cncForWarehouseFeatureSwitch);
          itemPickupPointQuickEditRequests.add(itemPickupPointQuickEditRequest);
          wholeSaleActivatedMap.put(CommonUtils.toL5Id(quickEditV2Request.getItemSku(),
              quickEditV2Request.getPickupPointCode()), wholesaleActivated);
        }
      }
    }
    Set<String> uniquePickupPointCodes = new HashSet<>();
    if (validateEditPickupPoints) {
      for (QuickEditV2Request quickEditV2Request : productLevel3QuickEditV2Request
        .getQuickEditV2Requests()) {
        uniquePickupPointCodes.add(quickEditV2Request.getPickupPointCode());
      }
      List<PickupPointResponse> pickupPointResponseList = pickupPointOutbound
        .getByPickupPointCodes(Constants.DEFAULT_REQUEST_ID,
          new ArrayList<>(uniquePickupPointCodes));
      CommonUtils.validatePickupPoints(productSku, uniquePickupPointCodes, pickupPointResponseList,
        new SimpleStringResponse(), businessPartner.getBusinessPartnerCode());
      overrideStatusForDistributionL5(productLevel3QuickEditV2Request, pickupPointResponseList);
    }
    for (QuickEditV2Request editV2Request : productLevel3QuickEditV2Request.getQuickEditV2Requests()) {
      try {
        updateInventoryData(itemSummaryListResponseMap, businessPartner, editV2Request, inventoryMap);
        stockUpdatedL4s.add(editV2Request.getItemPickupPointId());
      } catch (ApplicationRuntimeException e) {
        log.error("Error updating stock status from listing for request : {} ", editV2Request);
        throw e;
      } catch (Exception e) {
        log.error("Error updating stock status from listing for request : {} , success List : {} "
          + "with Error : ", editV2Request, stockUpdatedL4s, e);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          String.format(ErrorMessages.ERROR_STOCK_UPDATE_FAILED,
            StringUtils.join(editV2Request.getItemPickupPointId(), Constants.COMMA)));
      }
    }
    if (CollectionUtils.isNotEmpty(itemPickupPointQuickEditRequests)) {
      xProductOutbound.updateItemPickupPointListing(productSku, productType,
        itemPickupPointQuickEditRequests);
      if (Objects.nonNull(itemPickupPointQuickEditRequests.get(0).getOff2OnActiveFlag())) {
        if (!Boolean.valueOf(firstItemSummaryListResponse.isOff2OnChannelActive())
          .equals(itemPickupPointQuickEditRequests.get(0).getOff2OnActiveFlag())) {
          updateOff2OnActiveFlag(firstItemSummaryListResponse.getMerchantCode(),
            firstItemSummaryListResponse.getProductSku(), firstItemSummaryListResponse.getProductName(),
            itemPickupPointQuickEditRequests.get(0).getOff2OnActiveFlag());
        }
      }
    }

    for (QuickEditV2Request quickEditV2Request : productLevel3QuickEditV2Request.getQuickEditV2Requests()) {
      ItemSummaryListResponse itemSummaryListResponse =
        itemSummaryListResponseMap.get(quickEditV2Request.getItemPickupPointId());
      if (Objects.nonNull(itemSummaryListResponse)) {
        generateProductHistoryForSummary(itemSummaryListResponse, wholeSaleActivatedMap.get(
                CommonUtils.toL5Id(quickEditV2Request.getItemSku(), quickEditV2Request.getPickupPointCode())),
            productLevel3SummaryMap.get(
                CommonUtils.toL5Id(itemSummaryListResponse.getItemSku(), itemSummaryListResponse.getPickupPointCode())),
            quickEditV2Request.isScheduleUpdate());
        ItemViewConfigDTO defaultItemViewConfigDTO =
          itemSummaryListResponse.getItemViewConfigs().stream().filter(itemViewConfigDTO1 -> Constants.DEFAULT.equals(itemViewConfigDTO1.getChannel()))
              .findFirst().orElse(new ItemViewConfigDTO());
        ItemViewConfigDTO cncItemViewConfigDTO =
            itemSummaryListResponse.getItemViewConfigs().stream().filter(itemViewConfigDTO1 -> Constants.CNC_CHANNEL.equals(itemViewConfigDTO1.getChannel()))
                .findFirst().orElse(new ItemViewConfigDTO());

        if (ConverterUtil.isProductStatusChange(defaultItemViewConfigDTO.isBuyableOriginal(),
            defaultItemViewConfigDTO.isDiscoverableOriginal(), quickEditV2Request.getStatus().name()) ||
            (cncForWarehouseFeatureSwitch && Objects.nonNull(quickEditV2Request.getCncStatus()) &&
                ConverterUtil.isProductStatusChange(
                    cncItemViewConfigDTO.isBuyableOriginal(), cncItemViewConfigDTO.isDiscoverableOriginal(),
                    quickEditV2Request.getCncStatus().name()))) {
          ProductLevel3Status notificationStatus = ProductLevel3Status.OFFLINE.equals(quickEditV2Request.getStatus()) ?
              quickEditV2Request.getCncStatus() :
              quickEditV2Request.getStatus();
          productNotificationService.sendNotificationForProductStatus(businessPartner.getBusinessPartnerCode(),
              itemSummaryListResponse.getItemName(), notificationStatus.name());
        }
      }
    }
  }

  private void overrideStatusForDistributionL5(ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request,
      List<PickupPointResponse> pickupPointResponseList) {
    if (ranchIntegrationEnabled) {
      List<String> distributionTruePickupPointCodes =
          Optional.ofNullable(pickupPointResponseList).orElse(new ArrayList<>()).stream().filter(
                  pickupPointResponse -> Boolean.TRUE.equals(
                      Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
                          .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))).map(PickupPointResponse::getCode)
              .toList();
      productLevel3QuickEditV2Request.getQuickEditV2Requests().stream()
          .filter(editV2Request -> distributionTruePickupPointCodes.contains(editV2Request.getPickupPointCode()))
          .forEach(editV2Request -> {
            editV2Request.setStatus(ProductLevel3Status.OFFLINE);
            editV2Request.setCncStatus(ProductLevel3Status.OFFLINE);
          });
    }
  }

  private Map<String, ProductLevel3Inventory> getInventoryMap(
    ProductLevel3QuickEditV2Request productLevel3QuickEditV2Request,
    ProfileResponse businessPartner, List<ProductLevel3Inventory> inventoryData) throws Exception {
    if (mergeStockValueAndSyncStockUpdate) {
      List<List<QuickEditV2Request>> editRequestsBatch =
        Lists.partition(productLevel3QuickEditV2Request.getQuickEditV2Requests(),
          applicationProperties.getInventoryApiBatchSize());
      for (List<QuickEditV2Request> editRequest : editRequestsBatch) {
        inventoryData.addAll(
          this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
            CommonUtils.getInventoryDetailInfoRequestDTOList(editRequest,
              businessPartner.getBusinessPartnerCode())));
      }
      return inventoryData.stream()
        .collect(Collectors.toMap(inventory -> CommonUtils.toL5Id(inventory.getWebItemSku(),
            inventory.getWebPickupPointCode()),
          Function.identity()));
    }
    return new HashMap<>();
  }

  private void updateInventoryData(Map<String, ItemSummaryListResponse> itemSummaryListResponseMap,
    ProfileResponse businessPartner, QuickEditV2Request quickEditV2Request,
    Map<String, ProductLevel3Inventory> inventoryMap) throws Exception {
    ItemSummaryListResponse itemSummaryListResponse =
      itemSummaryListResponseMap.getOrDefault(quickEditV2Request.getItemPickupPointId(), null);
    boolean syncStockUpdated = Optional.ofNullable(quickEditV2Request.getUseWarehouseStock())
      .filter(useWarehouseStock -> !useWarehouseStock.equals(inventoryMap.getOrDefault(
        CommonUtils.toL5Id(quickEditV2Request.getItemSku(),
          quickEditV2Request.getPickupPointCode()), new ProductLevel3Inventory()).isWebSyncStock()))
      .isPresent();

    boolean mergedStockUpdate =
      mergeStockValueAndSyncStockUpdate(quickEditV2Request, itemSummaryListResponse,
        businessPartner , syncStockUpdated);
    if (!mergedStockUpdate) {
      validateAndUpdateStockAndPickupPointForL5(quickEditV2Request, itemSummaryListResponse,
        businessPartner, syncStockUpdated);
    }
  }

  private void validateCncStatus(QuickEditV2Request quickEditV2Request) {
    // If both cnc and delivery status are != OFFLINE but they don't have same value, move CNC to offline
    if (ProductLevel3Status.OFFLINE.equals(quickEditV2Request.getCncStatus()) || ProductLevel3Status.OFFLINE.equals(
        quickEditV2Request.getStatus())) {
      return;
    }
    if (!quickEditV2Request.getStatus().equals(quickEditV2Request.getCncStatus())) {
      log.error(
          "Product's cnc and delivery view config combination is not allowed, item details - {} , delivery status : {}, cnc status: {}",
          quickEditV2Request.getItemPickupPointId(), quickEditV2Request.getStatus(), quickEditV2Request.getCncStatus());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INVALID_DELIVERY_CNC_VIEW_CONFIG_STATES.getDesc());
    }
  }

  private ProductLevel3Summary generateProductLevel3SummaryFromItemPickupPoint(String merchantCode,
    ItemSummaryListResponse itemSummaryListResponse, ProfileResponse businessPartner) throws Exception {
    List<CategoryResponse> categoryData = this.categoryRepository.findHierarchyByCategoryCode(
      itemSummaryListResponse.getMasterCategoryCode());
    PickupPointResponse pickupPointData =
      this.pickupPointRepository.findByPickupPointCode(itemSummaryListResponse.getPickupPointCode());
    List<ProductLevel3Inventory> inventoryList =
      this.productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
          CommonUtils.toSingleInventoryDetailInfoRequestDTOList(merchantCode,
            itemSummaryListResponse.getItemSku(), itemSummaryListResponse.getPickupPointCode()));
    if (CollectionUtils.isEmpty(inventoryList)) {
      log.error(
        "Not found inventory data for businessPartnerCode: {}, gdnSku: {}, pickupPointCode:{},  "
          + "itemSummaryListResponse: {}", merchantCode,
        itemSummaryListResponse.getItemSku(), itemSummaryListResponse.getPickupPointCode(),
        itemSummaryListResponse);
    }
    return CommonUtils.generateProductLevel3SummarySingle(itemSummaryListResponse, categoryData, pickupPointData,
        Optional.ofNullable(inventoryList).orElse(new ArrayList<>()).stream().findFirst().orElse(null),
        businessPartner);
  }

  private Boolean validateAndUpdateWholesaleFlagForL5(String storeId,
    QuickEditV2Request quickEditV2Request, ItemSummaryListResponse itemSummaryListResponse)
    throws Exception {
    QuickEditV2Request partialPriceData = new QuickEditV2Request();
    WholesalePriceSkuResponse wholesalePriceSkuResponse =
      productPricingOutbound.getWholesalePriceByItemSkuAndPickupPointCode(Collections.singletonList(
        ItemInfoDto.builder().itemSku(quickEditV2Request.getItemSku())
          .pickupPointCode(quickEditV2Request.getPickupPointCode()).itemPickupPointId(
            CommonUtils.toL5Id(quickEditV2Request.getItemSku(),
              quickEditV2Request.getPickupPointCode())).build())).get(0);
    if (Objects.isNull(wholesalePriceSkuResponse)) {
      return false;
    }
    if(Objects.isNull(quickEditV2Request.getPrice())){
      ConverterUtil.setOfferPriceForPartialUpdate(partialPriceData, itemSummaryListResponse, new ItemRequest());
    }
    else{
      partialPriceData.setPrice(quickEditV2Request.getPrice());
    }
    if (Boolean.TRUE.equals(quickEditV2Request.getWholeSaleActivated())) {
      ItemRequest itemRequest = new ItemRequest();
      itemRequest.setWholesalePriceActivated(quickEditV2Request.getWholeSaleActivated());
      itemRequest.setPrice(ConverterUtil.setOfferPrice(
        Collections.singletonList(partialPriceData.getPrice())));
      List<ProductItemWholesalePriceRequest> productItemWholesalePriceRequests =
        wholesalePriceSkuResponse.getWholesaleRules().keySet().stream().map(
            key -> new ProductItemWholesalePriceRequest(key, wholesalePriceSkuResponse.getWholesaleRules().get(key)))
          .collect(toList());
      wholesaleValidationUtil
        .validateWholesaleConfigOnUpdate(itemSummaryListResponse.getMasterCategoryCode(),
          productItemWholesalePriceRequests, itemRequest, getMinimumPrice(storeId), quickEditV2Request.getItemSku(),
          quickEditV2Request.getWholeSaleActivated(), null);
      updateWholesaleFlagInPricing(itemSummaryListResponse.getItemSku(), itemRequest.getWholesalePriceActivated(),
        wholesalePriceSkuResponse, itemSummaryListResponse.getPickupPointCode());
      return itemRequest.getWholesalePriceActivated();
    } else {
      updateWholesaleFlagInPricing(itemSummaryListResponse.getItemSku(), quickEditV2Request.getWholeSaleActivated(),
        wholesalePriceSkuResponse, itemSummaryListResponse.getPickupPointCode());
      return quickEditV2Request.getWholeSaleActivated();
    }
  }


  private void validateVersionAndPriceForListingUpdate(QuickEditV2Request quickEditV2Request,
    ItemSummaryListResponse itemSummaryResponse,
    Map<String, QuickEditV2Request> salePriceChangeItemSummaryMap) throws Exception {
    if (Objects.nonNull(itemSummaryResponse.getVersion()) && Objects.nonNull(
      quickEditV2Request.getVersion())
      && quickEditV2Request.getVersion() <= itemSummaryResponse.getVersion()) {
      log.error("Version mismatch error. oldVersion : {}, newVersion : {} ",
        itemSummaryResponse.getVersion(), quickEditV2Request.getVersion());
      throw new ApplicationRuntimeException(ErrorCategory.INVALID_STATE,
        ErrorMessages.INVALID_VERSION_ERROR);
    }
    if ((itemSummaryResponse.isFreeSample()) && (!ProductLevel3Status.OFFLINE.equals(quickEditV2Request.getStatus())
        || Boolean.TRUE.equals(quickEditV2Request.getOff2OnActiveFlag()) || !ProductLevel3Status.OFFLINE.equals(
        quickEditV2Request.getCncStatus()))) {
      log.error("Error updating free sample product : {} ", itemSummaryResponse.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET.getDesc());
    }
    Double minimumPrice = getMinimumPrice(Constants.DEFAULT_STORE_ID).doubleValue();
    if (Double.compare(quickEditV2Request.getPrice().getSalePrice(), minimumPrice) < 0
        || Double.compare(quickEditV2Request.getPrice().getPrice(), minimumPrice) < 0) {
      log.error("Price lesser than allowed minimum price : {}, itemSku : {} ", minimumPrice,
          quickEditV2Request.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.MINIMUM_PRICE_VALUE_INVALID.getDesc() + minimumPrice);
    }
    if (Double.compare(quickEditV2Request.getPrice().getSalePrice(), quickEditV2Request.getPrice().getPrice()) == 1) {
      log.error("sale price : {} is less than regular price : {}", quickEditV2Request.getPrice().getSalePrice(),
          quickEditV2Request.getPrice().getPrice());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.INVALID_SALE_PRICE.getDesc());
    }
    if (Objects.nonNull(quickEditV2Request.getB2bFieldsRequest()) && Objects.nonNull(
        quickEditV2Request.getB2bFieldsRequest().getBasePrice())
        && Double.compare(quickEditV2Request.getB2bFieldsRequest().getBasePrice(), minimumPrice) < 0) {
      log.error("Base Price lesser than allowed minimum price : {}, itemSku : {} ", minimumPrice,
          quickEditV2Request.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.MINIMUM_PRICE_VALUE_INVALID.getDesc() + minimumPrice);
    }
    if (isPriceEditNotAllowed(itemSummaryResponse.getPrice(),
      quickEditV2Request.getPrice().getSalePrice(), itemSummaryResponse)) {
      log.error("Price Edit is not allowed for L5 : {} ",
        itemSummaryResponse.getItemSku() + Constants.HYPHEN + itemSummaryResponse.getPickupPointCode());
    }
    if (isSalesPriceChangedOrPriceEditDisabled(itemSummaryResponse.getPrice(),
      quickEditV2Request.getPrice().getSalePrice(),
      CommonUtils.isPriceEditDisabled(itemSummaryResponse.isMerchantPromoDiscount(),
        itemSummaryResponse.getPrice()))) {
      try {
        this.validateDiscountPrice(quickEditV2Request.getItemSku(),
          quickEditV2Request.getPrice().getSalePrice(), quickEditV2Request.getPickupPointCode(),
          itemSummaryResponse.getMasterCategoryCode());
        salePriceChangeItemSummaryMap.put(quickEditV2Request.getItemPickupPointId(),
          quickEditV2Request);
      } catch (ApplicationRuntimeException ex) {
        log.error(
          "Error on validate of price lock feature for campaign, item : {}, error : {}, error - ",
          quickEditV2Request.getItemSku(), ex.getErrorMessage(), ex);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.PRICE_UPDATE_FAILED.getDesc());
      }
    }
  }

  private void updateOfferPriceInCampaign(String itemSku, String pickupPointCode,
    Double salePrice, String masterCategoryCode) throws Exception {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    UpdateDiscountDTO updateDiscountDTO = new UpdateDiscountDTO();
    updateDiscountDTO.setCategoryCode(masterCategoryCode);
    updateDiscountDTO.setItemSku(itemSku);
    updateDiscountDTO.setSellingPrice(salePrice);
    updateDiscountDTO.setPickUpPointCode(pickupPointCode);
    campaignUpdateDiscountRequest.setDiscountDTOList(Collections.singletonList(updateDiscountDTO));
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
      campaignOutbound.validateAndUpdateDiscountPrice(true, campaignUpdateDiscountRequest);
    if (MapUtils.isNotEmpty(campaignUpdateDiscountResponse.getItemSkuStatusMap()) && campaignUpdateDiscountResponse
      .getItemSkuStatusMap().containsKey(itemSku)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        campaignUpdateDiscountResponse.getItemSkuStatusMap().get(itemSku));
    }
  }

  private void validateDiscountPrice(String itemSku, Double offerPrice,
    String pickupPointCode, String categoryCode) throws Exception {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    UpdateDiscountDTO updateDiscountDTO = new UpdateDiscountDTO();
    updateDiscountDTO.setCategoryCode(categoryCode);
    updateDiscountDTO.setItemSku(itemSku);
    updateDiscountDTO.setSellingPrice(offerPrice);
    updateDiscountDTO.setPickUpPointCode(pickupPointCode);
    campaignUpdateDiscountRequest.setDiscountDTOList(Collections.singletonList(updateDiscountDTO));
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse =
      campaignOutbound.validateDiscountPrice(true, campaignUpdateDiscountRequest);
    if (MapUtils.isNotEmpty(
      campaignUpdateDiscountResponse.getItemSkuStatusMap())
      && campaignUpdateDiscountResponse.getItemSkuStatusMap().containsKey(itemSku)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        campaignUpdateDiscountResponse.getItemSkuStatusMap().get(itemSku));
    }
  }

  private void validateAndUpdateStockAndPickupPointForL5(QuickEditV2Request quickEditRequest,
      ItemSummaryListResponse savedProductData, ProfileResponse profileResponse,
    boolean syncStockUpdated) throws Exception {
    if (Objects.nonNull(quickEditRequest.getUseWarehouseStock()) && GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI.equals(
        profileResponse.getCompany().getInventoryFulfillment()) && syncStockUpdated) {
      this.productLevel3InventoryService.updateSyncStockOrInsertStock(
          RequestHelper.getSyncStockUpdateVo(mppForWhEnabled, profileResponse.getBusinessPartnerCode(), profileResponse,
              savedProductData, quickEditRequest,null));
    }
    if (Objects.nonNull(quickEditRequest.getDeltaStock())) {
      GdnPreconditions.checkArgument(quickEditRequest.getDeltaStock() <= maxStockLimit,
          String.format(ErrorMessages.MAXIMUM_STOCK_LIMIT_EXCEEDED_ERROR, maxStockLimit));
      this.productLevel3InventoryService.updateOrInsertStock(
          RequestHelper.getUpdateOrInsertStockVo(mppForWhEnabled, profileResponse.getBusinessPartnerCode(),
              profileResponse, savedProductData, quickEditRequest));
    }
  }

  private boolean mergeStockValueAndSyncStockUpdate(QuickEditV2Request quickEditV2Request,
    ItemSummaryListResponse savedProductData, ProfileResponse profileResponse,
    boolean syncStockUpdated) throws Exception {
    if(shouldMergeStockValueAndSyncStockUpdate(quickEditV2Request, profileResponse, syncStockUpdated)){
      this.productLevel3InventoryService.updateSyncStockOrInsertStock(
        RequestHelper.getSyncStockUpdateVo(mppForWhEnabled, profileResponse.getBusinessPartnerCode(), profileResponse,
          savedProductData, quickEditV2Request ,
          Optional.ofNullable(quickEditV2Request.getDeltaStock()).orElse(Constants.ZERO)));
      return true;
    }
    return false;
  }

  public boolean shouldMergeStockValueAndSyncStockUpdate(QuickEditV2Request quickEditV2Request,
    ProfileResponse profileResponse, boolean syncStockUpdated) {
    boolean syncStockUpdate = Objects.nonNull(quickEditV2Request.getUseWarehouseStock())
      && GdnBaseLookup.INVENTORY_FULFILLMENT_BLIBLI.equals(
      profileResponse.getCompany().getInventoryFulfillment()) && syncStockUpdated;
    return syncStockUpdate && Objects.nonNull(quickEditV2Request.getDeltaStock())
      && mergeStockValueAndSyncStockUpdate;
  }

  private void updateOff2OnActiveFlag(String merchantCode, String productSku, String productName,
    Boolean off2OnActiveFlag) throws Exception {
    Map<String, Boolean> productSkuOff2OnMap = new HashMap<>();
    productSkuOff2OnMap.put(productSku, off2OnActiveFlag);
    xProductOutbound.updateOff2OnActiveFlagByProductSku(productSkuOff2OnMap);
    updatedProductHistoryService.createProductL3AuditLog(merchantCode, Constants.DEFAULT, productSku, productName,
      UpdateProductActivity.OFFLINE_TO_ONLINE.getDesc(), String.valueOf(!off2OnActiveFlag.booleanValue()),
      String.valueOf(off2OnActiveFlag.booleanValue()), false, Constants.HYPHEN);
  }

  private void generateProductHistoryForSummary(ItemSummaryListResponse savedProductData, Boolean wholeSaleFlagChange,
      ProductLevel3Summary savedProductLevel3Summary, boolean scheduleUpdate) throws Exception {
    ItemSummaryListResponse updatedProductData = this.xProductOutbound.getItemPickupPointSummary(
      Collections.singletonList(new ItemPickupPointRequest(savedProductData.getItemSku(),
        savedProductData.getPickupPointCode())), Constants.ALL).get(0);
    ProductLevel3Summary updatedProductLevel3Summary =
      generateProductLevel3SummaryFromItemPickupPoint(savedProductData.getMerchantCode(),
        updatedProductData, null);
    if (Objects.nonNull(wholeSaleFlagChange)) {
      updatedProductLevel3Summary.setWholesalePriceActivated(wholeSaleFlagChange);
    }
    UpdateProductItemLevel3Model savedData =
      updateProductItemLevel3ModelConverter.convertFromProductLevel3Summary(
        savedProductLevel3Summary);
    UpdateProductItemLevel3Model updatedData =
      updateProductItemLevel3ModelConverter.convertFromProductLevel3Summary(
        updatedProductLevel3Summary);
    updatedData.setOff2OnActiveFlag(savedData.getOff2OnActiveFlag());
    this.updatedProductHistoryService.saveUpdateProductLevel3Audit(
      savedProductData.getMerchantCode(), savedProductData.getItemSku(), savedData, updatedData,
      Constants.DEFAULT, savedProductData.getProductSku(), savedProductData.getItemName(), false, StringUtils.EMPTY);
    if (schedulesAddEditEnabled && scheduleUpdate) {
      updateProductHistoryForBuyableAndDiscoverableScheduleChange(savedProductData, updatedProductData);
    }
  }

  public void updateProductHistoryForBuyableAndDiscoverableScheduleChange(ItemSummaryListResponse savedProductData,
      ItemSummaryListResponse updatedProductData) throws Exception {
    ItemBuyableScheduleDTO oldItemBuyableScheduleDTO = getItemBuyableScheduleDTO(savedProductData.getItemViewConfigs());
    ItemBuyableScheduleDTO newItemBuyableScheduleDTO =
        getItemBuyableScheduleDTO(updatedProductData.getItemViewConfigs());
    if (CommonUtils.isBuyableScheduleChanged(oldItemBuyableScheduleDTO, newItemBuyableScheduleDTO)) {
      String oldBuyableScheduleValue =
          oldItemBuyableScheduleDTO.getStartDateTime() + Constants.SPACE + Constants.HYPHEN + Constants.SPACE
              + oldItemBuyableScheduleDTO.getEndDateTime();
      updatedProductHistoryService.createProductL3AuditLog(updatedProductData.getMerchantCode(),
          updatedProductData.getItemSku(), updatedProductData.getProductSku(), updatedProductData.getItemName(),
          UpdateProductActivity.BUYABLE_SCHEDULE.getDesc(), oldBuyableScheduleValue, Constants.HYPHEN, false,
          updatedProductData.getPickupPointCode());
    }
    ItemDiscoverableScheduleDTO oldItemDiscoverableScheduleDTO =
        getItemDiscoverableScheduleDTO(savedProductData.getItemViewConfigs());
    ItemDiscoverableScheduleDTO newItemDiscoverableScheduleDTO =
        getItemDiscoverableScheduleDTO(updatedProductData.getItemViewConfigs());
    if (CommonUtils.isDiscoverableScheduleChanged(oldItemDiscoverableScheduleDTO, newItemDiscoverableScheduleDTO)) {
      String oldDiscoverableScheduleValue =
          oldItemDiscoverableScheduleDTO.getStartDateTime() + Constants.SPACE + Constants.HYPHEN + Constants.SPACE
              + oldItemDiscoverableScheduleDTO.getEndDateTime();
      updatedProductHistoryService.createProductL3AuditLog(updatedProductData.getMerchantCode(),
          updatedProductData.getItemSku(), updatedProductData.getProductSku(), updatedProductData.getItemName(),
          UpdateProductActivity.DISCOVERABLE_SCHEDULE.getDesc(), oldDiscoverableScheduleValue, Constants.HYPHEN, false,
          updatedProductData.getPickupPointCode());
    }
  }

  public ItemBuyableScheduleDTO getItemBuyableScheduleDTO(Set<ItemViewConfigDTO> itemViewConfigDTOSet) {
    if (CollectionUtils.isEmpty(itemViewConfigDTOSet)) {
      return null;
    }
    return itemViewConfigDTOSet.stream().filter(Objects::nonNull)
        .filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannel())).findFirst()
        .map(ItemViewConfigDTO::getItemBuyableSchedules).orElse(null);
  }

  public ItemDiscoverableScheduleDTO getItemDiscoverableScheduleDTO(Set<ItemViewConfigDTO> itemViewConfigDTOSet) {
    if (CollectionUtils.isEmpty(itemViewConfigDTOSet)) {
      return null;
    }
    return itemViewConfigDTOSet.stream().filter(Objects::nonNull)
        .filter(viewConfig -> Constants.DEFAULT.equals(viewConfig.getChannel())).findFirst()
        .map(ItemViewConfigDTO::getItemDiscoverableSchedules).orElse(null);
  }

  private boolean isSalesPriceChangedOrPriceEditDisabled(Set<PriceDTO> prices, Double salesPrice,
    boolean priceEditDisabled) {
    return (prices.stream().noneMatch(price -> salesPrice.equals(price.getOfferPrice()))
      && !priceEditDisabled);
  }

  private boolean isPriceEditNotAllowed(Set<PriceDTO> prices, Double salesPrice,
    ItemSummaryListResponse itemSummaryResponse) {
    if (prices.stream().noneMatch(priceDTO -> salesPrice.equals(priceDTO.getOfferPrice()))
      && returnImmediatelyIfPriceEditNotAllowed) {
      return CommonUtils.isPriceEditDisabled(itemSummaryResponse.isMerchantPromoDiscount(), prices);
    }
    return false;
  }


  private void updateWholesaleFlagInPricing(String itemSku, Boolean response,
      WholesalePriceSkuResponse wholesalePriceSkuResponse, String pickupPointCode) throws Exception {
    if (checkIsStatusChanged(response, wholesalePriceSkuResponse.getSkuStatus())) {
      productPricingOutbound.setWholesaleActivatedFlag(itemSku, pickupPointCode,
        Boolean.TRUE.equals(response) ? Constants.ACTIVE_STATUS : Constants.INACTIVE_STATUS);
    }
  }

  private boolean checkIsStatusChanged(Boolean flag, String status) {
    return (Constants.INACTIVE_STATUS.equals(status) && Boolean.TRUE.equals(flag)) || (
      Constants.ACTIVE_STATUS.equals(status) && Boolean.FALSE.equals(flag));
  }

  private Integer getMinimumPrice(String storeId) {
    ProductSystemParameter minimumPrice =
      productSystemParameterService.findByStoreIdAndVariable(storeId, Constants.MINIMUM_PRICE);
    return Integer.parseInt(minimumPrice.getValue());
  }

  private Map<String, String> fetchProductSkuToMasterCategoryMap(
      List<ProductL3SummaryResponse> summaryResponseList) {
    Map<String, String> categoryMap = new HashMap<>();
    Set<String> categoryCodeSet = summaryResponseList.stream().filter(
        productL3SummaryResponse -> Objects.nonNull(productL3SummaryResponse.getCategoryCode()))
        .map(ProductL3SummaryResponse::getCategoryCode).collect(Collectors.toSet());
    CategoryMultipleIdRequest categoryNameRequest = new CategoryMultipleIdRequest();
    categoryNameRequest.setCategoryCode(new ArrayList<>(categoryCodeSet));
    CategoryNamesResponse categoryResponse = productOutbound.fetchCategoryNamesResponse(categoryNameRequest, 0, Constants.CATEGORY_PAGE_SIZE);
    summaryResponseList.forEach(productResponse -> categoryMap.put(productResponse.getProductSku(),
        categoryResponse.getCategoryMap().get(productResponse.getCategoryCode())));
    return categoryMap;
  }

  private Map<String, String> fetchProductSkuToSuspensionReasonMap(String storeId, List<String> productSkuList) {
    List<ProductSuspensionHistoryResponse> suspensionResponseList =
        productLevel3Service.getProductSuspensionHistoryByProductSkus(storeId, productSkuList);
    return suspensionResponseList.stream().collect(Collectors.toMap(ProductSuspensionHistoryResponse::getProductSku,
            ProductSuspensionHistoryResponse::getReason));
  }

  private Map<String, InventoryStockInfoDTO> fetchProductSkuToL3InventoryDetailMap(
      List<String> productSkuList, String merchantCode) throws Exception {
    List<List<String>> productSkuPartition =
        ListUtils.partition(productSkuList, applicationProperties.getInventoryApiBatchSize());
   InventoryDetailStockInfoRequestDTO inventoryDetailStockInfoRequestDTO =
     new InventoryDetailStockInfoRequestDTO();
   inventoryDetailStockInfoRequestDTO.setWebMerchantCode(merchantCode);
    List<InventoryStockInfoDTO> inventoryDTOList = new ArrayList<>();
    for (List<String> productSkus : productSkuPartition) {
      inventoryDetailStockInfoRequestDTO.setWebProductSkuList(productSkus);
      GdnRestListResponse<InventoryStockInfoDTO> inventoryL3Response =
        inventoryOutbound.findDetailByWebProductSkus(OfflineItemInventoryUtil.generateMandatoryRequestParam(),
          inventoryDetailStockInfoRequestDTO);
      inventoryDTOList.addAll(inventoryL3Response.getContent());
    }
    return inventoryDTOList.stream()
        .collect(Collectors.toMap(InventoryStockInfoDTO::getWebProductSku, Function.identity()));
  }

  private void fetchL5IdToInventoryAndCampaignDetailsMap(
      List<ProductL3SummaryResponse> productSummaryList,
      Map<String, InventoryDetailInfoResponseV2DTO> itemInventoryMapL5,
      Map<String, CampaignPriceSkuResponse> itemCampaignMap) throws Exception {
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestList = new ArrayList<>();
    List<CampaignPriceSkuRequest> campaignPriceSkuRequests = new ArrayList<>();
    for (ProductL3SummaryResponse productSummary : productSummaryList) {
      if (productSummary.getVariantCount() == Constants.NO_VARIANTS_COUNT && Objects.nonNull(
          productSummary.getItemL4SummaryResponse())) {
        InventoryDetailInfoRequestDTO inventoryRequest = new InventoryDetailInfoRequestDTO();
        inventoryRequest.setWebItemSku(productSummary.getItemL4SummaryResponse().getItemSku());
        inventoryRequest.setWebMerchantCode(productSummary.getMerchantCode());
        inventoryRequest.setPickupPointCode(
            productSummary.getItemL4SummaryResponse().getPickupPointCode());
        inventoryDetailInfoRequestList.add(inventoryRequest);
        CampaignPriceSkuRequest campaignPriceRequest =
            new CampaignPriceSkuRequest(productSummary.getItemL4SummaryResponse().getItemSku(),
                productSummary.getCategoryCode(),
                productSummary.getItemL4SummaryResponse().getPickupPointCode(), productSummary
                .getItemL4SummaryResponse().getPrice().stream().findFirst().orElse(new PriceDTO()).getOfferPrice());
        campaignPriceSkuRequests.add(campaignPriceRequest);
      }
    }
    if (CollectionUtils.isNotEmpty(inventoryDetailInfoRequestList)) {
      GdnRestListResponse<InventoryDetailInfoResponseV2DTO> inventoryL5Response =
          inventoryOutbound.findDetailByWebMerchantCodeAndWebItemSku(OfflineItemInventoryUtil.generateMandatoryRequestParam(),
              new ListRequestDTO<>(inventoryDetailInfoRequestList));
      itemInventoryMapL5.putAll(inventoryL5Response.getContent().stream().filter(
          inventoryDetailInfoResponseV2DTO -> Objects.nonNull(
              inventoryDetailInfoResponseV2DTO.getWebInventoryResponse())).collect(Collectors.toMap(
          inventoryL5 -> RequestHelper.toL5Id(inventoryL5.getWebInventoryResponse().getWebItemSku(),
              inventoryL5.getWebInventoryResponse().getPickupPointCode()), Function.identity())));
    }
    if (CollectionUtils.isNotEmpty(campaignPriceSkuRequests)) {
      CampaignPriceResponse campaignL5Response = campaignOutbound.getCampaignPriceInfoV2(
              CampaignPriceRequest.builder().campaignPriceSkuRequestList(campaignPriceSkuRequests)
                  .build());
      Optional.ofNullable(campaignL5Response).map(CampaignPriceResponse::getItemInfoToPriceResponse)
        .filter(CollectionUtils::isNotEmpty).ifPresent(itemInfoToPriceResponse -> {
          itemCampaignMap.putAll(campaignL5Response.getItemInfoToPriceResponse().stream().collect(
            Collectors.toMap(
              campaignPriceSkuResponse -> RequestHelper.toL5Id(campaignPriceSkuResponse.getItemSku(),
                campaignPriceSkuResponse.getPickUpPointCode()), Function.identity())));
        });
    }
  }

  @Override
  public Page<ProductL3ListingResponse> getProductL3List(String storeId, ProductL3ListingRequest request,
      Integer page, Integer size) throws Exception {
    List<ProductL3ListingResponse> productL3ListingResponses = new ArrayList<>();
    GdnRestListResponse<ProductL3SummaryResponse> productL3SummaryResponse =
        xProductOutbound.filterSummaryL3(page, size, RequestHelper.toProductSummaryRequest(request));
    if (CollectionUtils.isNotEmpty(productL3SummaryResponse.getContent())) {
      Map<String, InventoryStockInfoDTO> productSkuInventoryMap = new HashMap<>();
      Map<String, InventoryDetailInfoResponseV2DTO> itemInventoryMapL5 = new HashMap<>();
      Map<String, CampaignPriceSkuResponse> itemCampaignMap = new HashMap<>();
      Map<String, String> skuXSuspensionReasonMap = new HashMap<>();
      Map<String, String> skuXCategoryName =
          this.fetchProductSkuToMasterCategoryMap(productL3SummaryResponse.getContent());
      if (Boolean.TRUE.equals(request.getSuspended())) {
        List<String> productSkuList = productL3SummaryResponse.getContent().stream()
            .map(ProductL3SummaryResponse::getProductSku).collect(toList());
        skuXSuspensionReasonMap.putAll(this.fetchProductSkuToSuspensionReasonMap(storeId, productSkuList));
      } else {
        ProductSystemParameter l3StockSwitch =
            productSystemParameterService.findByStoreIdAndVariable(storeId, Constants.SHOW_L3_STOCK);
        if (Boolean.parseBoolean(l3StockSwitch.getValue())) {
          List<String> skusForL3Stock = productL3SummaryResponse.getContent().stream().filter(
              productSummary -> (productSummary.getL5Count() != Constants.NO_VARIANTS_COUNT))
              .map(ProductL3SummaryResponse::getProductSku).collect(toList());
          productSkuInventoryMap.putAll(
              fetchProductSkuToL3InventoryDetailMap(skusForL3Stock, request.getMerchantCode()));
        }
        this.fetchL5IdToInventoryAndCampaignDetailsMap(productL3SummaryResponse.getContent(), itemInventoryMapL5,
            itemCampaignMap);
      }
      productL3ListingResponses = productL3SummaryResponse.getContent().stream().map(
          response -> RequestHelper.toProductL3ListingResponse(response, skuXCategoryName,
              skuXSuspensionReasonMap, productSkuInventoryMap, itemInventoryMapL5, itemCampaignMap,
              RequestHelper.toProductDetailPage(response.getProductSku(), applicationProperties.getProductDetailPageUrlPrefix())))
          .collect(toList());
      return new PageImpl<>(productL3ListingResponses, PageRequest.of(page, size),
          productL3SummaryResponse.getPageMetaData().getTotalRecords());
    }
    return new PageImpl<>(productL3ListingResponses, PageRequest.of(page, size),
        productL3SummaryResponse.getPageMetaData().getTotalRecords());
  }

  private ProductLevel3DetailsV2Response generateProductLevel3DetailsV2ByProductDetailResponse(
          com.gdn.x.productcategorybase.dto.response.ProductDetailResponse productDetailResponse, List<CategoryResponse> categories,
          List<ProductLevel3Logistics> productLevel3Logistics, ProfileResponse profileResponse,
          ProductBusinessPartner productBusinessPartner, String productSku, String productCode,
          ProductCollection productCollection, boolean concatValueAndValueTypes) throws Exception {
    ProductLevel3DetailsV2Response product = new ProductLevel3DetailsV2Response();
    BeanUtils.copyProperties(productDetailResponse, product);
    product.setProductL3Response(new ProductL3Response());
    product.setProfileResponse(profileResponse);
    product.setProductCode(productCode);
    product.setProductSku(productSku);
    product.setBusinessPartnerCode(productBusinessPartner.getBusinessPartnerId());
    product.setMerchantCode(productBusinessPartner.getBusinessPartnerId());
    product.setSynchronize(true);
    product.setSuspended(false);
    product.setArchived(false);
    product.setProductType(
      productBusinessPartner.getProductItemBusinessPartners().get(0).getProductType());
    product.setProductEditable(true);
    product.setMerchantPromoDiscount(false);
    product.setMerchantPromoDiscountActive(false);
    product.setDisableUnSync(false);
    product.setRejected(false);
    product.setIsLateFulfillment(false);
    product.setOff2OnChannelActive(false);
    product.setCategoryCode(productBusinessPartner.getCategoryCode());
    if (Objects.nonNull(productCollection)) {
      product.setBrand(productCollection.getBrand());
      product.setBrandCode(productCollection.getBrandCode());
      product.setUpdatedBy(productCollection.getUpdatedBy());
      product.setUpdatedDate(productCollection.getUpdatedDate());
      product.setCreatedBy(productCollection.getCreatedBy());
      product.setCreatedDate(productCollection.getCreatedDate());
      product.setPostLive(productCollection.isPostLive());
      product.setNeedCorrectionNotes(productCollection.getNeedCorrectionNotes());
      product.setResubmitCount(productCollection.getResubmitCount());
    }
    product.setState(productBusinessPartner.getState());

    if (CollectionUtils.isNotEmpty(categories)) {
      CategoryDetailDto categoryDetailDto =
        ResponseHelper.generateCategoryNameIdAndHierarchy(categories);
      product.setCategoryName(categoryDetailDto.getCategoryName());
      product.setCategoryHierarchy(categoryDetailDto.getCategoryHierarchy());
      product.setCategoryId(categoryDetailDto.getCategoryId());
      product.setCategoryNameEnglish(categoryDetailDto.getCategoryNameEnglish());
      product.setCategoryHierarchyEnglish(categoryDetailDto.getCategoryHierarchyEnglish());
      product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
    }

    product.setProductName(productDetailResponse.getName());
    product.setBrand(productDetailResponse.getBrand());
    if(Objects.nonNull(productDetailResponse.getDescription())){
    product.setDescription(new String(productDetailResponse.getDescription()));
    }
    product.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    product.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    product.setProductStory(productDetailResponse.getProductStory());
    product.setUrl(productDetailResponse.getUrl());

    product.setInstallationRequired(false);
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());

    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      for (ProductAttributeResponse productAttributeResponse : productDetailResponse
        .getProductAttributeResponses()) {
        for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse
          .getProductAttributeValues()) {
          ProductLevel3AttributeResponse productLevel3Attribute =
            new ProductLevel3AttributeResponse();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute
            .setAttributeCode(productAttributeResponse.getAttribute().getAttributeCode());
          productLevel3Attribute
            .setAttributeName(productAttributeResponse.getAttribute().getName());
          productLevel3Attribute
            .setAttributeType(productAttributeResponse.getAttribute().getAttributeType());
          productLevel3Attribute
            .setVariantCreation(productAttributeResponse.getAttribute().isVariantCreation());
          productLevel3Attribute
            .setBasicView(productAttributeResponse.getAttribute().isBasicView());
          productLevel3Attribute
            .setMandatory(productAttributeResponse.getAttribute().isMandatory());
          productLevel3Attribute.setSkuValue(productAttributeResponse.getAttribute().isSkuValue());
          if (Objects.nonNull(productAttributeValueResponse.getAllowedAttributeValue())) {
            productLevel3Attribute.getValues().add(ResponseHelper.getValueAndValueType(
                productAttributeValueResponse.getAllowedAttributeValue().getValueType(),
                productAttributeValueResponse.getAllowedAttributeValue().getValue(), sizeChartValueTypeDelimiter,
                valueTypeAdditionForDefiningAttributes, concatValueAndValueTypes));
          } else if (Objects
            .nonNull(productAttributeValueResponse.getPredefinedAllowedAttributeValue())) {
            productLevel3Attribute.getValues()
              .add(productAttributeValueResponse.getPredefinedAllowedAttributeValue().getValue());
          } else if (Objects
            .nonNull(productAttributeValueResponse.getDescriptiveAttributeValue())) {
            productLevel3Attribute.getValues()
              .add(productAttributeValueResponse.getDescriptiveAttributeValue());
          }
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    for (Image imageData : productDetailResponse.getImages()) {
      if (imageData.isCommonImage()) {
        ProductL3CommonImageResponse image = new ProductL3CommonImageResponse();
        image.setMainImage(imageData.isMainImages());
        image.setSequence(imageData.getSequence());
        image.setLocationPath(imageData.getLocationPath());
        image.setActiveLocation(imageData.isActive());
        product.getCommonImages().add(image);
      }
    }

    if (CollectionUtils.isNotEmpty(productLevel3Logistics)) {
      for (ProductLevel3Logistics productLevel3LogisticsResponse : productLevel3Logistics) {
        ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse =
          new ProductItemLevel3LogisticResponse();
        BeanUtils.copyProperties(productLevel3LogisticsResponse, productItemLevel3LogisticResponse);
        product.getProductLevel3Logistics().add(productItemLevel3LogisticResponse);
      }
    }

    PreOrderResponse preOrderResponse =
      PreOrderResponse.builder().isPreOrder(productBusinessPartner.isPreOrder())
        .preOrderType(productBusinessPartner.getPreOrderType())
        .preOrderValue(productBusinessPartner.getPreOrderValue())
        .preOrderDate(productBusinessPartner.getPreOrderDate()).build();
    product.setPreOrder(preOrderResponse);
    product.setProductL3Response(new ProductL3Response());

    PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productBusinessPartner.isPreOrder())
      .preOrderType(productBusinessPartner.getPreOrderType())
      .preOrderValue(productBusinessPartner.getPreOrderValue())
      .preOrderDate(productBusinessPartner.getPreOrderDate()).build();
    product.getProductL3Response().setPreOrderDTO(preOrderDTO);
    product.setPickupPointCodes(productBusinessPartner.getProductItemBusinessPartners().stream()
        .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).map(ProductItemBusinessPartner::getPickupPointId)
        .distinct().collect(Collectors.toList()));
    product.setItemSkus(productBusinessPartner.getProductItemBusinessPartners().stream()
        .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete))
        .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(toList()));
    product.setItemCount((int) productBusinessPartner.getProductItemBusinessPartners().stream()
        .filter(Predicate.not(ProductItemBusinessPartner::isMarkForDelete)).count());
    product.setFreeSample(productBusinessPartner.isFreeSample());
    if (Objects.nonNull(productDetailResponse.getDistributionInfoResponse())) {
      DistributionInfo distributionInfo = new DistributionInfo();
      distributionInfo.setProductName(productDetailResponse.getDistributionInfoResponse().getProductName());
      distributionInfo.setCategoryName(productDetailResponse.getDistributionInfoResponse().getCategoryName());
      product.setDistributionInfoResponse(distributionInfo);
    }
    return product;
  }

  private ProductLevel3DetailsV2Response generateProductLevel3V2Detail(
    ProductL3Response productData, List<CategoryResponse> categories,
    List<ProductLevel3Logistics> productLevel3Logistics, ProfileResponse profileResponse,
    ProductCollection productCollection, InventoryStockInfoDTO inventoryStockInfoDTO,
    ProductSummaryResponseV2 productSummaryResponseV2, boolean concatValueAndValueTypes) throws Exception {
    Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap =
        ResponseHelper.getAttributeCodeAndValueAndValueTypeMap(productData.getMasterDataProduct(), sizeChartValueTypeDelimiter);
    ProductLevel3DetailsV2Response product = new ProductLevel3DetailsV2Response();
    BeanUtils.copyProperties(productData, product);
    product.setProductL3Response(productData);
    product.setProfileResponse(profileResponse);
    product.setBusinessPartnerCode(productData.getMerchantCode());
    product.setSynchronize(productData.isSynchronized());
    product.setProductType(productData.getProductType().getCode());
    product.setSuspended(productData.isSuspended());
    product.setArchived(productData.isArchived());
    product.setProductEditable(productData.isProductEditable());
    product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
    product.setMerchantPromoDiscountActive(productData.isMerchantPromoDiscountActive());
    product.setDisableUnSync(productData.isDisableUnSync());
    if (productData.isMarkForDelete() && !productData.isSuspended()) {
      product.setRejected(true);
    }
    product.setIsLateFulfillment(productData.getIsLateFulfillment());
    product.setOff2OnChannelActive(productData.isOff2OnChannelActive());
    product.setCategoryCode(productData.getMasterCatalog().getCategory().getCategoryCode());
    if (Objects.nonNull(productData.getProductScore())) {
      ProductScoreResponse productScore = new ProductScoreResponse();
      BeanUtils.copyProperties(productData.getProductScore(), productScore);
      product.setProductScore(productScore);
    }

    if (CollectionUtils.isNotEmpty(categories)) {
      CategoryDetailDto categoryDetailDto =
        ResponseHelper.generateCategoryNameIdAndHierarchy(categories);
      product.setCategoryName(categoryDetailDto.getCategoryName());
      product.setCategoryHierarchy(categoryDetailDto.getCategoryHierarchy());
      product.setCategoryId(categoryDetailDto.getCategoryId());
      product.setCategoryNameEnglish(categoryDetailDto.getCategoryNameEnglish());
      product.setCategoryHierarchyEnglish(categoryDetailDto.getCategoryHierarchyEnglish());
      product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
    }

    if (Objects.nonNull(productData.getMasterDataProduct())) {
      product.setProductName(productData.getMasterDataProduct().getProductName());
      product.setBrand(productData.getMasterDataProduct().getBrand());
      product.setDescription(new String(productData.getMasterDataProduct().getDescription()));
      product.setSpecificationDetail(productData.getMasterDataProduct().getSpecificationDetail());
      product.setUniqueSellingPoint(productData.getMasterDataProduct().getUniqueSellingPoint());
      product.setProductStory(productData.getMasterDataProduct().getProductStory());
      product.setUrl(productData.getMasterDataProduct().getUrl());
    }
    product.setInstallationRequired(productData.isInstallationRequired());
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());

    Map<String, MasterDataAttributeDTO> attributeDatas =
      new HashMap<String, MasterDataAttributeDTO>();
    if (Objects.nonNull(productData.getMasterDataProduct())) {
      for (MasterDataProductAttributeDTO attributeData : productData.getMasterDataProduct()
        .getMasterDataProductAttributes()) {
        attributeDatas.put(attributeData.getMasterDataAttribute().getAttributeCode(),
          attributeData.getMasterDataAttribute());
      }
    }

    List<String> specialAttributes = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productData.getProductSpecialAttributes())) {
      for (ProductSpecialAttributeDTO attributeSpecialData : productData
        .getProductSpecialAttributes()) {
        ProductLevel3AttributeResponse productLevel3Attribute =
          new ProductLevel3AttributeResponse();
        productLevel3Attribute.setValues(new ArrayList<>());
        productLevel3Attribute.setAttributeCode(attributeSpecialData.getAttributeCode());
        setProductLevel3SpecialAttributes(attributeDatas, attributeSpecialData,
          productLevel3Attribute);
        productLevel3Attribute.getValues().add(attributeSpecialData.getAttributeValue());
        product.getAttributes().add(productLevel3Attribute);
        specialAttributes.add(productLevel3Attribute.getAttributeCode());
      }
    }

    MasterDataAttributeDTO masterDataAttributeDTO = null;
    if (CollectionUtils.isNotEmpty(productData.getDescriptiveAttributes())) {
      for (ProductAttributeDetailDTO descriptiveAttributeData : productData
        .getDescriptiveAttributes()) {
        if (!specialAttributes.contains(descriptiveAttributeData.getAttributeCode())) {
          ProductLevel3AttributeResponse productLevel3Attribute =
            new ProductLevel3AttributeResponse();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute.setAttributeCode(descriptiveAttributeData.getAttributeCode());
          productLevel3Attribute.setAttributeName(descriptiveAttributeData.getAttributeName());
          masterDataAttributeDTO = attributeDatas.get(descriptiveAttributeData.getAttributeCode());
          if (Objects.nonNull(masterDataAttributeDTO) && Objects
            .nonNull(masterDataAttributeDTO.getAttributeType())) {
            productLevel3Attribute
              .setAttributeType(masterDataAttributeDTO.getAttributeType().name());
            productLevel3Attribute.setVariantCreation(masterDataAttributeDTO.isVariantCreation());
            productLevel3Attribute.setBasicView(masterDataAttributeDTO.isBasicView());
            productLevel3Attribute.setMandatory(masterDataAttributeDTO.isMandatory());
            productLevel3Attribute.setSkuValue(masterDataAttributeDTO.isSkuValue());
          }
          productLevel3Attribute.getValues().add(descriptiveAttributeData.getAttributeValue());
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(productData.getDefiningAttributes())) {
      for (ProductAttributeDTO definingAttributeData : productData.getDefiningAttributes()) {
        String itemSku = definingAttributeData.getItemSku();
        for (ProductAttributeDetailDTO definingDetailAttributeData : definingAttributeData
          .getProductAttributeDetails()) {
          ProductLevel3AttributeResponse productLevel3Attribute =
            new ProductLevel3AttributeResponse();
          productLevel3Attribute.setValues(new ArrayList<>());
          productLevel3Attribute.setAttributeCode(definingDetailAttributeData.getAttributeCode());
          productLevel3Attribute.setAttributeName(definingDetailAttributeData.getAttributeName());
          masterDataAttributeDTO =
            attributeDatas.get(definingDetailAttributeData.getAttributeCode());
          if (masterDataAttributeDTO != null && masterDataAttributeDTO.getAttributeType() != null) {
            productLevel3Attribute
              .setAttributeType(masterDataAttributeDTO.getAttributeType().name());
            productLevel3Attribute.setBasicView(masterDataAttributeDTO.isBasicView());
            productLevel3Attribute.setMandatory(masterDataAttributeDTO.isMandatory());
            productLevel3Attribute.setSkuValue(masterDataAttributeDTO.isSkuValue());
            productLevel3Attribute.setVariantCreation(masterDataAttributeDTO.isVariantCreation());
          }
          productLevel3Attribute.getValues().add(
              ResponseHelper.getValueAndValueType(attributeCodeAndValueAndValueTypeMap,
                  definingDetailAttributeData.getAttributeCode(), definingDetailAttributeData.getAttributeValue(),
                  valueTypeAdditionForDefiningAttributes, concatValueAndValueTypes));
          productLevel3Attribute.setItemSku(itemSku);
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }

    ResponseHelper.concatValueAndValueTypesInDefiningAttributes(valueTypeAdditionForDefiningAttributes,
        concatValueAndValueTypes, product, attributeCodeAndValueAndValueTypeMap);

    if (Objects.nonNull(productData.getMasterDataProduct())) {
      for (MasterDataProductImageDTO imageData : productData.getMasterDataProduct()
        .getMasterDataProductImages()) {
        if (imageData.isCommonImage()) {
          ProductL3CommonImageResponse image = new ProductL3CommonImageResponse();
          image.setMainImage(imageData.isMainImage());
          image.setSequence(imageData.getSequence());
          image.setLocationPath(imageData.getLocationPath());
          product.getCommonImages().add(image);
        }
      }
    }

    if (CollectionUtils.isNotEmpty(productLevel3Logistics)) {
      for (ProductLevel3Logistics productLevel3LogisticsResponse : productLevel3Logistics) {
        ProductItemLevel3LogisticResponse productItemLevel3LogisticResponse =
          new ProductItemLevel3LogisticResponse();
        BeanUtils.copyProperties(productLevel3LogisticsResponse, productItemLevel3LogisticResponse);
        product.getProductLevel3Logistics().add(productItemLevel3LogisticResponse);
      }
    }

    if (Objects.nonNull(productData.getPreOrderDTO())) {
      PreOrderDTO preOrderDTO = productData.getPreOrderDTO();
      PreOrderResponse preOrderResponse =
        PreOrderResponse.builder().isPreOrder(preOrderDTO.getIsPreOrder())
          .preOrderType(preOrderDTO.getPreOrderType()).preOrderValue(preOrderDTO.getPreOrderValue())
          .preOrderDate(preOrderDTO.getPreOrderDate()).build();
      product.setPreOrder(preOrderResponse);
    }

    product.setPickupPointCodes(productData.getPickupPointCodes());
    product.setItemSkus(productData.getItemSkus());
    product.setFreeSample(productData.isFreeSample());
    if (Objects.nonNull(productCollection)) {
      product.setBrandCode(productCollection.getBrandCode());
      product.setUpdatedBy(productCollection.getUpdatedBy());
      product.setUpdatedDate(productCollection.getUpdatedDate());
      product.setCreatedBy(productCollection.getCreatedBy());
      product.setCreatedDate(productCollection.getCreatedDate());
      product.setPostLive(productCollection.isPostLive());
      product.setState(productCollection.getState());
    }
    product.setTotalStock(
      inventoryStockInfoDTO.getWarehouseTotalAvailableStock() + inventoryStockInfoDTO
        .getWebTotalAvailableStock());
    product.setTotalWebStock(inventoryStockInfoDTO.getWebTotalAvailableStock());
    product.setTotalWarehouseStock(inventoryStockInfoDTO.getWarehouseTotalAvailableStock());
    product.setTotalActiveStock(inventoryStockInfoDTO.getTotalStock());
    product.setFbbActivated(productData.isFbbActivated());
    product.setSynchronized(productData.isSynchronized());
    product.setMinNormalPrice(productSummaryResponseV2.getMinNormalPrice());
    product.setMaxNormalPrice(productSummaryResponseV2.getMaxNormalPrice());
    product.setMinSellingPrice(productSummaryResponseV2.getMinSellingPrice());
    product.setMaxSellingPrice(productSummaryResponseV2.getMaxSellingPrice());
    if (ranchIntegrationEnabled && Objects.nonNull(productData.getDistributionInfoDTO())) {
      DistributionInfo distributionInfo = new DistributionInfo();
      distributionInfo.setProductName(productData.getDistributionInfoDTO().getProductName());
      distributionInfo.setCategoryName(productData.getDistributionInfoDTO().getCategoryName());
      product.setDistributionInfoResponse(distributionInfo);
    }
    return product;
  }

  private void setProductLevel3SpecialAttributes(Map<String, MasterDataAttributeDTO> attributeDatas,
    ProductSpecialAttributeDTO attributeSpecialData,
    ProductLevel3AttributeResponse productLevel3Attribute) {
    if (!attributeDatas.containsKey(attributeSpecialData.getAttributeCode())) {
      AttributeResponse attributeResponse =
        productOutbound.getAttributeDetailByAttributeCode(attributeSpecialData.getAttributeCode());
      productLevel3Attribute.setAttributeName(attributeResponse.getName());
      productLevel3Attribute.setAttributeType(attributeResponse.getAttributeType());
      productLevel3Attribute.setBasicView(attributeResponse.isBasicView());
      productLevel3Attribute.setMandatory(attributeResponse.isMandatory());
      productLevel3Attribute.setSkuValue(attributeResponse.isSkuValue());
      productLevel3Attribute.setVariantCreation(attributeResponse.isVariantCreation());
    } else {
      MasterDataAttributeDTO masterDataAttributeDetail =
        attributeDatas.get(attributeSpecialData.getAttributeCode());
      productLevel3Attribute.setAttributeName(masterDataAttributeDetail.getAttributeName());
      productLevel3Attribute
        .setAttributeType(String.valueOf(masterDataAttributeDetail.getAttributeType()));
      productLevel3Attribute.setBasicView(masterDataAttributeDetail.isBasicView());
      productLevel3Attribute.setMandatory(masterDataAttributeDetail.isMandatory());
      productLevel3Attribute.setSkuValue(masterDataAttributeDetail.isSkuValue());
      productLevel3Attribute.setVariantCreation(masterDataAttributeDetail.isVariantCreation());
    }
  }

  @Override
  public ItemPriceStockQuickUpdateResponse quickEditPatching(String storeId, String productSku,
    ProductLevel3QuickEditV2Request request) throws Exception {
    final List<String[]> editRequest = new ArrayList<>();
    List<ItemErrorListResponse> itemErrorListResponses = new ArrayList<>();
    editRequest.add(CAMPAIGN_REQUEST);
    editRequest.add(INVENTORY_REQUEST);
    editRequest.add(PRODUCT_REQUEST);
    sanitizeSellerSkuV2(request);
    setCncStatusForBackwardCompatibility(request);
    request.getQuickEditV2Requests().forEach(
      quickEditV2Request -> quickEditV2Request.setItemPickupPointId(
        CommonUtils.toL5Id(quickEditV2Request.getItemSku(),
          quickEditV2Request.getPickupPointCode())));
    ProductL3Response savedProductData = null;
    Map<String, ArrayList<String>> errorCodeMap = new HashMap<>();
    Map<String, ItemSummaryListResponse> itemSummaryResponseMap =
      fetchAndGenerateItemSummaryListResponseMap(request.getQuickEditV2Requests());
    if(fetchL3DetailForPLPAndPDPEdit) {
      // with switch off the call to fetch complete detail by product SKU will be skipped
      savedProductData = xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    }
    ApiErrorCode apiErrorCode = validateUpdateInfo(storeId, savedProductData, request,
      itemSummaryResponseMap, errorCodeMap );
    if (Objects.nonNull(apiErrorCode)) {
      return ItemPriceStockQuickUpdateResponse.builder().apiErrorCode(apiErrorCode).build();
    }
    if (instore2FlowSwitch) {
      validateStatusAndCncUpdateForInstoreProduct(request,
        itemSummaryResponseMap);
    }

    for (QuickEditV2Request quickEditV2Request : request.getQuickEditV2Requests()) {
      Long productVersion =
        Optional.ofNullable(savedProductData).map(ProductL3Response::getVersion).orElse(
        itemSummaryResponseMap.values().stream().findFirst()
          .map(ItemSummaryListResponse::getVersion).orElse(0L));
      if ((Objects.nonNull(quickEditV2Request.getVersion()))
        && quickEditV2Request.getVersion() <= productVersion) {
        log.error("Version mismatch error. oldVersion : {}, newVersion : {} ",
          productVersion, quickEditV2Request.getVersion());
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessages.INVALID_VERSION_ERROR);
      }
      performPatchingActions(storeId, productSku, editRequest, errorCodeMap, itemSummaryResponseMap,
        quickEditV2Request);

    }
    for (Map.Entry<String, ArrayList<String>> entries : errorCodeMap.entrySet()) {
      itemErrorListResponses.add(new ItemErrorListResponse(entries.getKey(), String.join(", ", entries.getValue())));
    }
    return ItemPriceStockQuickUpdateResponse.builder().apiErrorCode(apiErrorCode)
      .variantsErrorList(itemErrorListResponses).build();
  }

  private void setCncStatusForBackwardCompatibility(ProductLevel3QuickEditV2Request request) {
    if (!cncForWarehouseFeatureSwitch) {
      return;
    }
    request.getQuickEditV2Requests().forEach(quickEditV2Request -> {
      if (Objects.isNull(quickEditV2Request.getCncStatus()) && Objects.nonNull(quickEditV2Request.getCncActive())) {
        quickEditV2Request.setCncStatus(
            quickEditV2Request.getCncActive() ? ProductLevel3Status.ONLINE : ProductLevel3Status.OFFLINE);
      }
    });
  }

  private static ApiErrorCode validateSalePriceGreaterThanRegularPrice(QuickEditV2Request quickEditV2Request) {
    if (Objects.nonNull(quickEditV2Request.getPrice())) {
      Double salePrice = quickEditV2Request.getPrice().getSalePrice();
      Double regularPrice = quickEditV2Request.getPrice().getPrice();
      if (Objects.nonNull(salePrice) && Objects.nonNull(regularPrice) && Double.compare(salePrice, regularPrice) == 1) {
        log.error("sale price : {} is less than regular price : {}", quickEditV2Request.getPrice().getSalePrice(),
            quickEditV2Request.getPrice().getPrice());
        return ApiErrorCode.INVALID_SALE_PRICE;
      }
    }
    return null;
  }

  private Map<String, ArrayList<String>> performPatchingActions(String storeId, String productSku,
    List<String[]> editRequests, Map<String, ArrayList<String>> errorCodeMap,
    Map<String, ItemSummaryListResponse> itemSummaryResponseMap,
    QuickEditV2Request quickEditV2Request) throws Exception {
    List<String> notNullPropertiesNames = getNotNullProperties(quickEditV2Request);
    ItemSummaryListResponse itemSummaryListResponse =
      itemSummaryResponseMap.get(quickEditV2Request.getItemPickupPointId());
    ProfileResponse businessPartner =
      this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
        itemSummaryListResponse.getMerchantCode());
    if (CommonUtils.validateStockIncrementForSellerPenalty(businessPartner,
        sellerPenaltyEnabledPhase2, quickEditV2Request.getDeltaStock())) {
      log.error("Stock update failed: You are not allowed to increase stock due to seller penalty "
          + "restrictions for item sku : {} ", quickEditV2Request.getItemSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.SELLER_PENALTY_RESTRICTION.getDesc());
    }
    Map<String, ProductLevel3Summary> productLevel3SummaryMap = new HashMap<>();
    productLevel3SummaryMap.put(
      quickEditV2Request.getItemSku() + Constants.DASH_DELIMITER + quickEditV2Request.getPickupPointCode(),
      generateProductLevel3SummaryFromItemPickupPoint(itemSummaryListResponse.getMerchantCode(),
        itemSummaryListResponse, businessPartner));
    CommonUtils.validateSyncStockForFaasSeller(quickEditV2Request, businessPartner,
      itemSummaryListResponse, faasFeatureSwitch);
    if (!Collections.disjoint(notNullPropertiesNames, Arrays.asList(editRequests.get(1)))) {
      // do inventory actions
      try {
        validateAndUpdateStockAndPickupPointForL5(quickEditV2Request, itemSummaryListResponse,
          businessPartner, true);
        generateProductHistoryForSummary(itemSummaryListResponse,
            Boolean.TRUE.equals(quickEditV2Request.getWholeSaleActivated()), productLevel3SummaryMap.get(
                quickEditV2Request.getItemSku() + Constants.DASH_DELIMITER + quickEditV2Request.getPickupPointCode()),
            quickEditV2Request.isScheduleUpdate());
      } catch (Exception e) {
        log.error("Error on stock update from inventory, item : {}, error : {}, error - ",
          quickEditV2Request.getItemSku(), e.getMessage(), e);
        errorCodeMap.compute(quickEditV2Request.getItemPickupPointId(),
          (key, errors) -> errors == null ? new ArrayList<>() : errors).add(e.getMessage());
      }
    }
    if (!Collections.disjoint(notNullPropertiesNames,
      Stream.of(CAMPAIGN_REQUEST, PRODUCT_REQUEST).flatMap(Stream::of)
        .collect(Collectors.toList()))) {
      List<String> pathingL5Request =
        notNullPropertiesNames.stream().filter(Arrays.asList(editRequests.get(2))::contains)
          .collect(Collectors.toList());
      log.info("Performing L5 updates for Item PickupPoint code : {}",
        quickEditV2Request.getPickupPointCode());
      if (isPriceEdited(quickEditV2Request, itemSummaryListResponse)) {
        validateDiscountPrice(quickEditV2Request.getItemSku(),
            Optional.ofNullable(quickEditV2Request.getPrice()).map(ProductLevel3PriceRequest::getSalePrice)
                .orElse(itemSummaryListResponse.getPrice().iterator().next().getOfferPrice()),
            quickEditV2Request.getPickupPointCode(), itemSummaryListResponse.getMasterCategoryCode());
      }
      patchL5Details(itemSummaryResponseMap, quickEditV2Request, productSku, storeId,
        pathingL5Request, errorCodeMap, businessPartner, productLevel3SummaryMap);
      if (!Collections.disjoint(notNullPropertiesNames, Arrays.asList(editRequests.get(0)))) {
        updatePriceData(errorCodeMap, quickEditV2Request, itemSummaryListResponse);
        log.info(
          "Price Data Update(Campaign Actions) was performed for Item Pickup Point code : {}",
          quickEditV2Request.getPickupPointCode());
      }
    }
    return errorCodeMap;
  }

  private boolean isPriceEdited(QuickEditV2Request quickEditV2Request,
      ItemSummaryListResponse itemSummaryListResponse) {
    return
        Optional.of(quickEditV2Request).map(QuickEditV2Request::getPrice).map(ProductLevel3PriceRequest::getSalePrice)
            .isPresent() && isSalesPriceChangedOrPriceEditDisabled(itemSummaryListResponse.getPrice(),
            quickEditV2Request.getPrice().getSalePrice(),
            CommonUtils.isPriceEditDisabled(itemSummaryListResponse.isMerchantPromoDiscount(),
                itemSummaryListResponse.getPrice()));
  }

  private void updatePriceData(Map<String, ArrayList<String>> errorCodeMap,
    QuickEditV2Request quickEditV2Request, ItemSummaryListResponse itemSummaryListResponse)
    throws Exception {
    if (isPriceEdited(quickEditV2Request, itemSummaryListResponse) && !errorCodeMap.containsKey(
        quickEditV2Request.getItemPickupPointId())) {
      try {
        updateOfferPriceInCampaign(quickEditV2Request.getItemSku(),
          quickEditV2Request.getPickupPointCode(), quickEditV2Request.getPrice().getSalePrice(),
          itemSummaryListResponse.getMasterCategoryCode());
      } catch (ApplicationRuntimeException ex) {
        log.error("Error on price update for campaign, item : {}, error : {}, error - ",
          quickEditV2Request.getItemSku(), ex.getErrorMessage(), ex);
        errorCodeMap.compute(quickEditV2Request.getItemPickupPointId(),
          (key, errors) -> errors == null ? new ArrayList<>() : errors).add(
          ex.getMessage() + " for price data update for PP code : "
            + quickEditV2Request.getItemPickupPointId());
      }
    }
  }

  private void patchL5Details(Map<String, ItemSummaryListResponse> itemSummaryListResponseMap,
    QuickEditV2Request quickEditV2Request, String productSku, String storeId,
    List<String> patchingRequest, Map<String, ArrayList<String>> errorCodeMap,
    ProfileResponse businessPartner,Map<String, ProductLevel3Summary> productLevel3SummaryMap) throws Exception {
    Boolean wholeSaleActivated =
      itemSummaryListResponseMap.get(quickEditV2Request.getItemPickupPointId())
        .getWholesalePriceActivated();
    ItemSummaryListResponse itemSummaryListResponse =
      itemSummaryListResponseMap.get(quickEditV2Request.getItemPickupPointId());
    if ((itemSummaryListResponse.isFreeSample()) && (!ProductLevel3Status.OFFLINE.equals(quickEditV2Request.getStatus())
        || Boolean.TRUE.equals(quickEditV2Request.getOff2OnActiveFlag()) || !ProductLevel3Status.OFFLINE.equals(
        quickEditV2Request.getCncStatus()))) {
      log.error("Error updating free sample product : {} ",
        itemSummaryListResponse.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ApiErrorCode.FREE_SAMPLE_CANNOT_BE_SET.getDesc());
    }
    validateStatusAndCncUpdateForBopisProduct(quickEditV2Request, businessPartner, itemSummaryListResponse);
    if (!Collections.disjoint(patchingRequest, Collections.singletonList(OFF2ON_ACTIVE_FLAG))) {
      if (!Boolean.valueOf(itemSummaryListResponse.isOff2OnChannelActive())
        .equals(quickEditV2Request.getOff2OnActiveFlag())) {
        try {
          log.info("Attempting off2On Flag update for Item Pickup point code : {}",
            quickEditV2Request.getPickupPointCode());
          updateOff2OnActiveFlag(itemSummaryListResponse.getMerchantCode(),
            itemSummaryListResponse.getProductSku(), itemSummaryListResponse.getProductName(),
            quickEditV2Request.getOff2OnActiveFlag());
        } catch (Exception e) {
          log.error("Error on update of off2OnActiveFlag, item : {}, error : {}, error - ",
            quickEditV2Request.getItemSku(), e.getMessage(), e);
          errorCodeMap.compute(quickEditV2Request.getItemPickupPointId(),
            (key, errors) -> errors == null ? new ArrayList<>() : errors).add(
            e.getMessage() + " for off2OnActiveFlag update for PP code : "
              + quickEditV2Request.getItemPickupPointId());
        }
      }
    }
    if (!Collections.disjoint(patchingRequest, Collections.singletonList(WHOLESALE_ACTIVATED))) {
      try {
        log.info("Attempting WholeSale Activated Flag update for Item Pickup point code : {}",
          quickEditV2Request.getPickupPointCode());
        wholeSaleActivated =
          validateAndUpdateWholesaleFlagForL5(storeId, quickEditV2Request, itemSummaryListResponse);
      } catch (Exception e) {
        log.error("Error on update of wholeSaleActivated, item : {}, error : {}, error - ",
          quickEditV2Request.getItemSku(), e.getMessage(), e);
        errorCodeMap.compute(quickEditV2Request.getItemPickupPointId(),
          (key, errors) -> errors == null ? new ArrayList<>() : errors).add(
          e.getMessage() + " for wholeSaleActivated update for PP code : "
            + quickEditV2Request.getItemPickupPointId());
      }
    }
    try {
      if (Objects.isNull(quickEditV2Request.getWholeSaleActivated())) {
        quickEditV2Request.setWholeSaleActivated(
          Optional.ofNullable(itemSummaryListResponse.getWholesalePriceActivated()).orElse(null));
      }
      log.info("Proceeding with L5 Listing update in xProduct for Item Pickup point code : {}",
        quickEditV2Request.getPickupPointCode());
      patchItemPickupPointListing(itemSummaryListResponse, quickEditV2Request, productSku,
        businessPartner, wholeSaleActivated, productLevel3SummaryMap);
    } catch (Exception e) {
      log.error("Error on update of Item PickupPointListing , item : {}, error : {}, error - ",
        quickEditV2Request.getItemSku(), e.getMessage(), e);
      errorCodeMap.compute(quickEditV2Request.getItemPickupPointId(),
        (key, errors) -> errors == null ? new ArrayList<>() : errors).add(
        e.getMessage() + " for L5 data update for PP code : "
          + quickEditV2Request.getItemPickupPointId());
    }
  }

  private void validateStatusAndCncUpdateForBopisProduct(QuickEditV2Request quickEditV2Request,
      ProfileResponse businessPartner, ItemSummaryListResponse itemSummaryListResponse) {
    String merchantType = CommonUtils.getMerchantTypeFromProfileResponse(businessPartner);
    if (CommonUtils.validateShippingTypeAndDimensionMissingForBopis(bopisCategoryRestrictionEnabled,
        itemSummaryListResponse)) {
      log.error("Error updating for product : {} ", itemSummaryListResponse.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.BOPIS_EDIT_NOT_SUPPORTED.getDesc());
    }
    if (CommonUtils.validateStatusUpdateForBopis(quickEditV2Request, merchantType, itemSummaryListResponse,
        bopisCategoryRestrictionEnabled, Arrays.asList(bopisUnsupportedMerchantTypes.split(Constants.COMMA)))) {
      log.error("Error updating status for product : {} ", itemSummaryListResponse.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.BOPIS_STATUS_CHANGE_ERROR.getDesc());
    }
    if (CommonUtils.validateCncUpdateForBopis(quickEditV2Request, itemSummaryListResponse,
        bopisCNCRestrictionEnabled, cncForWarehouseFeatureSwitch)) {
      log.error("Error updating Cnc for product : {} ", itemSummaryListResponse.getProductSku());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ApiErrorCode.BOPIS_CNC_CHANGE_ERROR.getDesc());
    }
  }

  private void patchItemPickupPointListing(ItemSummaryListResponse itemSummaryListResponse,
    QuickEditV2Request quickEditV2Request, String productSku, ProfileResponse profileResponse,
    Boolean wholesaleActivated,Map<String, ProductLevel3Summary> productLevel3SummaryMap) throws Exception {
    QuickEditV2Request populatedQuickEditV2Request;
    List<ItemPickupPointQuickEditRequest> itemPickupPointQuickEditRequests = new ArrayList<>();
    Map<String, Boolean> wholeSaleActivatedMap = new HashMap<>();
    ProductType productType = itemSummaryListResponse.getProductType();
    if (Objects.nonNull(itemSummaryListResponse)) {
      ItemViewConfigDTO defaultItemViewConfigDTO = itemSummaryListResponse.getItemViewConfigs().stream()
          .filter(itemViewConfigDTO -> Constants.DEFAULT.equals(itemViewConfigDTO.getChannel())).findFirst()
          .orElse(new ItemViewConfigDTO());
      ItemViewConfigDTO cncItemViewConfigDTO = itemSummaryListResponse.getItemViewConfigs().stream()
          .filter(itemViewConfigDTO -> Constants.CNC_CHANNEL.equals(itemViewConfigDTO.getChannel())).findFirst()
          .orElse(new ItemViewConfigDTO());
      String status = ConverterUtil.getProductStatus(defaultItemViewConfigDTO.isBuyableOriginal(),
          defaultItemViewConfigDTO.isDiscoverableOriginal());
      String cncStatus = ConverterUtil.getProductStatus(cncItemViewConfigDTO.isBuyableOriginal(),
          cncItemViewConfigDTO.isDiscoverableOriginal());
      log.info("Item Summary List Response for Code : {}, is {}",
        quickEditV2Request.getPickupPointCode(), itemSummaryListResponse);
      if(Objects.nonNull(wholesaleActivated)) {
        quickEditV2Request.setWholeSaleActivated(wholesaleActivated);
      }
      populatedQuickEditV2Request =
        RequestHelper.populateQuickEditV2Request(quickEditV2Request, itemSummaryListResponse,
          status, cncStatus, cncForWarehouseFeatureSwitch);
      log.info("Populated request was : {}", populatedQuickEditV2Request);
      if (productLevel3Helper.isProductItemDetailChangedForL5ListingUpdate(
        populatedQuickEditV2Request, itemSummaryListResponse, wholesaleActivated)) {
        ItemPickupPointQuickEditRequest itemPickupPointQuickEditRequest =
          ConverterUtil.toItemPickupPointListingUpdateRequestVo(populatedQuickEditV2Request);
        CommonUtils.setSchedulesForQuickEditRequest(itemSummaryListResponse,
          itemPickupPointQuickEditRequest, cncForWarehouseFeatureSwitch);
        itemPickupPointQuickEditRequest.setWholeSaleActivated(wholesaleActivated);
        itemPickupPointQuickEditRequest.setMerchantSku(populatedQuickEditV2Request.getSellerSku());
        itemPickupPointQuickEditRequests.add(itemPickupPointQuickEditRequest);
        wholeSaleActivatedMap.put(quickEditV2Request.getItemSku(), wholesaleActivated);
      }
      if (CollectionUtils.isNotEmpty(itemPickupPointQuickEditRequests)) {
        xProductOutbound.updateItemPickupPointListing(productSku, productType,
          itemPickupPointQuickEditRequests);
        log.info("updated data in x-product for Item Pickup Point Code : {}",
          quickEditV2Request.getItemPickupPointId());
      }
      generateProductHistoryForSummary(itemSummaryListResponse,
          wholeSaleActivatedMap.get(populatedQuickEditV2Request.getItemSku()), productLevel3SummaryMap.get(
              quickEditV2Request.getItemSku() + Constants.DASH_DELIMITER + quickEditV2Request.getPickupPointCode()),
          quickEditV2Request.isScheduleUpdate());
      if (ConverterUtil.isProductStatusChange(
        itemSummaryListResponse.getItemViewConfigs().stream().findFirst().get().isBuyable(),
        itemSummaryListResponse.getItemViewConfigs().stream().findFirst().get().isDiscoverable(),
        populatedQuickEditV2Request.getStatus().name())) {
        productNotificationService.sendNotificationForProductStatus(
          profileResponse.getBusinessPartnerCode(), itemSummaryListResponse.getItemName(),
          populatedQuickEditV2Request.getStatus().name());
      }
    }
  }


  public ApiErrorCode validateUpdateInfo(String storeId, ProductL3Response productDetailsByProductSku,
    ProductLevel3QuickEditV2Request request, Map<String, ItemSummaryListResponse> itemSummaryResponseMap,
    Map<String, ArrayList<String>> errorCodeMap) {
    List<QuickEditV2Request> quickEditV2Requests = new ArrayList<>(request.getQuickEditV2Requests());
    List<QuickEditV2Request> validatedRequests = new ArrayList<>();
    List<ApiErrorCode> apiErrorCodes = new ArrayList<>();
    if (MapUtils.isEmpty(itemSummaryResponseMap)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        ErrorMessages.ITEM_SKU_NOT_FOUND);
    }

    List<String> itemSkusRequest = getItemSkusRequest(request);
    List<String> itemSkus = getItemSkus(productDetailsByProductSku, itemSummaryResponseMap);

    if (!areAllItemSkusMapped(itemSkus, itemSkusRequest)) {
      log.error("Item SKUs in the request were not mapped to Product SKU for {}",
        request.getQuickEditV2Requests().stream().map(QuickEditV2Request::getItemPickupPointId)
          .collect(Collectors.toList()));
      apiErrorCodes.add(ApiErrorCode.INVALID_DATA_INPUT);
    }
    ItemSummaryListResponse itemSummaryListResponse = getItemSummaryListResponse(itemSummaryResponseMap);

    if (isActiveInXProduct(productDetailsByProductSku, itemSummaryListResponse)) {
      Optional.ofNullable(validateProductDetails(productDetailsByProductSku, itemSummaryListResponse, storeId))
        .ifPresent(apiErrorCodes::add);
    }
    else {
      apiErrorCodes.add(ApiErrorCode.PRODUCT_DELETED_STATE);
    }
    if (isArchived(productDetailsByProductSku, itemSummaryListResponse)) {
      apiErrorCodes.add(ApiErrorCode.ITEM_IS_ARCHIVED);
    }
    performPriceEditValidations(quickEditV2Requests, itemSummaryListResponse, apiErrorCodes, errorCodeMap);
    for (QuickEditV2Request quickEditV2Request : quickEditV2Requests) {
      ApiErrorCode apiErrorCode = validateSalePriceGreaterThanRegularPrice(quickEditV2Request);
      if (Objects.isNull(apiErrorCode)) {
        log.error("PickupPointId : {} was removed as Sale price is greater than regular price",
          quickEditV2Request.getItemPickupPointId());
        validatedRequests.add(quickEditV2Request);
      }
      apiErrorCodes.add(apiErrorCode);
    }
    request.setQuickEditV2Requests(validatedRequests);
    return apiErrorCodes.stream().filter(Objects::nonNull).findFirst().orElse(null);
  }

  private void performPriceEditValidations(List<QuickEditV2Request> quickEditV2Requests,
    ItemSummaryListResponse itemSummaryListResponse, List<ApiErrorCode> apiErrorCodes,
    Map<String, ArrayList<String>> errorCodeMap) {
    quickEditV2Requests.stream()
      .filter(quickEditV2Request -> Optional.ofNullable(quickEditV2Request).map(QuickEditV2Request::getPrice).map(ProductLevel3PriceRequest::getSalePrice).isPresent())
      .filter(quickEditV2Request -> isPriceEditNotAllowed(itemSummaryListResponse.getPrice(),
        quickEditV2Request.getPrice().getSalePrice(), itemSummaryListResponse)).findAny()
      .ifPresent(quickEditV2Request -> {
        log.error("Price Update request failed for L5 : {} ", quickEditV2Request.getItemPickupPointId());
        errorCodeMap.compute(quickEditV2Request.getItemPickupPointId(),
          (key, errors) -> new ArrayList<>()).add(ApiErrorCode.PRICE_UPDATE_FAILED.getCode());});
  }



  private List<String> getItemSkusRequest(ProductLevel3QuickEditV2Request request) {
    return request.getQuickEditV2Requests().stream()
      .map(QuickEditV2Request::getItemSku)
      .collect(Collectors.toList());
  }

  private List<String> getItemSkus(ProductL3Response productDetailsByProductSku,
    Map<String, ItemSummaryListResponse> itemSummaryResponseMap) {
    return Optional.ofNullable(productDetailsByProductSku)
      .map(ProductL3Response::getItemSkus)
      .orElseGet(() -> itemSummaryResponseMap.values().stream()
        .map(ItemSummaryListResponse::getItemSku)
        .collect(Collectors.toList()));
  }

  private boolean areAllItemSkusMapped(List<String> itemSkus, List<String> itemSkusRequest) {
    return new HashSet<>(itemSkus).containsAll(itemSkusRequest);
  }

  private ItemSummaryListResponse getItemSummaryListResponse(Map<String, ItemSummaryListResponse> itemSummaryResponseMap) {
    return itemSummaryResponseMap.values().stream().findFirst()
      .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        ErrorMessages.ITEM_SKU_NOT_FOUND));
  }

  private boolean isActiveInXProduct(ProductL3Response productDetailsByProductSku,
    ItemSummaryListResponse itemSummaryListResponse) {
    return !Optional.ofNullable(productDetailsByProductSku)
      .map(ProductL3Response::isMarkForDelete)
      .orElseGet(() -> Optional.ofNullable(itemSummaryListResponse)
        .map(ItemSummaryListResponse::isMarkForDelete)
        .orElse(true));
  }

  private ApiErrorCode validateProductDetails(ProductL3Response productDetailsByProductSku,
    ItemSummaryListResponse itemSummaryListResponse,
    String storeId) {
    ProductCollection productCollection =
      productCollectionRepository.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId,
        Optional.ofNullable(productDetailsByProductSku).map(ProductL3Response::getProductCode)
          .orElse(itemSummaryListResponse.getProductCode()));

    String state = productCollection.getState();
    if (WorkflowStates.DELETED.name().equals(state)) {
      return ApiErrorCode.ITEM_IS_REJECTED;
    }
    if (WorkflowStates.NEED_CORRECTION.name().equals(state)) {
      return ApiErrorCode.ITEM_IS_IN_NEED_CORRECTION;
    }
    if (Optional.ofNullable(productDetailsByProductSku).map(ProductL3Response::isSuspended)
      .orElse(itemSummaryListResponse.isSuspended())) {
      return ApiErrorCode.ITEM_IS_SUSPENDED;
    }
    return null;
  }

  private boolean isArchived(ProductL3Response productDetailsByProductSku,
    ItemSummaryListResponse itemSummaryListResponse) {
    return Optional.ofNullable(productDetailsByProductSku).map(ProductL3Response::isArchived)
      .orElse(itemSummaryListResponse.isArchived());
  }


  private static List<String> getNotNullProperties(QuickEditV2Request quickEditV2Request) {
    final BeanWrapper editRequestBeanWrapper = new BeanWrapperImpl(quickEditV2Request);
    PropertyDescriptor[] editRequestPDs = editRequestBeanWrapper.getPropertyDescriptors();
    List<String> nonNullFields = new ArrayList<>();
    for (PropertyDescriptor pd : editRequestPDs) {
      Object srcValue = editRequestBeanWrapper.getPropertyValue(pd.getName());
      if (Objects.nonNull(srcValue))
        nonNullFields.add(pd.getName());
    }
    return nonNullFields;
  }

  @Override
  public void insertInventoryForNewlyAddedL5DuringNeedRevision(ProductBusinessPartner productBusinessPartner,
      List<NewlyAddedL5Response> newlyAddedL5Responses, ProfileResponse profileResponse) throws Exception {
    if (CollectionUtils.isNotEmpty(newlyAddedL5Responses)) {
      Map<String, Map<String, ProductItemBusinessPartner>> productItemBusinessPartnerAndItemSkuPPCodeMap = new HashMap<>();
      for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartner.getProductItemBusinessPartners()) {
        Map<String, ProductItemBusinessPartner> productItemBusinessPartnerAndPPCodeMap = productItemBusinessPartnerAndItemSkuPPCodeMap
            .getOrDefault(productItemBusinessPartner.getGdnProductItemSku(), new HashMap<>());
        productItemBusinessPartnerAndPPCodeMap.put(productItemBusinessPartner.getPickupPointId(), productItemBusinessPartner);
        productItemBusinessPartnerAndItemSkuPPCodeMap.put(productItemBusinessPartner.getGdnProductItemSku(),
            productItemBusinessPartnerAndPPCodeMap);
      }

      Map<String, Map<String, NewlyAddedL5Response>> newlyAddedL5ResponseAndItemSkuPPCodeMap = new HashMap<>();
      for (NewlyAddedL5Response newlyAddedL5Response : newlyAddedL5Responses) {
        Map<String, NewlyAddedL5Response> newlyAddedL5ResponseAndPPCodeMap = newlyAddedL5ResponseAndItemSkuPPCodeMap
            .getOrDefault(newlyAddedL5Response.getItemSku(), new HashMap<>());
        newlyAddedL5ResponseAndPPCodeMap.put(newlyAddedL5Response.getPickupPointCode(), newlyAddedL5Response);
        newlyAddedL5ResponseAndItemSkuPPCodeMap.put(newlyAddedL5Response.getItemSku(), newlyAddedL5ResponseAndPPCodeMap);
      }

      List<ProductLevel3Inventory> productLevel3InventoryList = new ArrayList<>();
      for (NewlyAddedL5Response newlyAddedL5Response : newlyAddedL5Responses) {
        ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
        productLevel3Inventory.setWebItemSku(newlyAddedL5Response.getItemSku());
        productLevel3Inventory.setWebPickupPointCode(newlyAddedL5Response.getPickupPointCode());
        ProductItemBusinessPartner productItemBusinessPartner = productItemBusinessPartnerAndItemSkuPPCodeMap
            .get(newlyAddedL5Response.getItemSku()).get(newlyAddedL5Response.getPickupPointCode());
        productLevel3Inventory.setWebAvailable(productItemBusinessPartner.getStock());
        productLevel3Inventory.setInitialPreOrderQuota(productItemBusinessPartner.getPreOrderQuota());
        productLevel3Inventory.setWebMinAlert(productItemBusinessPartner.getMinimumStock());
        productLevel3Inventory.setWebMerchantCode(profileResponse.getBusinessPartnerCode());
        productLevel3Inventory.setWarehouseItemSku(newlyAddedL5Response.getItemCode());
        productLevel3Inventory.setProductSku(productBusinessPartner.getGdnProductSku());
        productLevel3Inventory.setFbbPP(productItemBusinessPartner.isFbbActive() && mppForWhEnabled);
        productLevel3Inventory.setDistributionPickupPoint(productItemBusinessPartner.isDistribution());
        productLevel3Inventory.setWebSyncStock(
            CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch, productItemBusinessPartner.isFbbActive(),
                profileResponse));
        CommonUtils.setPreOrderFields(preOrderConfig.isPoQuotaFeatureSwitch(), profileResponse,
            productBusinessPartner.getPreOrderDate(), productLevel3Inventory,
            productItemBusinessPartner.getPreOrderQuota());
        if (ConverterUtil.isPurchaseOrderPurchaseTerm(profileResponse)) {
          productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
        } else {
          productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
        }
        productLevel3InventoryList.add(productLevel3Inventory);
      }

      productLevel3InventoryService.insertInventory(productLevel3InventoryList);
      for (ProductLevel3Inventory productLevel3Inventory : productLevel3InventoryList) {
        this.updatedProductHistoryService.createProductL3AuditLog(productBusinessPartner.getBusinessPartnerId(),
            productLevel3Inventory.getWebItemSku(), productLevel3Inventory.getProductSku(),
            newlyAddedL5ResponseAndItemSkuPPCodeMap.get(productLevel3Inventory.getWebItemSku())
                .get(productLevel3Inventory.getWebPickupPointCode()).getItemName(),
            UpdateProductActivity.STOCK_VALUE.getDesc(), Constants.HYPHEN,
            String.valueOf(productLevel3Inventory.getWebAvailable()), true,
            productLevel3Inventory.getWebPickupPointCode());
        }
    }
  }

  @Override
  public void saveL3NeedRevisionHistory(String businessPartnerCode, ProductVariantUpdateRequest request,
      ProductBusinessPartner productBusinessPartner) throws Exception {
    //save l3 shipping toggle history
    String requestId = mandatoryParameterHelper.getRequestId();
    String changedBy = mandatoryParameterHelper.getUsername();
    String clientHost = mandatoryParameterHelper.getClientId();
    if (CommonUtils.isOnlineFlagChanged(productBusinessPartner.isOnline(), request.getOnline())) {
      AuditTrailListRequest auditTrailListRequest =
          CommonUtils.getAuditTrailRequestForL3History(businessPartnerCode, request.getProductSku(),
              request.getProductName(), UpdateProductActivity.ONLINE_FLAG.name(), Constants.NEED_REVISION_WITHOUT_SPACE,
              String.valueOf(productBusinessPartner.isOnline()), String.valueOf(request.getOnline()), changedBy,
              requestId, clientHost, Constants.DEFAULT, Constants.HYPHEN);
      kafkaProducer.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, request.getProductSku(),
          auditTrailListRequest);
    }
    if (Objects.nonNull(request.getB2cActivated())) {
      if (productBusinessPartner.isB2cActivated() != request.getB2cActivated()) {
        AuditTrailListRequest auditTrailListRequest =
            CommonUtils.getAuditTrailRequestForL3History(businessPartnerCode, request.getProductSku(),
                request.getProductName(), UpdateProductActivity.B2C_ACTIVATED.name(),
                Constants.NEED_REVISION_WITHOUT_SPACE, String.valueOf(productBusinessPartner.isB2cActivated()),
                String.valueOf(request.getB2cActivated()), changedBy, requestId, clientHost, Constants.DEFAULT,
                Constants.HYPHEN);
        kafkaProducer.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, request.getProductSku(),
            auditTrailListRequest);
      }
    }
    if (Objects.nonNull(request.getB2bActivated())) {
      if (productBusinessPartner.isB2bActivated() != request.getB2bActivated()) {
        AuditTrailListRequest auditTrailListRequest =
            CommonUtils.getAuditTrailRequestForL3History(businessPartnerCode, request.getProductSku(),
                request.getProductName(), UpdateProductActivity.B2B_ACTIVATED.name(),
                Constants.NEED_REVISION_WITHOUT_SPACE, String.valueOf(productBusinessPartner.isB2bActivated()),
                String.valueOf(request.getB2bActivated()), changedBy, requestId, clientHost, Constants.DEFAULT,
                Constants.HYPHEN);
        kafkaProducer.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, request.getProductSku(),
            auditTrailListRequest);
      }
    }
  }

  @Override
  public FbbCreatePickupPointResponse createDefaultFbbPickupPoint(
      FbbCreatePickupPointRequest fbbCreatePickupPointRequest) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(fbbCreatePickupPointRequest.getBusinessPartnerCode()),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(fbbCreatePickupPointRequest.getItemSku()),
        ErrorMessages.ITEM_GDN_SKU_MUST_NOT_BE_BLANK);
    FbbCreatePickupPointResponse fbbCreatePickupPointResponse = new FbbCreatePickupPointResponse();
    fbbCreatePickupPointResponse.setItemSku(fbbCreatePickupPointRequest.getItemSku());
    getFbbPickupPoint(fbbCreatePickupPointRequest, fbbCreatePickupPointResponse);
    if (StringUtils.isNotBlank(fbbCreatePickupPointResponse.getReason())) {
      return fbbCreatePickupPointResponse;
    }
    ProfileResponse profileResponse = this.businessPartnerRepository
        .filterDetailByBusinessPartnerCode(fbbCreatePickupPointRequest.getBusinessPartnerCode());
    RequestHelper.validateForFAASSeller(profileResponse, fbbCreatePickupPointRequest, fbbCreatePickupPointResponse,
        faasFeatureSwitch);
    if (StringUtils.isNotBlank(fbbCreatePickupPointResponse.getReason())) {
      return fbbCreatePickupPointResponse;
    }
    log.info("Creating  default fbb pickup point in x-product with item sku = {} , pickup point code = {} ",
        fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId());
    CreateFbbPickupPointResponse createFbbPickupPointResponse =
        xProductOutbound.createFbbPickupPoint(ConverterUtil.toCreateFbbPickupPointRequest(fbbCreatePickupPointRequest));
    if (StringUtils.isNotBlank(createFbbPickupPointResponse.getReason())) {
      fbbCreatePickupPointResponse.setReason(createFbbPickupPointResponse.getReason());
      fbbCreatePickupPointResponse.setErrorCode(createFbbPickupPointResponse.getErrorCode());
      return fbbCreatePickupPointResponse;
    }
    log.info("Default fbb pickup point in x-product created successfully with item sku = {} , pickup point code = {} ",
        fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId());
    insertInventoryForFbbPickupPoint(fbbCreatePickupPointRequest, fbbCreatePickupPointResponse, createFbbPickupPointResponse, profileResponse);
    return fbbCreatePickupPointResponse;
  }

  @Override
  public List<com.gda.mta.product.dto.response.ProductAndItemPickupPontL5Response> getProductDetailsByItemSkuAndPickupPointCode(
    String storeId, List<ItemSkuPpCodeRequest> itemSkusRequest, boolean needInventoryData) throws Exception {
    List<ProductL5DetailResponse> productL5DetailResponseList =
      getProductL5Details(RequestHelper.toItemPickupPointRequest(itemSkusRequest));
    Map<String, ProductLevel3Inventory> productLevel3InventoryMap = new HashMap<>();
    if (needInventoryData) {
      productLevel3InventoryMap = getProductLevel3InventoryByItemSkuAndPickupPointCode(productL5DetailResponseList);
    }
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap =
      getCampaignPriceByItemSkuAndPickupPointCode(productL5DetailResponseList);
    return ResponseHelper.toProductDetailResponse(productL5DetailResponseList,
        productLevel3InventoryMap, campaignPriceSkuResponseMap, cncForWarehouseFeatureSwitch);
  }

  @Override
  public <T extends MasterDataUpdateRequest> ApiErrorCode validateShippingAndDimensionForEdit(T request, boolean relaxShippingWeightValidation) throws Exception {
    if(!validateEditShippingAndDimensions && !relaxShippingWeightValidation) {
      return null;
    }
    Map<String, Double> dimensionsMap = new HashMap<>();
    dimensionsMap.put(LENGTH, request.getLength());
    dimensionsMap.put(HEIGHT, request.getHeight());
    dimensionsMap.put(WEIGHT, request.getWeight());
    dimensionsMap.put(WIDTH, request.getWidth());
    if(!relaxShippingWeightValidation) {
      dimensionsMap.put(SHIPPING_WEIGHT, request.getShippingWeight());
    }

    String nullDimension =
      dimensionsMap.entrySet().stream().filter(entry -> Objects.isNull(entry.getValue()))
        .map(Map.Entry::getKey).findFirst().orElse(null);
    if (Objects.nonNull(nullDimension)) {
      log.error("Cannot proceed With Edit request for : {}, {} was Null ", request.getProductCode(),
        nullDimension);
      return ApiErrorCode.DIMENSION_LESS_THAN_ZERO;
    }
    if (Optional.of(request).map(T::getProductType)
      .filter(ProductLevel3V2ServiceImpl::isProductTypeValidAndRequiresShippingWeight)
      .isPresent()) {
      return performShippingAndDimensionsValidations(dimensionsMap, request, relaxShippingWeightValidation);
    }
    return null;
  }

  private <T extends MasterDataUpdateRequest> ApiErrorCode performShippingAndDimensionsValidations(
    Map<String, Double> dimensionsMap, T request, boolean relaxShippingWeightValidation) {
    request.setLength(validateAndSetDimensionFieldValues(LENGTH, dimensionsMap.get(LENGTH)));
    request.setWidth(validateAndSetDimensionFieldValues(WIDTH, dimensionsMap.get(WIDTH)));
    request.setHeight(validateAndSetDimensionFieldValues(HEIGHT, dimensionsMap.get(HEIGHT)));
    request.setWeight(validateAndSetDimensionFieldValues(WEIGHT, dimensionsMap.get(WEIGHT)));
    if(!relaxShippingWeightValidation) {
      request.setShippingWeight(validateAndSetDimensionFieldValues(SHIPPING_WEIGHT, dimensionsMap.get(SHIPPING_WEIGHT)));
    }
    double weightInKgs = request.getWeight() / GMS_TO_KG_FACTOR;
    boolean hasZero = relaxShippingWeightValidation ? hasZeroDimension(request.getLength(), request.getWidth(), request.getHeight(),
      request.getWeight()) : hasZeroDimension(request.getLength(), request.getWidth(), request.getHeight(),
      request.getWeight(), request.getShippingWeight());
    if (request.isPureInstoreProduct() && hasZero) {
      request.setLength(0.0);
      request.setWidth(0.0);
      request.setHeight(0.0);
      request.setWeight(0.0);
      request.setShippingWeight(0.0);
    } else if (hasZero) {
      log.error(
        "Validation failed for product creation for request : {} due to zero Dimensions ",
        request);
      return ApiErrorCode.DIMENSION_LESS_THAN_ZERO;
    } else if (hasExceededDimensionLimit(maxProductDimensionLimit, request.getLength(),
      request.getWidth(), request.getHeight(), weightInKgs)) {
      log.error(
        "Validation failed for product creation for request : {} due to Dimensions exceeded ",
        request);
      ApiErrorCode apiErrorCode = ApiErrorCode.DIMENSION_EXCEEDED_THRESHOLD;
      apiErrorCode.setConfiguredValue(maxProductDimensionLimit);
      return apiErrorCode;
    }
    return null;
  }

  private static boolean hasZeroDimension(double... dimensions) {
    for (double dimension : dimensions) {
      if (dimension == 0) {
        return true;
      }
    }
    return false;
  }

  private static boolean hasExceededDimensionLimit(double limit, double... dimensions) {
    for (double dimension : dimensions) {
      if (dimension > limit) {
        return true;
      }
    }
    return false;
  }

  private static double validateAndSetDimensionFieldValues(String fieldName, Double value) {
    double roundedValue = Math.round(value * 1000.0) / 1000.0;
    if (roundedValue <= 0) {
      value = 0.0;
    } else {
      value = roundedValue;
    }
    if (Double.compare(value, 0.0) <= 0) {
      return 0;
    }
    return value;
  }

  private static boolean isProductTypeValidAndRequiresShippingWeight(Integer productType) {
    return (productType.compareTo(1) == 0 || productType.compareTo(2) == 0);
  }

  private Map<String, CampaignPriceSkuResponse> getCampaignPriceByItemSkuAndPickupPointCode(
    List<ProductL5DetailResponse> productL5DetailResponseList) throws Exception {
    CampaignPriceResponse campaignPriceResponse = campaignOutbound.getCampaignPriceInfoV2(
      RequestHelper.toCampaignPriceRequestFromProductDetail(productL5DetailResponseList));
    if (Objects.isNull(campaignPriceResponse)) {
      return new HashMap<>();
    }
    return Optional.ofNullable(campaignPriceResponse.getItemInfoToPriceResponse())
      .orElse(new ArrayList<>()).stream().collect(Collectors.toMap(
        campaignPriceSkuResponse -> com.gdn.partners.pbp.commons.util.CommonUtils.getItemSkuAndPickupPointKey(
          campaignPriceSkuResponse.getItemSku(), campaignPriceSkuResponse.getPickUpPointCode()),
        Function.identity(), (v1, v2) -> v2));
  }

  private List<ProductL5DetailResponse> getProductL5Details(
    List<ItemPickupPointRequest> itemPickupPointRequest) {
    return xProductOutbound.findProductAndItemByItemSkuAndPickupPointCode(itemPickupPointRequest);
  }

  private Map<String, ProductLevel3Inventory> getProductLevel3InventoryByItemSkuAndPickupPointCode(
    List<ProductL5DetailResponse> productL5Details) throws Exception {
    return productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
      RequestHelper.toInventoryDetailInfoRequestList(productL5Details)).stream().collect(
      Collectors.toMap(
        productLevel3Inventory -> com.gdn.partners.pbp.commons.util.CommonUtils.getItemSkuAndPickupPointKey(
          productLevel3Inventory.getWebItemSku(), productLevel3Inventory.getWebPickupPointCode()),
        Function.identity(), (v1, v2) -> v2));
  }

  private void insertInventoryForFbbPickupPoint(FbbCreatePickupPointRequest fbbCreatePickupPointRequest,
      FbbCreatePickupPointResponse fbbCreatePickupPointResponse,
      CreateFbbPickupPointResponse createFbbPickupPointResponse, ProfileResponse profileResponse) {
    try {
      log.info(
          "Calling x-inventory to insert stock for default fbb pickup point with itemSku = {} , pickupPoint code = {} ",
          fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId());
      ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
      productLevel3Inventory.setWebItemSku(fbbCreatePickupPointRequest.getItemSku());
      productLevel3Inventory.setWebPickupPointCode(fbbCreatePickupPointRequest.getPickupPointId());
      productLevel3Inventory.setWebSyncStock(fbbCreatePickupPointRequest.getSynchronizeStock());
      productLevel3Inventory.setWebAvailable(fbbCreatePickupPointRequest.getStock());
      productLevel3Inventory.setWebMerchantCode(fbbCreatePickupPointRequest.getBusinessPartnerCode());
      productLevel3Inventory.setWarehouseItemSku(createFbbPickupPointResponse.getItemCode());
      productLevel3Inventory.setProductSku(createFbbPickupPointResponse.getProductSku());
      productLevel3Inventory.setFbbPP(mppForWhEnabled);
      if (isPurchaseOrderPurchaseTerm(profileResponse)) {
        productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
      } else {
        productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
      }
      productLevel3InventoryService.insertInventory(Collections.singletonList(productLevel3Inventory));
      log.info("Sucessfully added stock for itemSku = {} , pickupPoint code = {} ",
          fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId());
    } catch (Exception e) {
      log.info("Error while inserting stock for default fbb L5 with itemSku = {} ,pickupPoint = {} ",
          fbbCreatePickupPointRequest.getItemSku(), fbbCreatePickupPointRequest.getPickupPointId());
      fbbCreatePickupPointResponse.setReason(ApiErrorCode.FAILED_TO_ADD_STOCK_FOR_DEFAULT_FBB_PICKUP_POINT.getDesc());
      fbbCreatePickupPointResponse.setErrorCode(ApiErrorCode.FAILED_TO_ADD_STOCK_FOR_DEFAULT_FBB_PICKUP_POINT.getCode());
    }
  }

  private boolean isPurchaseOrderPurchaseTerm(ProfileResponse businessPartner) throws Exception {
    return GdnBaseLookup.PURCHASE_TERM_PURCHASE_ORDER.equals(businessPartner.getCompany()
        .getPurchaseTerm());
  }

  private void getFbbPickupPoint(FbbCreatePickupPointRequest fbbCreatePickupPointRequest,
      FbbCreatePickupPointResponse fbbCreatePickupPointResponse) {
    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(fbbCreatePickupPointRequest.getBusinessPartnerCode());
    pickupPointFilterRequest.setFbbActivated(true);
    CommonUtils.setWaitingDeletionForDeletePickupPoint(setWaitingDeletionForDeletePickupPoint,
        pickupPointFilterRequest);
    if (fbbCreatePickupPointRequest.isDefaultWarehouse()) {
      pickupPointFilterRequest.setDefaultWarehouse(true);
    }
    try {
      if (StringUtils.isNotBlank(fbbCreatePickupPointRequest.getPpCode())) {
        fbbCreatePickupPointRequest.setPickupPointId(fbbCreatePickupPointRequest.getPpCode());
        fbbCreatePickupPointResponse.setPickupPointId(fbbCreatePickupPointRequest.getPpCode());
      } else {
        PickupPointResponse defaultFbbPickupPoint =
            businessPartnerRepository.filterPickupPointsByPickupPointRequest(pickupPointFilterRequest).get(0);
        fbbCreatePickupPointRequest.setPickupPointId(defaultFbbPickupPoint.getCode());
        fbbCreatePickupPointResponse.setPickupPointId(defaultFbbPickupPoint.getCode());
        if (ranchIntegrationEnabled && Boolean.TRUE.equals(
            Optional.ofNullable(defaultFbbPickupPoint.getFlags()).orElse(new HashMap<>())
                .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))) {
          fbbCreatePickupPointResponse.setReason(ApiErrorCode.FAILED_TO_FETCH_DEFAULT_FBB_PICKUP_POINT.getDesc());
          fbbCreatePickupPointResponse.setErrorCode(ApiErrorCode.FAILED_TO_FETCH_DEFAULT_FBB_PICKUP_POINT.getCode());
        }
      }
    } catch (Exception e) {
      log.error("Error while fetching default fbb pickup point for item sku =  {} ",
          fbbCreatePickupPointRequest.getItemSku(), e);
      fbbCreatePickupPointResponse.setReason(ApiErrorCode.FAILED_TO_FETCH_DEFAULT_FBB_PICKUP_POINT.getDesc());
      fbbCreatePickupPointResponse.setErrorCode(ApiErrorCode.FAILED_TO_FETCH_DEFAULT_FBB_PICKUP_POINT.getCode());
    }
  }

  @Override
  public ItemsPriceStockImagesUpdateResponse validateL5UpdateRequest(
      ProductVariantUpdateRequest productVariantUpdateRequest, ProfileResponse profileResponse) throws Exception {
    return productLevel3Service.validateL5UpdateRequest(productVariantUpdateRequest, profileResponse);
  }

  @Override
  public ProductEditValidationDTO validationsForEdit(String requestId, ProductL3UpdateRequest request,
      ProductLevel3 productLevel3Edit, EditProductResponse editResponse, ProductL3Response savedProductData,
    ProfileResponse profileResponse) throws Exception {
    List<ApiErrorCode> apiErrorCodeList = new ArrayList<>();
    request.setB2cActivated(BooleanUtils.toBooleanDefaultIfNull(request.getB2cActivated(), true));
    ValidationUtil.validateDescriptiveFieldsForProductUpdate(productLevel3Edit);
    validateProductEditInfo(request, savedProductData);
    productLevel3Service.processSellerPenaltyChecks(
      ProductVariantUpdateRequest.builder().productItems(request.getProductItems())
        .addPickupPoints(request.getAddPickupPoints()).build(), profileResponse,
      sellerPenaltyEnabledPhase2);
    editResponse.setProfileResponse(profileResponse);
    GdnRestSingleResponse<EditProductResponse> L5ValidationEditResponse =
        validateL5RequestAndReturn(requestId, request, editResponse);
    if (Objects.nonNull(L5ValidationEditResponse)) {
      log.info("L5 validation failed for product : productCode {} productSku : {} ",
          productLevel3Edit.getProductCode(), productLevel3Edit.getProductSku());
      return ProductEditValidationDTO.builder().editProductResponse(editResponse).L5ValidationFailed(true).build();
    }
    ApiErrorCode apiErrorCode = validateNewProductItemRequest(request);
    if (Objects.nonNull(apiErrorCode)) {
      apiErrorCodeList.add(apiErrorCode);
    }

    apiErrorCode = validateNewProductItemImageRequest(request, savedProductData);
    if (Objects.nonNull(apiErrorCode)) {
      apiErrorCodeList.add(apiErrorCode);
    }

    apiErrorCode = validateShippingAndDimensionForEdit(request, false);
    if (Objects.nonNull(apiErrorCode)) {
      apiErrorCodeList.add(apiErrorCode);
    }

    preOrderValidation(request, profileResponse, apiErrorCodeList);

    apiErrorCode = validateLocationPath(request);
    if (Objects.nonNull(apiErrorCode)) {
      apiErrorCodeList.add(apiErrorCode);
    }
    EligibilitySchedulesForEdit eligibilitySchedulesForEdit = new EligibilitySchedulesForEdit(true);
    ApiErrorCode eligibilityForSchedulesAtL5 =
      ValidationUtil.validateEligibilityForSchedulesAtL5(request,
        fetchActiveSchedulesFromValidations(request, eligibilitySchedulesForEdit), savedProductData, schedulesAddEditEnabled);
    if (Objects.nonNull(eligibilityForSchedulesAtL5)) {
      log.error("Validations Failed for Addition of Schedules for product : {} , error : {} ",
        request.getProductSku(), eligibilityForSchedulesAtL5.getDesc());
      apiErrorCodeList.add(eligibilityForSchedulesAtL5);
    }
    if (!eligibilitySchedulesForEdit.isEligibilitySchedulesForEdit()) {
      log.error("Validations Failed for Addition of Schedules for product : {} , error : {} ", request.getProductSku(),
          ApiErrorCode.SCHEDULE_SUPPORTED_ONLY_FOR_OFFLINE_L5.getDesc());
      apiErrorCodeList.add(ApiErrorCode.SCHEDULE_SUPPORTED_ONLY_FOR_OFFLINE_L5);
    }
    ApiErrorCode validateVariantRestrictedValues = validateVariantRestrictedValues(request);
    if (Objects.nonNull(validateVariantRestrictedValues)) {
      apiErrorCodeList.add(validateVariantRestrictedValues);
    }
    ApiErrorCode validateWholesaleDiscount = validateWholeSaleDiscountPercentage(request);
    if (Objects.nonNull(validateWholesaleDiscount)) {
      log.error("wholesale discount for product is more than 100 percent : {} ", request.getProductSku());
      apiErrorCodeList.add(validateWholesaleDiscount);
    }
    if (CollectionUtils.isNotEmpty(apiErrorCodeList) && apiErrorCodeList.stream().anyMatch(Objects::nonNull)) {
      editResponse.setApiErrorCode(apiErrorCodeList.stream().findFirst().get());
    }
    distributionInfoValidation(request, editResponse);
    return ProductEditValidationDTO.builder().editProductResponse(editResponse).L5ValidationFailed(false)
        .productL3UpdateRequest(request).productLevel3(productLevel3Edit).build();
  }

  private void preOrderValidation(ProductL3UpdateRequest request, ProfileResponse profileResponse,
      List<ApiErrorCode> apiErrorCodeList) throws Exception {
    if (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())) {
      boolean isSellerOmg = CommonUtils.getBusinessPartnerFlagValue(profileResponse, Constants.BLIBLI_OMG);
      ApiErrorCode validationResponseForPreOrder =
          ValidationUtil.getValidationResponseForPreOrder(request.getPreOrder(), preOrderMaximumDays,
              preOrderMaximumWeek, isSellerOmg, preOrderConfig.isPoQuotaFeatureSwitch());
      if (Objects.nonNull(validationResponseForPreOrder)) {
        apiErrorCodeList.add(validationResponseForPreOrder);
      }
    }
  }

  private void distributionInfoValidation(ProductL3UpdateRequest request, EditProductResponse editResponse) throws Exception {
    if (ranchIntegrationEnabled) {
      if (MapUtils.isNotEmpty(request.getDistributionInfoRequest()) || CollectionUtils.isNotEmpty(
          request.getDistributionAndUOMRequest())) {
        DistributionInfoRequest distributionInfoRequest = new DistributionInfoRequest();
        distributionInfoRequest.setSellerCode(request.getMerchantCode());
        distributionInfoRequest.setDistributionInfoRequest(request.getDistributionInfoRequest());
        distributionInfoRequest.setProductItems(request.getDistributionAndUOMRequest());
        Map<String, String> skuCodeToMerchantSku = new HashMap<>();
        if (CollectionUtils.isNotEmpty(request.getProductItems())) {
          skuCodeToMerchantSku = Optional.ofNullable(request.getProductItems()).orElse(new ArrayList<>()).stream()
              .collect(toMap(ProductVariantPriceStockAndImagesRequest::getSkuCode,
                  ProductVariantPriceStockAndImagesRequest::getMerchantSku, (a, b) -> a));
        }
        setOmniChannelSku(request, skuCodeToMerchantSku);
        List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
        boolean distributionInfoUpdated =
            distributionInfoService.validateUomInfo(request.getProductCode(), distributionInfoRequest,
                auditTrailDtoList);
        editResponse.setDistributionInfoUpdated(distributionInfoUpdated);
        editResponse.setAuditTrailDtoList(auditTrailDtoList);
      }
    }
  }

  private static void setOmniChannelSku(ProductL3UpdateRequest request, Map<String, String> skuCodeToMerchantSku) {
    if (CollectionUtils.isNotEmpty(request.getDistributionAndUOMRequest())) {
      for (ProductItemDistributionInfoRequest productItemDistributionInfoRequest : request.getDistributionAndUOMRequest()) {
        if (Objects.nonNull(productItemDistributionInfoRequest.getDistributionItemInfoRequest()) && StringUtils.isBlank(
            productItemDistributionInfoRequest.getDistributionItemInfoRequest().getOmniChannelSku())) {
          productItemDistributionInfoRequest.getDistributionItemInfoRequest()
              .setOmniChannelSku(skuCodeToMerchantSku.get(productItemDistributionInfoRequest.getSkuCode()));
        }
      }
    }
  }

  private static ApiErrorCode validateWholeSaleDiscountPercentage(ProductL3UpdateRequest request) {
    return hasInvalidDiscountForModifiedPPCodes(request) || hasInvalidDiscountForAddPPCodes(request) ?
        ApiErrorCode.WHOLESALE_DISCOUNT_SHOULD_BE_BETWEEN_0_AND_100 :
        null;
  }

  private static boolean hasInvalidDiscountForAddPPCodes(ProductL3UpdateRequest request) {
    return request.getAddPickupPoints().stream().flatMap(
        pickupPoint -> Optional.ofNullable(pickupPoint.getProductItemWholesalePriceRequests())
            .orElse(Collections.emptyList()).stream()).anyMatch(
        priceRequest -> priceRequest.getWholesaleDiscount() > WHOLESALE_DISCOUNT_MAX_THRESHOLD
            || priceRequest.getWholesaleDiscount() < WHOLESALE_DISCOUNT_MIN_THRESHOLD);
  }

  private static boolean hasInvalidDiscountForModifiedPPCodes(ProductL3UpdateRequest request) {
    return request.getProductItems().stream().flatMap(
            item -> Optional.ofNullable(item.getModifiedItemPickupPoints()).orElse(Collections.emptyList()).stream())
        .flatMap(pickupPoint -> Optional.ofNullable(pickupPoint.getProductItemWholesalePriceRequests())
            .orElse(Collections.emptyList()).stream()).anyMatch(
            priceRequest -> priceRequest.getWholesaleDiscount() > WHOLESALE_DISCOUNT_MAX_THRESHOLD
                || priceRequest.getWholesaleDiscount() < WHOLESALE_DISCOUNT_MIN_THRESHOLD);
  }

  private Map<String, Set<String>> fetchActiveSchedulesFromValidations(
    ProductL3UpdateRequest requestsForSchedulesValidations, EligibilitySchedulesForEdit eligibilitySchedulesForEdit) {
    List<ItemPickupPointRequest> existingSchedulesFromRequest =
      CommonUtils.getExistingSchedulesFromRequest(requestsForSchedulesValidations);
    if (schedulesAddEditEnabled && CollectionUtils.isNotEmpty(existingSchedulesFromRequest)) {
      if (isInvalidSchedulesForAddPickupPoints(requestsForSchedulesValidations, cncForWarehouseFeatureSwitch)) {
        return updateEligibilityForSchedules(eligibilitySchedulesForEdit);
      }

      List<ItemPickupPointBasicResponse> itemPickupPointBasicResponses =
        xProductOutbound.fetchBasicDetailsByItemSkuAndPickupPointCodeList(
          existingSchedulesFromRequest);
      if (isInvalidSchedulesForModifiedPickupPoints(itemPickupPointBasicResponses, requestsForSchedulesValidations, cncForWarehouseFeatureSwitch)) {
        return updateEligibilityForSchedules(eligibilitySchedulesForEdit);
      }
      if (cncForWarehouseFeatureSwitch) {
        checkCncStatusAndUpdateSchedulesAddPickupPoints(requestsForSchedulesValidations);
        checkCncStatusAndUpdateSchedulesModifiedPickupPoints(requestsForSchedulesValidations);
      }
      // Get item pickup point requests with active schedules
      return CommonUtils.fetchL5WithActiveAndUnModifiedSchedules(itemPickupPointBasicResponses,
        requestsForSchedulesValidations);
    }
    return new HashMap<>();
  }

  private static HashMap<String, Set<String>> updateEligibilityForSchedules(
      EligibilitySchedulesForEdit eligibilitySchedulesForEdit) {
    eligibilitySchedulesForEdit.setEligibilitySchedulesForEdit(false);
    return new HashMap<>();
  }

  private static boolean isInvalidSchedulesForModifiedPickupPoints(List<ItemPickupPointBasicResponse> itemPickupPointBasicResponses,
      ProductL3UpdateRequest requestsForSchedulesValidations, boolean cncForWarehouseFeatureSwitch) {

    // Update item SKUs in the request
    requestsForSchedulesValidations.getProductItems().forEach(
      productVariant -> productVariant.getModifiedItemPickupPoints()
        .forEach(pickupPoint -> pickupPoint.setItemSku(productVariant.getItemSku())));

    // L5's that are offline and have schedules from the request
    Set<String> offlineDeliveryOrValidCncVariantsWithScheduleFromRequest =
      requestsForSchedulesValidations.getProductItems().stream()
        .flatMap(productVariant -> productVariant.getModifiedItemPickupPoints().stream())
          .filter(pickupPoint -> !pickupPoint.isDisplay() && !pickupPoint.isBuyable() &&
          //If switch is off, 2nd half of the check will be ignored, otherwise will be used for filtering
              (!cncForWarehouseFeatureSwitch || Objects.equals(pickupPoint.isCncDisplay(), pickupPoint.isCncBuyable())))
          .filter(itemPickupPointRequest -> StringUtils.isNotBlank(itemPickupPointRequest.getItemSku()))
          .map(pickupPoint -> pickupPoint.getItemSku().concat(Constants.HYPHEN)
            .concat(pickupPoint.getPickupPointId())).collect(Collectors.toSet());

    // Skip L5's from saved view config where L5 has been made offline in the request
    return itemPickupPointBasicResponses.stream().filter(Objects::nonNull).filter(
        response -> !offlineDeliveryOrValidCncVariantsWithScheduleFromRequest.contains(
          response.getItemSku().concat(Constants.HYPHEN).concat(response.getPickupPointCode())))
      .anyMatch(itemPickupPointBasicResponse -> isDeliveryAndCncViewConfigValid(itemPickupPointBasicResponse, cncForWarehouseFeatureSwitch));
  }

  private static boolean isDeliveryAndCncViewConfigValid(ItemPickupPointBasicResponse itemPickupPointBasicResponse,
      boolean cncForWarehouseFeatureSwitch) {
    ViewConfigResponse viewConfigResponse =
        Optional.ofNullable(itemPickupPointBasicResponse.getViewConfigResponse()).orElse(new ViewConfigResponse());
    ItemViewConfigDTO cncItemViewConfigDTO =
        Optional.ofNullable(itemPickupPointBasicResponse.getAllItemViewConfigDTO()).orElse(new ArrayList<>()).stream()
            .filter(itemViewConfigDTO -> Constants.CNC_CHANNEL.equals(itemViewConfigDTO.getChannel())).findFirst()
            .orElse(new ItemViewConfigDTO());
    return (Constants.DEFAULT.equals(viewConfigResponse.getChannelId()) && (
        viewConfigResponse.isBuyable() || viewConfigResponse.isDisplay()))
        || (cncForWarehouseFeatureSwitch
        && !Objects.equals(cncItemViewConfigDTO.isBuyable(), viewConfigResponse.isDisplay()));
  }


  private static boolean isInvalidSchedulesForAddPickupPoints(ProductL3UpdateRequest requestsForSchedulesValidations,
      boolean cncForWarehouseFeatureSwitch) {
    return requestsForSchedulesValidations.getAddPickupPoints().stream().filter(Objects::nonNull).anyMatch(
        itemPickupPointRequest -> (Objects.nonNull(itemPickupPointRequest.getBuyableSchedule()) || Objects.nonNull(
            itemPickupPointRequest.getDiscoverableSchedule()))
            &&
            //Schedules will be invalid if default view config != OFFLINE OR cnc view config is not ONLINE/OFFLINE
            ((itemPickupPointRequest.isBuyable() || itemPickupPointRequest.isDisplay())
                || (cncForWarehouseFeatureSwitch && !Objects.equals(itemPickupPointRequest.isCncDisplay(), itemPickupPointRequest.isCncBuyable()))));
  }

  private void checkCncStatusAndUpdateSchedulesAddPickupPoints(ProductL3UpdateRequest requestsForSchedulesValidations) {
    requestsForSchedulesValidations.getAddPickupPoints().stream().filter(Objects::nonNull)
        .forEach(CommonUtils::updateItemPickupPointSchedules);
  }

  private void checkCncStatusAndUpdateSchedulesModifiedPickupPoints(ProductL3UpdateRequest requestsForSchedulesValidations) {
    requestsForSchedulesValidations.getProductItems().stream()
        .flatMap(productItem -> productItem.getModifiedItemPickupPoints().stream())
        .forEach(CommonUtils::updateItemPickupPointSchedules);
  }

  private ApiErrorCode validateNewProductItemRequest(ProductL3UpdateRequest productL3UpdateRequest) {
    if (validateNewProductItemL5Request) {
      List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests =
          Optional.ofNullable(productL3UpdateRequest.getProductItems()).orElseGet(() -> new ArrayList<>());
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantPriceStockAndImagesRequests) {
        if (productVariantPriceStockAndImagesRequest.isNewlyAddedItem() && CollectionUtils.isEmpty(
            productVariantPriceStockAndImagesRequest.getModifiedItemPickupPoints())) {
          log.error("New product item request is invalid. Found item without pickup point details : {} ",
              productVariantPriceStockAndImagesRequest);
          return ApiErrorCode.ITEM_PICKUP_POINT_NOT_FOUND_FOR_NEWLY_ADDED_VARIANTS;
        }
      }
    }
    return null;
  }

  private ApiErrorCode validateVariantRestrictedValues(ProductL3UpdateRequest productL3UpdateRequest) {
    if (variantRestrictedValuesFlag) {
      List<ProductVariantPriceStockAndImagesRequest> productVariantPriceStockAndImagesRequests =
          Optional.ofNullable(productL3UpdateRequest.getProductItems()).orElseGet(() -> new ArrayList<>());
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productVariantPriceStockAndImagesRequests) {
        if (productVariantPriceStockAndImagesRequest.isNewlyAddedItem()
            && productVariantPriceStockAndImagesRequest.getAttributesMap().values().stream().map(String::toLowerCase)
            .anyMatch(variantRestrictedValues::contains)) {
          log.error("Restricted attribute value present in update request : {} ", productL3UpdateRequest);
          return ApiErrorCode.ITEM_ATTRIBUTE_VALUE_INVALID;
        }
      }
    }
    return null;
  }

  private ApiErrorCode validateNewProductItemImageRequest(ProductL3UpdateRequest productL3UpdateRequest,
      ProductL3Response savedProductData) {
    if (validateNewProductItemImageRequest) {
      boolean isCommonMainImageNotPresentInSavedProduct = isCommonMainImageNotPresentInSavedProduct(savedProductData);
      boolean isCommonImageNotPresentInRequest = CollectionUtils.isEmpty(productL3UpdateRequest.getCommonImages());
      for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : productL3UpdateRequest.getProductItems()) {
        if (productVariantPriceStockAndImagesRequest.isNewlyAddedItem() && isCommonMainImageNotPresentInSavedProduct
            && isCommonImageNotPresentInRequest && CollectionUtils.isEmpty(
            productVariantPriceStockAndImagesRequest.getImages())) {
          log.error("New product item request is invalid. Found item without common images or variant images : {} ",
              productVariantPriceStockAndImagesRequest);
          return ApiErrorCode.NO_IMAGE_FOUND_FOR_NEWLY_ADDED_VARIANT;
        }
      }
    }
    return null;
  }

  private boolean isCommonMainImageNotPresentInSavedProduct(ProductL3Response savedProductData) {
    return Optional.ofNullable(savedProductData).map(ProductL3Response::getMasterDataProduct)
        .map(MasterDataProductDTO::getMasterDataProductImages).orElseGet(() -> new ArrayList<>()).stream()
        .filter(MasterDataProductImageDTO::isCommonImage).noneMatch(MasterDataProductImageDTO::isMainImage);
  }


  private ApiErrorCode validateLocationPath(ProductL3UpdateRequest request) {
    boolean variantLocationPath = request.getProductItems().stream().anyMatch(
        variantRequest -> RequestHelper.hasInvalidImagePath(request.getProductCode(), variantRequest.getImages(),
            productCodeLocationValidate, validateImagePathEnabled));
    boolean commonImageLocationPath =
        RequestHelper.hasInvalidImagePath(request.getProductCode(), request.getCommonImages(),
            productCodeLocationValidate, validateImagePathEnabled);
    if (commonImageLocationPath || variantLocationPath) {
      return ApiErrorCode.INVALID_LOCATION_PATH;
    }
    return null;
  }

  @Override
  public ProductEditValidationDTO validationForProductL3ResponseAndNeedRevisionUpdate(ProductLevel3 product,
      EditProductResponse editProductResponse, ProfileResponse profileResponse,
      boolean combineContentAndLogisticsPcbUpdate, ProductL3Response savedProductData,
      List<PickupPointDeleteRequest> pickupPointDeleteRequests, ProductL3UpdateRequest productL3UpdateRequest, boolean addingPickupPoints, boolean isPureExternalUser)
      throws Exception {
    ProductEditValidationDTO productEditValidationDTO =
        ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productLevel3(product)
            .productL3Response(null).build();
    CategoryDetailResponse categoryDetailResponse =
        this.productOutboundBean.getCategoryDetailByCategoryCode(product.getCategoryCode());
    ApiErrorCode bpBopisApiErrorCode =
        productService.checkBpBopisEligibility(product.getProductType(), profileResponse, categoryDetailResponse, productL3UpdateRequest, isPureExternalUser);
    if (Objects.nonNull(bpBopisApiErrorCode)) {
      productEditValidationDTO.getEditProductResponse().setApiErrorCode(bpBopisApiErrorCode);
      return productEditValidationDTO;
    }
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID,
            product.getProductCode());
    editProductResponse.setProductCollection(productCollection);

    if (brandCategoryEditEnabledForExternal) {
      updateCategory(product, editProductResponse, categoryDetailResponse);
      validateBrandAuthorisationAndStatus(product, editProductResponse);
    }

    // fetching details from x-product, for need revision this fetch is not required as product
    // might not be there in x-product
    if (!product.isNeedCorrection()) {
      if (Objects.isNull(savedProductData)) {
        savedProductData = getProductL3ResponseFromXProduct(product.getProductSku());
      }
    }
    if (productNameEditValidationSwitch || brandCategoryEditEnabledForExternal) {
      ApiErrorCode apiErrorCode =
          validateProductNameAndBrandOrderCheckInEdit(product, savedProductData, profileResponse, productCollection);
      if (Objects.nonNull(apiErrorCode)) {
        productEditValidationDTO.getEditProductResponse().setApiErrorCode(apiErrorCode);
        return productEditValidationDTO;
      }
    }

    if (product.isNeedCorrection()) {
      log.info("product content revised for productSku : {} product : {} ", product.getProductSku(), product);
      editProductResponse = productLevel3ServiceBean.saveNeedCorrectionChangesInPCB(product, pickupPointDeleteRequests, addingPickupPoints,
          savedProductData, productL3UpdateRequest, editProductResponse.getProductCollection());
      return ProductEditValidationDTO.builder().editProductResponse(editProductResponse).needCorrection(true).productLevel3(product).build();
    }
    if (brandCategoryEditEnabledForExternal) {
      savedProductData.getMasterCatalog()
          .setCategory(new CategoryDTO(product.getCategoryCode(), product.getCategoryCode()));
      savedProductData.getMasterDataProduct().setBrand(product.getBrand());
    }
    if (validateYoutubeUrlNewFlow) {
      validateYoutubeUrlIsValid(productL3UpdateRequest, savedProductData);
      product.setUrl(productL3UpdateRequest.getUrl());
    }
    ApiErrorCode apiErrorCode = productLevel3ServiceBean.checkProductStatus(savedProductData);
    if (Objects.nonNull(apiErrorCode)) {
      productEditValidationDTO.getEditProductResponse().setApiErrorCode(apiErrorCode);
      return productEditValidationDTO;
    }
    apiErrorCode = productLevel3ServiceBean.checkProductVersion(product, savedProductData);
    if (Objects.nonNull(apiErrorCode)) {
      productEditValidationDTO.getEditProductResponse().setApiErrorCode(apiErrorCode);
      return productEditValidationDTO;
    }

    ProductLevel3UpdateRequest productLevel3UpdateRequest = new ProductLevel3UpdateRequest();
    if (combineContentAndLogisticsPcbUpdate) {
      BeanUtils.copyProperties(product, productLevel3UpdateRequest, "productEditable");
      apiErrorCode = productLevel3ServiceBean.validateShippingType(productLevel3UpdateRequest);
      if (Objects.nonNull(apiErrorCode)) {
        productEditValidationDTO.getEditProductResponse().setApiErrorCode(apiErrorCode);
        return productEditValidationDTO;
      }
    }

    if (addDeleteVariantsValidationSwitch) {
      apiErrorCode =
          CommonUtils.validateAddDeleteVariantsRequest(product, savedProductData, addDeleteForWarehouseSwitch,
              profileResponse, categoryDetailResponse, validateAttributeAtProductAndCategory,
              getExistingProductAttributeDetails(product, editProductResponse), deleteVariantValidationSwitch,
              uniqueValueTypeAdditionEnabled, ranchIntegrationEnabled);
    }
    apiErrorCode = validatePreOrder(productL3UpdateRequest, apiErrorCode);
    apiErrorCode = validateSyncStockForFaas(profileResponse, productL3UpdateRequest, apiErrorCode);
    if (Objects.nonNull(apiErrorCode)) {
      productEditValidationDTO.getEditProductResponse().setApiErrorCode(apiErrorCode);
      return productEditValidationDTO;
    }
    productLevel3ServiceBean.migrateUnsyncProductForEdit(savedProductData, syncProductDataOnMasterDataUpdate);
    if (!brandCategoryEditEnabledForExternal) {
      productLevel3ServiceBean.brandChangeCheck(savedProductData.getMasterDataProduct().getBrand(),
          product.getBrand());
    }
    return ProductEditValidationDTO.builder().editProductResponse(editProductResponse).productLevel3(product)
        .productL3Response(savedProductData).build();
  }

  private ApiErrorCode validateSyncStockForFaas(ProfileResponse profileResponse, ProductL3UpdateRequest productL3UpdateRequest,
      ApiErrorCode apiErrorCode) {
    if (faasFeatureSwitch && Objects.isNull(apiErrorCode)) {
      apiErrorCode =
        CommonUtils.validateSyncStockForFAASMerchantsInEditRequest(productL3UpdateRequest, profileResponse,
            apiErrorCode);
    }

    return apiErrorCode;
  }

  private ApiErrorCode validatePreOrder(ProductL3UpdateRequest productL3UpdateRequest, ApiErrorCode apiErrorCode) {
    if (preOrderConfig.isPoQuotaFeatureSwitch() && productL3UpdateRequest.isSellerOmg() && Objects.isNull(apiErrorCode) && Objects.nonNull(
        productL3UpdateRequest.getPreOrder()) && Boolean.TRUE.equals(
        productL3UpdateRequest.getPreOrder().getIsPreOrder())) {
      apiErrorCode = CommonUtils.validatePreOrderStockForEdit(productL3UpdateRequest.getAddPickupPoints(),
          productL3UpdateRequest.getPreOrder(), apiErrorCode);
      if (Objects.isNull(apiErrorCode)) {
        for (ProductVariantPriceStockAndImagesRequest item : productL3UpdateRequest.getProductItems()) {
          apiErrorCode = CommonUtils.validatePreOrderStockForEdit(item.getModifiedItemPickupPoints(),
              productL3UpdateRequest.getPreOrder(), null);
          if (apiErrorCode != null) {
            break;
          }
        }
      }
    }
    return apiErrorCode;
  }

  private void validateBrandAuthorisationAndStatus(ProductLevel3 product,
      EditProductResponse editProductResponse) throws Exception {
    ProductCollection productCollection = editProductResponse.getProductCollection();
    if (CommonUtils.isBrandNameUpdated(product.getBrand(), productCollection.getBrand())) {
      product.setBrandUpdated(true);
      product.setOldBrandName(productCollection.getBrand());
      String storeId = mandatoryParameterHelper.getStoreId();
      String channelId = mandatoryParameterHelper.getChannelId();
      String clientId = mandatoryParameterHelper.getClientId();
      String requestId = mandatoryParameterHelper.getRequestId();
      String username = mandatoryParameterHelper.getUsername();
      String brand = product.getBrand();
      String brandCode = product.getBrandCode();
      String businessPartnerCode = product.getBusinessPartnerCode();
      GdnRestSingleResponse<BrandResponse> brandResponse =
          pcbFeign.filterByBrandName(storeId, channelId, clientId, requestId, username,
              product.getBrand(), false, true);
      if (Objects.isNull(brandResponse) || Objects.isNull(brandResponse.getValue())) {
        BrandWipResponse brandWipResponse =
            createProductWorkflowWorkerBean.getBrandWipResponseAndValidateBrandStatus(brandCode,
                brand, product.getBusinessPartnerCode());
        product.setBrand(brandWipResponse.getBrandName());
        product.setBrand(brandWipResponse.getBrandCode());
        productCollection.setBrandApprovalStatus(BrandApprovalStatus.DRAFT);
      } else {
        createProductWorkflowWorkerBean.checkForProtectionBrandAndAuthorisation(
            storeId, brandResponse, businessPartnerCode, channelId, clientId,
            requestId, username);
        product.setBrand(brandResponse.getValue().getBrandName());
        product.setBrandCode(brandResponse.getValue().getBrandCode());
        productCollection.setBrandApprovalStatus(BrandApprovalStatus.APPROVED);
      }
      productCollection.setBrand(product.getBrand());
      productCollection.setBrandCode(product.getBrandCode());
      editProductResponse.setProductCollection(productCollection);
    }
  }

  private void updateCategory(ProductLevel3 product, EditProductResponse editProductResponse,
      CategoryDetailResponse categoryDetailResponse) {
    ProductCollection productCollection = editProductResponse.getProductCollection();
    if (CommonUtils.isCategoryCodeUpdated(product.getCategoryCode(), productCollection.getCategoryCode())) {
      product.setOldCategoryName(productCollection.getCategoryName());
      product.setCategoryId(categoryDetailResponse.getId());
      product.setCategoryName(categoryDetailResponse.getName());
      productCollection.setCategoryCode(categoryDetailResponse.getCategoryCode());
      productCollection.setCategoryName(categoryDetailResponse.getName());
      product.setCategoryUpdated(true);
      editProductResponse.setProductCollection(productCollection);
    }
  }

  private AttributeCodeValueValueTypeDetails getExistingProductAttributeDetails(ProductLevel3 productReqeust,
      EditProductResponse editProductResponse) throws Exception {
    AttributeCodeValueValueTypeDetails attributeCodeValueValueTypeDetails = new AttributeCodeValueValueTypeDetails();
    if (valueTypeAdditionForDefiningAttributes) {
      ProductCollection productCollection = Optional.ofNullable(editProductResponse.getProductCollection()).orElseGet(
          () -> productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID,
              productReqeust.getProductCode()));
      Product product = this.productRepository.findOne(productCollection.getProductId());
      attributeCodeValueValueTypeDetails.setExistingAttributeCodeValueAndValueTypeMap(ValueTypeUtil
          .getAttributeCodeValueAndValueTypeMap(product));
      editProductResponse.setProduct(product);
      editProductResponse.setProductCollection(productCollection);
    }
    attributeCodeValueValueTypeDetails.setSizeChartValueTypeDelimiter(sizeChartValueTypeDelimiter);
    attributeCodeValueValueTypeDetails.setValueTypeAdditionForDefiningAttributes(valueTypeAdditionForDefiningAttributes);
    return attributeCodeValueValueTypeDetails;
  }

  private ApiErrorCode validateProductNameAndBrandOrderCheckInEdit(ProductLevel3 productLevel3, ProductL3Response productL3Response,
      ProfileResponse profileResponse, ProductCollection productCollection) {
    ApiErrorCode apiErrorCode = null;
    String productName =
        Optional.ofNullable(productL3Response).map(ProductL3Response::getMasterDataProduct)
            .map(MasterDataProductDTO::getProductName).orElse(productCollection.getProductName());
    String editedProductName = productLevel3.getProductName();
    boolean isOfficialSeller = Optional.of(profileResponse.isOfficial()).orElse(false);
    boolean isNameChanged = !validateProductNames(productName, editedProductName) && !isOfficialSeller;
    boolean isBrandChanged = brandCategoryEditEnabledForExternal && productLevel3.isBrandUpdated();
    boolean isCategoryChanged = brandCategoryEditEnabledForExternal && productLevel3.isCategoryUpdated();
    try {
      if (isNameChanged || isBrandChanged || isCategoryChanged) {
        AgpSimpleQueryResponse agpResponse =
            agpQueryFeign.findNumberOfOrder(productLevel3.getProductSku(), String.valueOf(0), String.valueOf(0),
                Constants.AGP_ITEM_STATUS);
        if (Objects.nonNull(agpResponse) && Objects.nonNull(agpResponse.getHits()) && validateOrderForProduct(agpResponse)) {
            if (isNameChanged) {
              apiErrorCode = ApiErrorCode.NAME_EDIT_NOT_ALLOWED;
            } else if (isBrandChanged) {
              apiErrorCode = ApiErrorCode.BRAND_EDIT_NOT_ALLOWED_SINCE_ORDER_IS_PRESENT;
            }
            else {
              apiErrorCode = ApiErrorCode.CATEGORY_EDIT_NOT_ALLOWED_SINCE_ORDER_IS_PRESENT;
            }
        }
      }
    } catch (Exception e) {
      log.error("Error while calling AGP to get order details for productSku : {} , error - ",
          productLevel3.getProductSku(), e);
    }
    return apiErrorCode;
  }

  @Override
  public ApiErrorCode validateExistingOrderOnProductNameEdit(ProductMasterDataEditRequest productMasterDataEditRequest) {
    ApiErrorCode apiErrorCode = null;
    try {
      if (productMasterDataEditRequest.getMasterDataEditChangeTypes()
        .contains(L3InfoUpdateChangeType.PRODUCT_NAME_UPDATE)
        && !productMasterDataEditRequest.isOfficialStoreSeller()) {
        AgpSimpleQueryResponse agpResponse =
          agpQueryFeign.findNumberOfOrder(productMasterDataEditRequest.getProductSku(),
            String.valueOf(Constants.ZERO), String.valueOf(Constants.ZERO),
            Constants.AGP_ITEM_STATUS);
        if (Objects.nonNull(agpResponse) && Objects.nonNull(agpResponse.getHits())) {
          apiErrorCode =
            validateOrderForProduct(agpResponse) ? ApiErrorCode.NAME_EDIT_NOT_ALLOWED : null;
        }
      }
    } catch (Exception e) {
      log.error("Error while calling AGP to get order details for productSku : {} , error - ",
        productMasterDataEditRequest.getProductSku(), e);
    }
    return apiErrorCode;
  }

  public boolean validateProductNames(String existingProductName, String requestedProductName) {
    if (removeAllExtraSpaceBeforeNameEditValidation) {
      String existingProductNameWithoutSpaces =
          getTextWithoutTags(PATTERN_FOR_EXTRA_SPACE, existingProductName, StringUtils.EMPTY);
      String requestedProductNameWithoutSpaces =
          getTextWithoutTags(PATTERN_FOR_EXTRA_SPACE, requestedProductName, StringUtils.EMPTY);
      return existingProductNameWithoutSpaces.equals(requestedProductNameWithoutSpaces);
    }
    return existingProductName.equals(requestedProductName);
  }

  private boolean validateOrderForProduct(AgpSimpleQueryResponse response) {
    return response.getHits().getTotal() > 0;
  }

  @Override
  public ProductL3Response getProductL3ResponseFromXProduct(String productSku) throws Exception {
    ProductL3Response savedProductData =
        xProductOutbound.getProductDetailsByProductSku(productSku).getValue();
    return savedProductData;
  }

  @Override
  public Map<String, String> getAttributeCodeAndIdMap(boolean fetchOnlyBasicAttributeDetails,
      AttributeCodesRequest attributeCodesRequest) {
    List<AttributeResponse> attributeResponseList =
        productOutbound.getAttributeDetailByAttributeCodes(fetchOnlyBasicAttributeDetails, attributeCodesRequest);
    return attributeResponseList.stream().collect(
        Collectors.toMap(AttributeResponse::getAttributeCode, AttributeResponse::getId,
            (oldValue, newValue) -> newValue));
  }

  @Override
  public ProductEditValidationDTO performProductDetailEdit(boolean isOnlyExternal,
    boolean combineContentAndLogisticsPcbUpdate, boolean combinePreOrderUpdate,
    EditProductResponse editProductResponse, ProductEditValidationDTO productEditValidationDTO,
    ProfileResponse profileResponse) throws Exception {
    ProductLevel3 productLevel3 = productEditValidationDTO.getProductLevel3();
    // l3 product details with logistics, attributes for newly added variants
    ProductCollection productCollection =
      this.productCollectionRepository.findByStoreIdAndProductCode(DEFAULT_STORE_ID,
        productLevel3.getProductCode());
    ProductL3Response savedProductDataFromXProduct =
      productEditValidationDTO.getProductL3Response();
    // productL3Response -> savedProductDataFromXProduct with generated item name for newly added variants
//    String gdnSku = addDeleteVariantSwitch ?
//      productLevel3.getItems().stream().filter(Predicate.not(ProductItemLevel3::isMarkForDelete))
//        .map(ProductItemLevel3::getItemSku).findFirst()
//        .orElse(savedProductDataFromXProduct.getDefaultItemSku()) :
//      savedProductDataFromXProduct.getDefaultItemSku();
    ProductAndItemsResponse productAndItemsResponse =
      this.productLevel3Repository.findDetailByGdnSku(savedProductDataFromXProduct.getDefaultItemSku());
    com.gdn.x.businesspartner.dto.PickupPointResponse pickupPointResponse =
      new PickupPointResponse();
    productAndItemsResponse.getItems().stream().findFirst()
      .ifPresent(item -> pickupPointResponse.setCode(item.getPickupPointCode()));
    ProductLevel3 productLevel3WithPPData =
      productLevel3ServiceBean.generateProductLevel3(productAndItemsResponse, null,
        pickupPointResponse, null, null, null, new HashMap<>(), new ArrayList<>(), false);
    ProductLevel3DetailResponse savedDataForHistory =
      productLevel3ServiceBean.generateProductLevel3Detail(savedProductDataFromXProduct,
        new ArrayList<>(), new ArrayList<>(), null);
    return null;
  }


  private GdnRestSingleResponse<EditProductResponse> validateL5RequestAndReturn(String requestId,
      ProductL3UpdateRequest request, EditProductResponse editResponse) throws Exception {
    if (combineL3AndL5Validation) {
      ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse =
          validateL5UpdateRequest(toProductVariantUpdateRequest(request, editResponse),
            editResponse.getProfileResponse());
      if (Objects.nonNull(itemsPriceStockImagesUpdateResponse)) {
        if (Objects.nonNull(itemsPriceStockImagesUpdateResponse.getApiErrorCode())) {
          editResponse.setApiErrorCode(itemsPriceStockImagesUpdateResponse.getApiErrorCode());
        }
        editResponse.setVariantsErrorList(itemsPriceStockImagesUpdateResponse.getVariantsErrorList());
        return new GdnRestSingleResponse<>(null, null, true, editResponse, requestId);
      }
    }
    return null;
  }

  @Override
  public void updateBrandDataOfProduct(String productCode, BrandUpdateRequest brandUpdateRequest) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCode), ErrorMessages.PRODUCT_CODE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(brandUpdateRequest.getOldBrandCode()),
        ErrorMessages.OLD_BRAND_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(brandUpdateRequest.getNewBrandCode()),
        ErrorMessages.NEW_BRAND_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(
        !StringUtils.equals(brandUpdateRequest.getNewBrandCode(), brandUpdateRequest.getOldBrandCode()),
        ErrorMessages.OLD_BRAND_CODE_AND_NEW_BRAND_CODE_SAME);
    ProductCollection productCollection =
        this.productCollectionRepository.findByStoreIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            productCode);
    if (Objects.nonNull(productCollection)) {
      String oldBrand = productCollection.getBrand();
      ProductBusinessPartner productBusinessPartner =
          productBusinessPartnerRepository.findFirstByStoreIdAndProductId(GdnMandatoryRequestParameterUtil.getStoreId(),
              productCollection.getProductId());
      ProductBrandUpdateRequest pcbBrandUpdateRequest = new ProductBrandUpdateRequest();
      pcbBrandUpdateRequest.setProductCode(productCode);
      pcbBrandUpdateRequest.setNewBrandCode(brandUpdateRequest.getNewBrandCode());
      pcbBrandUpdateRequest.setOldBrandCode(brandUpdateRequest.getOldBrandCode());
      ProductBrandUpdateResponse brandUpdateResponseFromPcb =
          productRepository.updateProductBrandData(pcbBrandUpdateRequest);
      log.info("Brand successfully updated in PCB for productCode = {} ", brandUpdateResponseFromPcb.getProductCode());
      xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, productCode, false);
      log.info("Brand successfully updated in x-product for productCode = {} ", productCode);
      if (productCollection.isReviewPending()) {
        productDistributionTaskRepository.updateProductBrand(
            ChangeBrandRequest.builder().productCode(productCode).brandCode(brandUpdateResponseFromPcb.getBrandCode())
                .brandName(brandUpdateResponseFromPcb.getBrandName()).build());
        log.info("Brand successfully updated in PDT for productCode = {} ", productCode);
      }
      updateBrandDataInPbpAndSaveHistory(productCollection, oldBrand, productBusinessPartner, brandUpdateResponseFromPcb);
    } else {
      throw new ApiIncorrectInputDataException(ApiErrorCode.PRODUCT_NOT_PRESENT.getDesc(),
          ApiErrorCode.PRODUCT_NOT_PRESENT);
    }
  }

  @Override
  public void publishProductLevelHistoryToPcbForDistributionUpdate(String productCode, String businessPartnerCode,
      List<AuditTrailDto> auditTrailDtoList) {
    distributionInfoService.publishProductLevelHistoryToPcbForDistributionUpdate(productCode, businessPartnerCode,
        auditTrailDtoList, GdnMandatoryRequestParameterUtil.getUsername());
  }

  @Override
  public Pair<CombinedEditItemResponse, EditProductItemAndImageResponse> updateAllCollectionsDownstreamAndProcessHistoryForPDPEdit(
    ProductDetailEditDTO productDetailEditDTO, EditProductResponse editProductResponse) throws Exception {
    ProductCollection productCollection = ConverterUtil.convertDtoToProductCollection(productDetailEditDTO.getProductCollectionDTO());
    boolean categoryUpdated = Objects.nonNull(productDetailEditDTO.getCategoryResponse());
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCollection.getProductCode()),
      ErrorMessages.PRODUCT_CODE_BLANK);
    CombinedEditItemResponse combinedEditItemResponse = null;
    EditProductItemAndImageResponse editProductItemAndImageResponse = null;
    if (Boolean.TRUE.equals(productDetailEditDTO.getProductEditable())) {
      combinedEditItemResponse =
        updateXProductForPdpEdit(productDetailEditDTO, categoryUpdated);
      log.info(
        "x-product save was success, proceed with product collection update for product : {} ",
        productCollection.getProductCode());
      if (productDetailEditDTO.isTakeDownProduct()) {
        log.info("Updating product business partner for taken down product : {} ",
          productCollection.getProductCode());
        if(Objects.nonNull(productDetailEditDTO.getProductBusinessPartner())) {
          productBusinessPartnerRepository.save(productDetailEditDTO.getProductBusinessPartner());
        }
      }
      handleProductTypeAndDimensionUpdates(productDetailEditDTO, productCollection.getProductCode());
      log.info("publishing vendor Combined Event for Add Edit Request for Product : {} ",
        productCollection.getProductCode());
      handleAutoApprovalType(productDetailEditDTO, productCollection.getProductCode());
      handleVendorPublishEvent(productDetailEditDTO, editProductResponse, productCollection);
      productLevel3ServiceBean.updateProductCollectionAndSolrAndPublishHistoryEvent(
        Optional.ofNullable(productDetailEditDTO.getProductLevel3()).map(ProductLevel3::getProductName)
          .orElse(StringUtils.EMPTY), productCollection, productDetailEditDTO.getReviewTypeList(),
        productDetailEditDTO, StringUtils.EMPTY);

    }
    updateProductHistory(productCollection, productDetailEditDTO);
    if (productDetailEditDTO.isEligibleForAutoReject()) {
      // update PCB for content, for reject product , done after L5 update for other products
      performPCBUpdateForPDPEditRequest(productDetailEditDTO, productCollection);
      return null;
    }
    return Pair.of(combinedEditItemResponse, editProductItemAndImageResponse);
  }

  private CombinedEditItemResponse updateXProductForPdpEdit(ProductDetailEditDTO productDetailEditDTO, boolean isCategoryUpdated)
    throws Exception {
    new CombinedEditItemResponse();
    CombinedEditItemResponse combinedEditItemResponse = new CombinedEditItemResponse();
    Optional<ProductEditRequest> productEditRequest =
      Optional.ofNullable(productDetailEditDTO.getProductDetailEditRequestForXProduct())
        .map(ProductDetailPageEditRequest::getProductEditRequest);
    if (Optional.ofNullable(productDetailEditDTO.getProductDetailEditRequestForXProduct())
      .map(ProductDetailPageEditRequest::getEditChangeType).isPresent()) {
      log.info("Updating xProduct For PDP Edit request for product: {} with request: {}",
        productDetailEditDTO.getProductSku(), Optional.of(productEditRequest).orElse(null));
      combinedEditItemResponse = xProductOutbound.updateEditedProductAndItemPickupPoint(productDetailEditDTO.getProductSku(),
        isCategoryUpdated, productDetailEditDTO.getProductDetailEditRequestForXProduct());
    }
    else{
      log.info("Calling generate Product score for main Image Url update at L4 for product : {} "
        , productDetailEditDTO.getProductSku());
      L3VersionResponse l3VersionResponse =
        xProductOutbound.generateProductScoreByProductSkuOrProductCode(
          productDetailEditDTO.getProductSku(), null,
          Objects.nonNull(productDetailEditDTO.getCategoryResponse()));
      combinedEditItemResponse.setL3Version(l3VersionResponse.getL3Version());
    }
    return combinedEditItemResponse;
  }

  private void handleProductTypeAndDimensionUpdates(ProductDetailEditDTO productDetailEditDTO,
    String productCode) {
    if (productDetailEditDTO.isProductTypeChanged()
      || productDetailEditDTO.isDimensionUpdated()) {
      productService.publishDimensionRefreshEventForReviewPendingProducts(
        GdnMandatoryRequestParameterUtil.getStoreId(), productCode,
        productDetailEditDTO.getDimensionRefreshRequest());
    }
  }

  private void handleVendorPublishEvent(ProductDetailEditDTO productDetailEditDTO, EditProductResponse editProductResponse,
    ProductCollection productCollection) throws Exception {
    if (ResponseHelper.isProductEligibleForVendorPublish(editProductResponse.getAction(), false,
      Optional.ofNullable(editProductResponse.getProfileResponse())
        .map(ProfileResponse::isTrustedSeller).orElse(false)) && StringUtils.isNotEmpty(
      productDetailEditDTO.getContentType())) {
      productService.publishAddEditedProductToPDTEvent(DEFAULT_STORE_ID,
        productDetailEditDTO.getContentType(), productCollection,
        productDetailEditDTO.getAllModifiedFields());
    }
  }

  private void handleAutoApprovalType(ProductDetailEditDTO productDetailEditDTO, String productCode)
    throws Exception {
    if (Objects.nonNull(productDetailEditDTO.getAutoApprovalType())
      && productDetailEditDTO.getAutoApprovalType().equals(AutoApprovalType.CONTENT_AND_IMAGE)) {
      if (Objects.nonNull(productDetailEditDTO.getInternalProductHistoryEventModel())) {
        kafkaProducer.send(DomainEventName.PRODUCT_INTERNAL_HISTORY_SAVE,
          productCode,
          productDetailEditDTO.getInternalProductHistoryEventModel());
      } else if (Objects.nonNull(productDetailEditDTO.getProductHistory())) {
        this.productService.saveProductHistory(productCode,
          productDetailEditDTO.getProductHistory());
      }
    }
  }

  private void updateProductHistory(ProductCollection productCollection, ProductDetailEditDTO productDetailEditDTO)
    throws Exception {
    Optional.ofNullable(productDetailEditDTO.getProductLevel3DetailResponse())
      .map(ProductLevel3DetailResponse::getAttributes)
      .ifPresent(attributes -> {
        List<ProductLevel3AttributeResponse> attributeList = new ArrayList<>(attributes);

        if (attributeList.stream().anyMatch(this::isVariantCreationTrue)) {
          attributeList.removeIf(this::isVariantCreationTrue);
          productDetailEditDTO.getProductLevel3DetailResponse().setAttributes(attributeList);
        }
      });

    log.info("Updating product History for product: {}", productCollection.getProductCode());

    String accessChannel = Optional.ofNullable(productDetailEditDTO.getProductLevel3())
      .map(ProductLevel3::getAccessChannel).orElse(mandatoryParameterHelper.getChannelId());

    boolean isSizeChartChanged = Optional.ofNullable(productDetailEditDTO.getProductLevel3())
      .map(ProductLevel3::isSizeChartChanged).orElse(false);

    productLevel3ServiceBean.createAuditLogsForEdit(
      productDetailEditDTO.getProductLevel3DetailResponse() , accessChannel, isSizeChartChanged);
  }

  private boolean isVariantCreationTrue(
    ProductLevel3AttributeResponse productLevel3AttributeResponse) {
    return Constants.WARNA.equals(productLevel3AttributeResponse.getAttributeName())
      || productLevel3AttributeResponse.isVariantCreation();
  }

  @Override
  public EditProductItemAndImageResponse performPCBUpdateForPDPEditRequest(
    ProductDetailEditDTO productDetailEditDTO, ProductCollection productCollection)
    throws Exception {
     EditProductDetailRequest editProductDetailRequest =
      createEditProductDetailRequest(productDetailEditDTO, productCollection);
    if (Objects.nonNull(editProductDetailRequest.getProductRequest())) {
      try {
        log.info("Updating PCB For PDP Edit request for product: {} with request: {}",
            productCollection.getProductCode(), editProductDetailRequest);
        EditProductItemAndImageResponse editProductItemAndImageResponse =
            productRepository.updateProductMasterDataAndImagesAndUpcCode(productCollection.getProductCode(), false,
                editProductDetailRequest);
        handleProductItemUpcCodeUpdate(productDetailEditDTO, productCollection);
        return editProductItemAndImageResponse;
      } catch (Exception e) {
        log.error(
            "Error updating PCB for Description or Content Edit for product: {} with request:" + " {}, with error : ",
            productCollection.getProductCode(), editProductDetailRequest, e);
        throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
            "Failed to update PCB for Description or Content Edit");
      }
    } else {
      return new EditProductItemAndImageResponse();
    }
  }

  @Override
  @Cacheable(cacheManager = Constants.PRODUCT_LIMIT_CACHE_MANAGER, value = CacheKeys.PRODUCT_LIMITS, key = "#businessPartnerCode", unless = "#result == null")
  public Long getProductCountCacheable(String storeId, String businessPartnerCode)
      throws Exception {
    return computeCurrentProductCount(storeId, businessPartnerCode);
  }

  @Override
  public Long getProductCount(String storeId, String businessPartnerCode, boolean incrementCounter) throws Exception {
    long productCount = 0;
    ProfileResponse profileResponse = businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    if (CommonUtils.getBusinessPartnerFlagValue(profileResponse, Constants.PRODUCT_LIMIT)) {
      if (productLimitRedisEnabled) {
        if (productLimitRedisIncrementEnabled && incrementCounter) {
          String redisKey = CacheKeys.PRODUCT_LIMITS + Constants.COLON + businessPartnerCode;
          try {
            return getAndIncrementFromCache(storeId, businessPartnerCode, redisKey);
          } catch (Exception e) {
            // Fallback to compute if Redis unavailable
            log.error("Fallback when trying to getAndIncrement product counter : {} ", businessPartnerCode, e);
            return computeCurrentProductCount(storeId, businessPartnerCode);
          }
        } else {
          // Redis get without increment
          String redisKey = CacheKeys.PRODUCT_LIMITS + Constants.COLON + businessPartnerCode;
          return getProductCountCacheable(storeId, businessPartnerCode, redisKey);
        }
      } else {
        return applicationContext.getBean(ProductLevel3V2ServiceImpl.class)
            .getProductCountCacheable(storeId, businessPartnerCode);
      }
    }
    return productCount;
  }

  private long getAndIncrementFromCache(String storeId, String businessPartnerCode, String redisKey) throws Exception {
    String cachedValue = stringRedisTemplate.opsForValue().get(redisKey);
    if (StringUtils.isBlank(cachedValue)) {
      return setRedisKeyAndReturnResponse(storeId, businessPartnerCode, redisKey);
    }
    Long incremented = stringRedisTemplate.opsForValue().increment(redisKey);
    return Optional.ofNullable(incremented).orElse(Long.parseLong(cachedValue));
  }

  private long setRedisKeyAndReturnResponse(String storeId, String businessPartnerCode, String redisKey)
      throws Exception {
    long computed = computeCurrentProductCount(storeId, businessPartnerCode);
    Duration ttl = Duration.ofSeconds(redisProperties.getProductLimitsTtl());
    stringRedisTemplate.opsForValue().set(redisKey, String.valueOf(computed), ttl);
    return computed;
  }

  public Long getProductCountCacheable(String storeId, String businessPartnerCode, String redisKey) throws Exception {
    String cachedValue = stringRedisTemplate.opsForValue().get(redisKey);
    if (StringUtils.isNotBlank(cachedValue)) {
      return Long.parseLong(cachedValue);
    }
    return setRedisKeyAndReturnResponse(storeId, businessPartnerCode, redisKey);
  }

  private long computeCurrentProductCount(String storeId, String businessPartnerCode) throws Exception {
    ProductLevel3CountResponse productLevel3CountResponse =
        productLevel3WipService.countSummaryByFilterType(businessPartnerCode, storeId, Constants.PRIMARY);
    ProductCountResponse productCountResponseVo =
        xProductOutbound.getSecondaryCounts(Constants.ACTIVE, businessPartnerCode);
    long productCount;
    productCount = productLevel3CountResponse.getTotalItemsByCriterias().get(ProductLevel3SummaryCriteria.IN_PROGRESS)
        + productLevel3CountResponse.getTotalItemsByCriterias().get(ProductLevel3SummaryCriteria.NEED_CORRECTION);
    productCount += Optional.ofNullable(productCountResponseVo.getActive()).orElse(0L) + Optional.ofNullable(
        productCountResponseVo.getOutOfStock()).orElse(0L);
    return productCount;
  }


  @Override
  public List<NeedRevisionEligibilityResponse> eligibilityForNeedRevisionDeletion(String storeId,
      List<NeedRevisionEligibilityRequest> needRevisionEligibilityRequestList, boolean processBulkFlow)
      throws ApplicationRuntimeException {
    List<String> productCodeList = getProductCodes(needRevisionEligibilityRequestList);
    List<ProductSkuBusinessPartnerDTO> productSkuAndProductCodeList =
        productBusinessPartnerServiceBean.getGdnSkuListByProductCodes(productCodeList);
    Map<String, String> productSkuToCodeMap =
        productSkuToProductCodeMap(productSkuAndProductCodeList);
    Map<String, Set<String>> businessPartnerToProductSkuMap =
        BusinessPartnerToProductSkuMap(productSkuAndProductCodeList);
    Map<String, Boolean> productSkuCampaignEligibility =
        fetchCampaignEligibility(businessPartnerToProductSkuMap);
    Set<String> orderEligibilityList = productSkuToCodeMap.entrySet().stream()
        .filter(entry -> Boolean.FALSE.equals(productSkuCampaignEligibility.getOrDefault(entry.getKey(), false)))
        .map(Map.Entry::getKey).collect(Collectors.toSet());
    Map<String, Boolean> productSkuOrderEligibility = new HashMap<>();
    if (CollectionUtils.isNotEmpty(orderEligibilityList)) {
      productSkuOrderEligibility =
          agpQueryOutbound.findNumberOfOrderByProductSkuList(orderEligibilityList);
    }
    return createEligibilityResponseList(productSkuAndProductCodeList, productSkuToCodeMap,
        productSkuCampaignEligibility, productSkuOrderEligibility, processBulkFlow);
  }

  private List<String> getProductCodes(List<NeedRevisionEligibilityRequest> requestList) {
    return requestList.stream().filter(Objects::nonNull)
        .map(NeedRevisionEligibilityRequest::getProductCode).collect(Collectors.toList());
  }

  private Map<String, String> productSkuToProductCodeMap(
      List<ProductSkuBusinessPartnerDTO> productSkuList) {
    return productSkuList.stream().collect(
        Collectors.toMap(ProductSkuBusinessPartnerDTO::getGdnProductsku,
            ProductSkuBusinessPartnerDTO::getProductCode, (existing, replacement) -> existing));
  }

  private Map<String, Set<String>> BusinessPartnerToProductSkuMap(
      List<ProductSkuBusinessPartnerDTO> productSkuList) {
    return productSkuList.stream().collect(
        Collectors.groupingBy(ProductSkuBusinessPartnerDTO::getBusinessPartnerCode,
            Collectors.mapping(ProductSkuBusinessPartnerDTO::getGdnProductsku,
                Collectors.toSet())));
  }

  private Map<String, Boolean> fetchCampaignEligibility(
      Map<String, Set<String>> businessPartnerToProductSkuMap) throws ApplicationRuntimeException {
    PromoEligibilityRequest promoEligibilityRequest = new PromoEligibilityRequest();
    promoEligibilityRequest.setBusinessPartnerAndProductSkuList(businessPartnerToProductSkuMap);
    return productLevel3Repository.campaignEligibilityForProductSku(promoEligibilityRequest);
  }

  private List<NeedRevisionEligibilityResponse> createEligibilityResponseList(
      List<ProductSkuBusinessPartnerDTO> productSkuList, Map<String, String> productSkuToCodeMap,
      Map<String, Boolean> productSkuCampaignEligibility,
      Map<String, Boolean> productSkuOrderEligibility, boolean processBulkFlow) {
    List<NeedRevisionEligibilityResponse> responseList = new ArrayList<>();
    for (ProductSkuBusinessPartnerDTO productSkuDTO : productSkuList) {
      NeedRevisionEligibilityResponse response = new NeedRevisionEligibilityResponse();
      String productSku = productSkuDTO.getGdnProductsku();
      response.setProductCode(productSkuToCodeMap.get(productSku));
      response.setProductName(productSkuDTO.getProductName());
      if (productSkuCampaignEligibility.getOrDefault(productSku, false)
          || productSkuOrderEligibility.getOrDefault(productSku, false)) {
        response.setEligibleForDeletion(null);
      } else if(processBulkFlow){
        response.setEligibleForDeletion(isEligibleForDeletion(productSkuDTO.getSubmittedDate()));
      }
      else {
        //product has no order or campaign and request is from ui, no checks on submission date
        response.setEligibleForDeletion(true);
      }
      response.setActiveOrderHistory(productSkuOrderEligibility.getOrDefault(productSku, false));
      response.setPartOfCampaign(productSkuCampaignEligibility.getOrDefault(productSku, false));

      responseList.add(response);
    }
    return responseList;
  }

  private Boolean isEligibleForDeletion(Date submittedDate) {
    if (Objects.isNull(submittedDate))
      return false;
    long daysSinceSubmission =
        TimeUnit.DAYS.convert(System.currentTimeMillis() - submittedDate.getTime(),
            TimeUnit.MILLISECONDS);
    return daysSinceSubmission > needRevisionDeletionThreshold;
  }


  private EditProductDetailRequest createEditProductDetailRequest(ProductDetailEditDTO productDetailEditDTO,
    ProductCollection productCollection) throws Exception {
    EditProductDetailRequest editProductDetailRequest = new EditProductDetailRequest();
    editProductDetailRequest.setProductRequest(productDetailEditDTO.getProductRequestForPCB());
    if (RequestHelper.isEligibleForPCBUpdate(productDetailEditDTO, editProductDetailRequest)) {
      editProductDetailRequest.setProductRequest(
          productLevel3Service.copyProductLevel3ToProductAndUpdateToPCB(productCollection,
              productDetailEditDTO.getProductLevel3(), false,
              CollectionUtils.isNotEmpty(productDetailEditDTO.getRestrictedKeywordsByFieldList()),
              productDetailEditDTO.isAutoApproved(), productDetailEditDTO.getCategoryResponse(),
              new EditProductResponse(), productDetailEditDTO, new ProductL3Response(), null).getKey());
    }
    if (processVariantImageNewlyAddedItems) {
        setVariantImagesForNewlyAddedItems(productDetailEditDTO.getProductLevel3().getNewlyAddedItems(),
        editProductDetailRequest.getProductRequest(), productDetailEditDTO.getProductImageEditRequests());
    }
    editProductDetailRequest.setResetExtractedAttributeValue(true);
    editProductDetailRequest.setProductItemUpcCodeUpdateRequestList(productDetailEditDTO.getProductItemUpcCodeUpdateRequests());
    editProductDetailRequest.setProductImageEditRequests(productDetailEditDTO.getProductImageEditRequests());
    return editProductDetailRequest;
  }

  public void setVariantImagesForNewlyAddedItems(List<ProductItemLevel3> newlyAddedItems,
    ProductRequest productRequest, List<ProductImageEditRequest> productImageEditRequests) {
    Map<String, List<ProductLevel3Image>> newlyAddedItemNameImagesMap = newlyAddedItems.stream()
      .collect(Collectors.toMap(ProductItemLevel3::getItemName, ProductItemLevel3::getImages,
        (a, b) -> a));
    populateImagesFromProductImageEditRequest(productRequest, productImageEditRequests);
    List<String> commonImagesPaths =
      Optional.ofNullable(productRequest).map(ProductRequest::getImages).orElse(Collections.emptyList()).stream()
      .filter(Image::isCommonImage).filter(Predicate.not(Image::isMarkForDelete))
      .map(Image::getLocationPath)
      .collect(Collectors.toList());
    try {
      for (ProductItemRequest productItemRequest : Optional.ofNullable(productRequest)
        .map(ProductRequest::getNewlyAddedProductItems).orElse(Collections.emptyList())) {
        List<ProductLevel3Image> images =
          newlyAddedItemNameImagesMap.getOrDefault(productItemRequest.getGeneratedItemName(),
            Collections.emptyList());

        List<ProductLevel3Image> filteredImages =
          images.stream().filter(image -> !commonImagesPaths.contains(image.getLocationPath()))
            .collect(toList());

        String imagePrefix = fileStorageService.getImagePathPrefix() + Constants.DELIMITER_SLASH;
        AtomicInteger sequence = new AtomicInteger(commonImagesPaths.size());

        List<Image> newImages = filteredImages.stream().map(
          productLevel3Image -> populateNewlyAdddedVariantImages(productLevel3Image, imagePrefix,
            sequence)).collect(toList());

        Set<String> uniqueLocationPaths = new HashSet<>();
        List<Image> uniqueImages =
          newImages.stream().filter(image -> uniqueLocationPaths.add(image.getLocationPath()))
            .collect(toList());

        Image mainImageFromImageList =
          uniqueImages.stream().filter(Image::isMainImages).findFirst().orElse(null);

        List<Image> finalImages = new ArrayList<>(productItemRequest.getImages());
        finalImages.addAll(uniqueImages);

        // Override main Image flag to new variant image if added
        if (Objects.nonNull(mainImageFromImageList)) {
          finalImages.stream().filter(
              image -> !image.getLocationPath().equals(mainImageFromImageList.getLocationPath()))
            .forEach(image -> image.setMainImages(false));
        }

        removeCopyToAllVariantImagesAndSetFinalImages(productImageEditRequests, productItemRequest, finalImages);
      }
    } catch (Exception e) {
      log.error("Error while processing new images for newly added variant for product: {} for variants: {} ",
        productRequest.getProductCode(), productRequest.getNewlyAddedProductItems(), e);
    }
  }

  private static void populateImagesFromProductImageEditRequest(ProductRequest productRequest,
    List<ProductImageEditRequest> productImageEditRequests) {
    Map<String, ProductImageEditRequest> productImageEditRequestMap = Optional.ofNullable(
        productImageEditRequests)
      .orElse(Collections.emptyList()).stream()
      .filter(Objects::nonNull)
      .collect(Collectors.toMap(ProductImageEditRequest::getImagePath, Function.identity(), (a, b) -> a));

    for(Image image : Optional.ofNullable(productRequest).map(ProductRequest::getImages).orElse(Collections.emptyList())){
      if(productImageEditRequestMap.containsKey(image.getLocationPath())){
        ProductImageEditRequest productImageEditRequest =
          productImageEditRequestMap.get(image.getLocationPath());
        image.setMarkForDelete(productImageEditRequest.isMarkForDelete());
      }
    }
  }

  private static void removeCopyToAllVariantImagesAndSetFinalImages(List<ProductImageEditRequest> productImageEditRequests,
    ProductItemRequest productItemRequest, List<Image> finalImages) {
    List<String> copyToAllVariantImages = new ArrayList<>();
    for (ProductImageEditRequest request : Optional.ofNullable(productImageEditRequests)
      .orElse(Collections.emptyList())) {
      if (Objects.nonNull(request) && Objects.nonNull(request.getCopyToAllVariantImages())) {
        String imagePath = request.getImagePath();
        copyToAllVariantImages.add(imagePath);}
    }
    // Remove copyToAllVariantImages and inactive images
    finalImages.removeIf(
      image -> copyToAllVariantImages.contains(image.getLocationPath()) || !image.isActive());

    productItemRequest.setImages(finalImages);
  }

  private static Image populateNewlyAdddedVariantImages(ProductLevel3Image productLevel3Image,
    String imagePrefix,
    AtomicInteger sequence) {
    String locationPath = productLevel3Image.getLocationPath();
    if (!locationPath.contains(imagePrefix)) {
      locationPath = imagePrefix.concat(locationPath);
    }
    Image image = new Image();
    image.setLocationPath(locationPath);
    image.setMainImages(productLevel3Image.getMainImage());
    image.setActive(true);
    image.setOriginalImage(true);
    image.setEdited(true);
    image.setSequence(sequence.getAndIncrement());
    image.setHashCode(ApproveProductUtils.generateHashcodeByLocationPath(locationPath));
    return image;
  }


  private void handleProductItemUpcCodeUpdate(ProductDetailEditDTO productDetailEditDTO,
    ProductCollection productCollection) throws Exception {
    if (CollectionUtils.isNotEmpty(productDetailEditDTO.getProductItemUpcCodeUpdateRequests())) {
      log.info("UPC code was updated for product: {} with request: {}",
        productCollection.getProductCode(), productDetailEditDTO.getProductItemUpcCodeUpdateRequests());
      productService.publishAddEditedProductToPDTEvent(
        GdnMandatoryRequestParameterUtil.getStoreId(), EditedReviewTypeConstants.CONTENT_REFRESH,
        productCollection, productDetailEditDTO.getAllModifiedFields());
    }
    if (CollectionUtils.isNotEmpty(productDetailEditDTO.getHistoryForUPC())) {
      for (Map<String, String> historyMap : productDetailEditDTO.getHistoryForUPC()) {
        this.updatedProductHistoryService.createProductL3AuditLog(productCollection.getBusinessPartnerCode(),
            historyMap.get(ITEM_SKU), historyMap.get(PRODUCT_SKU), historyMap.get(ITEM_NAME),
            UpdateProductActivity.UPC_CODE.getDesc(), historyMap.get(OLD_UPC_CODE), historyMap.get(NEW_UPC_CODE), false,
            org.apache.commons.lang3.StringUtils.EMPTY);
      }
    }
  }


  private void updateBrandDataInPbpAndSaveHistory(ProductCollection productCollection, String oldBrand,
      ProductBusinessPartner productBusinessPartner, ProductBrandUpdateResponse brandUpdateResponseFromPcb) {
    productCollection.setBrand(brandUpdateResponseFromPcb.getBrandName());
    productCollection.setBrandCode(brandUpdateResponseFromPcb.getBrandCode());
    productCollectionRepository.save(productCollection);
    productBusinessPartner.setBrand(brandUpdateResponseFromPcb.getBrandName());
    productBusinessPartnerRepository.save(productBusinessPartner);
    productLevel1HistoryService.saveProductHistory(productCollection.getProductCode(),
        GdnMandatoryRequestParameterUtil.getUsername(), ProductWorkflowLookup.STATE_EDIT_DESCRIPTION, String.valueOf(
            new ProductFieldHistory(SaveHistoryConstants.BRAND_FIELD, oldBrand,
                brandUpdateResponseFromPcb.getBrandName())));
    productService.updateSolrProductCollection(productCollection);
  }

  @Override
  public ProductL3BasicResponse getProductL3BasicResponse(String storeId, String productCode) {
    List<ProductBusinessPartner> businessPartnerList = productBusinessPartnerServiceBean.findByProductCode(productCode);
    ProductL3BasicResponse productL3BasicResponse = new ProductL3BasicResponse();
    productL3BasicResponse.setProductCode(productCode);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(businessPartnerList), ErrorMessages.PRODUCT_NOT_FOUND);
    productL3BasicResponse.setProductSku(businessPartnerList.get(0).getGdnProductSku());
    if (IN_PROGRESS.equals(businessPartnerList.get(0).getState())) {
      BeanUtils.copyProperties(businessPartnerList.get(0), productL3BasicResponse);
    } else {
      BasicProductResponse basicProductInfo =
          xProductOutbound.getBasicProductInfoV2(businessPartnerList.get(0).getGdnProductSku());
      BeanUtils.copyProperties(basicProductInfo, productL3BasicResponse);
    }
    productL3BasicResponse.setPureInStoreProduct(
        productL3BasicResponse.isOff2OnChannelActive() && !productL3BasicResponse.isB2cActivated());
    return productL3BasicResponse;
  }

  @Override
  public ValidOmniChannelSkuResponse checkIfOmniChannelSkuExists(String storeId,
      OmniChannelSkuRequest omniChannelSkuRequest) {
    return new ValidOmniChannelSkuResponse(
        productOutbound.getOmniChannelSkuToItemCode(omniChannelSkuRequest.getSellerCode(),
            omniChannelSkuRequest.getOmniChannelSkus()));
  }

  @Override
  public void updateCogsValue(String productSku, CogsUpdateRequests request) throws Exception {
    CogsUpdateListRequest cogsUpdateListRequest = new CogsUpdateListRequest();
    if (Objects.nonNull(request) && CollectionUtils.isNotEmpty(request.getListRequest())) {
      validatePpCodesInRequest(request);
      List<CogsUpdateRequest> cogsUpdateRequests = request.getListRequest().stream().map(dto -> {
        CogsUpdateRequest cogsUpdateRequest = new CogsUpdateRequest();
        cogsUpdateRequest.setItemSku(dto.getItemSku());
        cogsUpdateRequest.setPickupPointCode(dto.getPickupPointCode());
        cogsUpdateRequest.setInsuredAmount(dto.getInsuredAmount());
        return cogsUpdateRequest;
      }).collect(Collectors.toList());
      cogsUpdateListRequest.setListRequest(cogsUpdateRequests);
      xProductOutbound.updateCogsValue(productSku, cogsUpdateListRequest);
    }
  }

  private void validatePpCodesInRequest(CogsUpdateRequests request) throws Exception {
    List<String> pickupPointCodes =
        request.getListRequest().stream().map(CogsUpdateDtoRequest::getPickupPointCode).distinct().toList();
    List<PickupPointResponse> pickupPointResponses =
        pickupPointOutbound.getByPickupPointCodes(Constants.DEFAULT_USERNAME, new ArrayList<>(pickupPointCodes));
    List<String> distributionTruePickupPointCodes = pickupPointResponses.stream().filter(
        pickupPointResponse -> Boolean.TRUE.equals(
            Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
                .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false))).map(PickupPointResponse::getCode).toList();
    if (distributionTruePickupPointCodes.size() != pickupPointCodes.size()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.COGS_UPDATE_FAILED);
    }
  }

  @Override
  public List<CogsDataResponse> getCogsData(String productSku, int page, int size) throws Exception {
    List<CogsResponse> cogsResponses = xProductOutbound.getCogsData(productSku, page, size);
    return cogsResponses.stream()
        .map(cogsResponse -> {
          CogsDataResponse cogsDataResponse = new CogsDataResponse();
          cogsDataResponse.setItemSku(cogsResponse.getItemSku());
          cogsDataResponse.setPickupPointCode(cogsResponse.getPickupPointCode());
          cogsDataResponse.setInsuredAmount(cogsResponse.getInsuredAmount());
          return cogsDataResponse;
        })
        .collect(Collectors.toList());
  }
}
