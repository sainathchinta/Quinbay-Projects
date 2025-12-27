package com.gdn.mta.product.controller;

import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
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
import java.util.TimeZone;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.AppealProductConfigResponse;
import com.gda.mta.product.dto.AppealProductRequest;
import com.gda.mta.product.dto.AppealProductResponse;
import com.gda.mta.product.dto.AutoApprovalTypeRequest;
import com.gda.mta.product.dto.BrandUpdateRequest;
import com.gda.mta.product.dto.BusinessPartnerCodeResponseList;
import com.gda.mta.product.dto.CategoryAndCnDetailDto;
import com.gda.mta.product.dto.EditProductV2Response;
import com.gda.mta.product.dto.GenericStringListRequest;
import com.gda.mta.product.dto.NeedRevisionProductsRequest;
import com.gda.mta.product.dto.OmniChannelExistsRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gda.mta.product.dto.ProductBrandUpdateRequest;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gda.mta.product.dto.RetryNeedRevisionRequest;
import com.gda.mta.product.dto.ValidateDuplicateProductRequest;
import com.gda.mta.product.dto.ValidateDuplicateProductResponse;
import com.gda.mta.product.dto.response.AutoApprovalTypeResponse;
import com.gda.mta.product.dto.response.CogsValueResponse;
import com.gda.mta.product.dto.response.HalalProductHistoryResponse;
import com.gda.mta.product.dto.response.InProgressProductResponse;
import com.gda.mta.product.dto.response.InProgressProductResponsePageResponse;
import com.gda.mta.product.dto.response.ItemsPriceStockImagesUpdateResponse;
import com.gda.mta.product.dto.response.OmniChannelMapAndSkuResponse;
import com.gda.mta.product.dto.response.OmniChannelSkuResponse;
import com.gda.mta.product.dto.response.ProductAndBrandResponse;
import com.gda.mta.product.dto.response.ProductCodeAndNameDetails;
import com.gda.mta.product.dto.response.ProductCreationFailureResponse;
import com.gda.mta.product.dto.response.SimpleStringResponse;
import com.gda.mta.product.dto.response.RetryAutoNeedRevisionResponse;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.service.ProductAppealService;
import com.gdn.mta.product.service.ProductLevel3V2Wrapper;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;

import com.gda.mta.product.dto.response.ProductFilterResponse;
import com.gda.mta.product.dto.response.ProductSkuResponseList;
import com.gda.mta.product.dto.response.VendorNotesResponse;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gda.mta.product.dto.AttributeReqModelListRequest;
import com.gda.mta.product.dto.BulkDeleteProductWipRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gda.mta.product.dto.CreateProductRequest;
import com.gda.mta.product.dto.DalamProductListRequest;
import com.gda.mta.product.dto.DeleteProductRequest;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.NeedRevisionSubmitRequest;
import com.gda.mta.product.dto.ProductCodesResponse;
import com.gda.mta.product.dto.ProductCollectionRequest;
import com.gda.mta.product.dto.ProductCollectionResponse;
import com.gda.mta.product.dto.ProductDetailCompleteResponse;
import com.gda.mta.product.dto.ProductCreationRequest;
import com.gda.mta.product.dto.ProductDetailMtaResponse;
import com.gda.mta.product.dto.ProductFilterRequest;
import com.gda.mta.product.dto.ProductHistoryRequest;
import com.gda.mta.product.dto.ProductHistoryResponse;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductMtaResponse;
import com.gda.mta.product.dto.ProductRevisionInfoResponse;
import com.gda.mta.product.dto.ScreeningProductBulkActionsRequest;
import com.gda.mta.product.dto.StringListRequest;
import com.gda.mta.product.dto.VendorNotesRequest;
import com.gda.mta.product.dto.generator.StuckProductResponse;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.entity.MergeStatus;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductBusinessPartnerAttribute;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductHistory;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductSystemParameter;
import com.gdn.mta.product.entity.ProductWorkflow;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.BulkActionType;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.mta.product.service.HalalHistoryUpdateService;
import com.gdn.mta.product.service.ProductBusinessPartnerService;
import com.gdn.mta.product.service.ProductMigrationWrapperService;
import com.gdn.mta.product.service.ProductService;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.mta.product.service.ProductSystemParameterService;
import com.gdn.mta.product.service.ProductWorkflowServiceWrapper;
import com.gdn.mta.product.service.TrackerService;
import com.gdn.mta.product.service.domainevent.publisher.ProductStatusPublisherService;
import com.gdn.mta.product.service.exception.ApiDataNotFoundException;
import com.gdn.mta.product.service.util.WholesaleValidationUtil;
import com.gdn.mta.product.util.BarcodeGenerator;
import com.gdn.mta.product.util.CommonUtils;
import com.gdn.mta.product.util.ControllerUtils;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.mta.product.util.MergeProductsUtility;
import com.gdn.mta.product.util.ProductCreationValidation;
import com.gdn.mta.product.util.ProductWorkflowLookup;
import com.gdn.mta.product.util.SingleValueResponse;
import com.gdn.mta.product.util.ValidateUrlUtil;
import com.gdn.mta.product.util.validator.ValidationUtil;
import com.gdn.mta.product.valueobject.SolrProductCollectionDTO;
import com.gdn.mta.product.web.model.ProductControllerErrorMessage;
import com.gdn.mta.product.web.model.ProductControllerPath;
import com.gdn.partner.pbp.annotations.AuditLog;
import com.gdn.partner.pbp.logger.standar.LoggerAspect;
import com.gdn.partner.pbp.logger.standar.LoggerAttributeModel;
import com.gdn.partner.pbp.logger.standar.LoggerParam;
import com.gdn.partner.pbp.logger.standar.LoggerStandard;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.constants.EditedReviewTypeConstants;
import com.gdn.partners.pbp.commons.constants.SystemParameterConstants;
import com.gdn.partners.pbp.dto.productcategory.CategoryHierarchyProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel1.ProductSearchRequest;
import com.gdn.partners.pbp.dto.productlevel3.PostLiveProductCountResponse;
import com.gdn.partners.pbp.dto.productlevel3.ProductCollectionCountRestResponse;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountRequest;
import com.gdn.partners.pbp.model.productlevel3.ProductCollectionCountResponse;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.service.notification.ProductNotificationService;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.gdn.partners.pbp.workflow.product.ProductWfService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.CatalogType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.OmniChannelSkuRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductL1AndL2CodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.google.api.services.youtube.YouTube;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping(value = ProductControllerPath.BASE_PATH)
@Tag(name = "ProductController", description = "Product Service API")
@Slf4j
public class ProductController {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductController.class);
  private static final String RESULT = "result";
  private static final String DEFAULT_NOTE = "permanent delete";
  private static final String QR_CODE_NOTIFICATION_ERROR = "Error while sending the QR code notification for business partner : {} and file : {}";
  private static final ArrayList<Integer> dgLevelList = new ArrayList<>(Arrays.asList(0, 1, 2));
  private static final String INVALID_DG_LEVEL_ERROR = "invalid DG level found";
  public static final String BULK_CLIENT_ID = "x-bulk";
  private static final List<String> REVIEW_TYPE_LIST = new ArrayList<>(Arrays
      .asList(EditedReviewTypeConstants.CONTENT_EDIT, EditedReviewTypeConstants.CONTENT_REFRESH,
          EditedReviewTypeConstants.IMAGE_EDIT, EditedReviewTypeConstants.IMAGE_REFRESH));

  @Lazy
  @Autowired
  private ProductService productService;

  @Autowired
  private ProductLevel3V2Wrapper productLevel3V2Wrapper;

  @Lazy
  @Autowired
  private ProductBusinessPartnerService productBusinessPartnerService;

  @Autowired
  private ProductStatusPublisherService productStatusPublisherService;

  @Autowired
  private ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  @Qualifier(value = "productWfService")
  private ProductWfService productWorkflowService;

  @Value("${cron.job.stuck.product.batch.size}")
  private int cronJobStuckProductBatchSize;

  @Value("${family.colour.validation.switch}")
  private boolean familyColourValidationSwitch;

  @Value("${validate.brand.code.switch}")
  private boolean validateBrandCode;

  @Value("${max.wholesale.price.requests}")
  private int maxWholesalePriceRequests;

  @Value("${check.only.new.images.for.extension}")
  private boolean checkOnlyNewImagesForExtension;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${full.image.source.directory}")
  private String fullImageSourceDirectory;

  @Value(value = "${youtube.data.api.key}")
  private String youTubeDataApiKey;

  @Value("${preOrder.maximum.days}")
  private int preOrderMaximumDays;

  @Value("${preOrder.working.maximum.week}")
  private int preOrderMaximumWeek;

  @Value("${system.parameter.max.stock.value}")
  private long maxStockLimit;

  @Value("${validate.create.shipping.dimensions}")
  private boolean validateCreateShippingAndDimensions;

  @Value("${validate.product.type}")
  private boolean validateProductType;

  @Value("${validate.create.product.switch.based}")
  private boolean validateCreateProductBasedOnSwitch;

  @Value("${product.limit.switch.enabled}")
  private boolean productLimitSwitchEnabled;

  @Value("${sanitise.product.name}")
  private boolean sanitiseProductName;

  @Value("${sanitise.attribute.values}")
  private boolean sanitiseAttributeValues;

  @Value("${bp.bopis.restriction.enabled}")
  private boolean bpBopisRestrictionEnabled;

  @Value("${validate.duplicate.upc.code.create}")
  private boolean validateDuplicateUpcCodeCreate;

  @Value("${max.characters.in.description}")
  private int maximumCharactersInDescription;

  @Value("${max.characters.without.formatting.description}")
  private int maximumCharactersWithoutFormattingDescription;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${size.chart.addition.for.product}")
  private boolean sizeChartAdditionForProduct;

  @Value("${validate.item.and.product.attribute.value}")
  private boolean validateItemAndProductAttributeValue;

  @Value("${variant.restricted.values}")
  private List<String> variantRestrictedValues;

  @Value("${variant.restricted.values.flag}")
  private boolean variantRestrictedValuesFlag;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionEnabled;

  @Value("${bopis.cnc.restriction.feature.switch}")
  private boolean bopisCNCRestrictionEnabled;

  @Value("${instore.new.flow.enabled}")
  private boolean instoreNewFlowEnabled;

  @Value("${set.default.b2c.activated}")
  private boolean setDefaultB2CActivated;

  @Value("${webp.conversion.enabled}")
  private boolean webpConversionEnabled;

  @Value("${youtube.regex}")
  private String youtubeRegex;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductNotificationService productNotificationService;

  @Autowired
  private ProductWorkflowServiceWrapper productWorkflowServiceWrapper;

  @Autowired
  private YouTube youTube;

  @Autowired
  private WholesaleValidationUtil wholesaleValidationUtil;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductMigrationWrapperService productMigrationWrapperService;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductCreationValidation productCreationValidation;

  @Autowired
  private HalalHistoryUpdateService halalHistoryUpdateService;

  @Autowired
  private ProductAppealService productAppealService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${populate.default.promosku.value}")
  private boolean populateDefaultPromoskuValue;

  @Value("${max.product.dimension.limit}")
  private int maxProductDimensionLimit;

  @Value("${bopis.category.validation.for.merchant.types}")
  private String bopisCategoryValidationMerchantTypes;

  @Value("${seller.penalty.enabled.phase1}")
  private boolean sellerPenaltyEnabledPhase1;

  @Value("${convert.pre.order.date.to.jkt}")
  private boolean convertPreOrderDateToJKT;

  private static final String INVALID_BULK_ACTION_TYPE = "Invalid bulk action type: {}";
  private static final String FAILED_BULK_ACTION =
      "BulkActions : failed to do bulk action for productCodes : {}";
  private static final String ERROR_WHILE_FETCHING_REVISION_HISTORY =
      "Error while fetching revision history for productCode: {}";

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_PRODUCT_IMAGE_NAME, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product image name", description = "update product image name")
  @ResponseBody
  public GdnRestSingleResponse<ActivateImageResponse> updateProductImageName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ActivateImageRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateProductImageName", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_IMAGE_UPDATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getFilenames()),
        ProductControllerErrorMessage.FILENAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getHashCode()),
        ProductControllerErrorMessage.HASH_CODE_MUST_NOT_BE_BLANK);
    ActivateImageResponse response = this.productService.updateProductImageName(request);
    return new GdnRestSingleResponse<ActivateImageResponse>(null, null, true, response, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "activate and update product image name", description = "activate and update product image name")
  @ResponseBody
  public GdnBaseRestResponse activateAndUpdateImageName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ActivateImageRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "activateAndUpdateImageName", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_IMAGE_ACTIVATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getFilenames()),
        ProductControllerErrorMessage.FILENAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getHashCode()),
        ProductControllerErrorMessage.HASH_CODE_MUST_NOT_BE_BLANK);
    this.productService.activateAndUpdateImageName(request);
    return new GdnBaseRestResponse("","",true,requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.IS_PRODUCT_IMAGES_ACTIVATED, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "activate and update product image name", description = "activate and update product image name")
  @ResponseBody
  public GdnRestSingleResponse<ActivateImageResponse> isProductImagesActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "isProductImagesActivated", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH, productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    ActivateImageResponse response = this.productService.isProductImagesActivated(productCode);
    return new GdnRestSingleResponse<ActivateImageResponse>(null, null, true, response, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.APPROVED_CONTENT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "approved content product", description = "approved content product")
  @ResponseBody
  @Deprecated
  public GdnBaseRestResponse approvedContent(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approvedContent", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_CONTENT,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || (request.getUpdatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    Product product = this.productService.findById(request.getId());
    BeanUtils.copyProperties(request, product, "id", "storeId", "createdDate", "createdBy",
        "markForDelete", "productCategories", "productAttributes", "productItems", "images");
    convertProductRequestToProduct(request, product);
    this.productService.approvedContent(storeId, product);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @Deprecated
  @RequestMapping(value = ProductControllerPath.APPROVED_IMAGE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "approved image product", description = "approved image product")
  @ResponseBody
  public GdnBaseRestResponse approvedImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approvedImage", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_IMAGE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || (request.getUpdatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    GdnPreconditions.checkArgument(!(request.getImages().isEmpty()),
        ProductControllerErrorMessage.PRODUCT_IMAGES_MUST_NOT_BE_BLANK);
    Product product = this.productService.findById(request.getId());
    BeanUtils.copyProperties(request, product, "id", "storeId", "createdDate", "createdBy",
        "markForDelete", "productCategories", "productAttributes", "productItems", "images");
    convertProductRequestToProduct(request, product);
    this.productService.approvedImage(storeId, product);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  private void convertProductRequestToProduct(ProductRequest request, Product product)
      throws Exception {
    product.setUpdatedBy(request.getUpdatedBy());
    product.setUpdatedDate(request.getUpdatedDate());
    product.getProductCategories().clear();
    product.getProductAttributes().clear();
    product.getProductItems().clear();
    product.getProductImages().clear();
    for (ProductCategoryRequest productCategoryRequest : request.getProductCategories()) {
      ProductCategory productCategory = new ProductCategory();
      Category category = new Category();
      BeanUtils.copyProperties(productCategoryRequest, productCategory);
      BeanUtils.copyProperties(productCategoryRequest.getCategory(), category);
      productCategory.setCategory(category);
      product.getProductCategories().add(productCategory);
    }
    for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
      ProductAttribute productAttribute = new ProductAttribute();
      Attribute attribute = new Attribute();
      BeanUtils.copyProperties(productAttributeRequest, productAttribute, "productAttributeValues");
      BeanUtils.copyProperties(productAttributeRequest.getAttribute(), attribute);
      if (Objects.nonNull(productAttributeRequest.getAttribute().getAttributeType())) {
        attribute
            .setAttributeType(AttributeType.valueOf(productAttributeRequest.getAttribute().getAttributeType().name()));
      }
      productAttribute.setAttribute(attribute);
      product.getProductAttributes().add(productAttribute);

      productAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
      List<ProductAttributeValueRequest> productAttributeValueRequestList = productAttributeRequest.getProductAttributeValues();
      for (ProductAttributeValueRequest req : productAttributeValueRequestList) {
        ProductAttributeValue productAttributeValue = new ProductAttributeValue();
        BeanUtils.copyProperties(req, productAttributeValue, "predefinedAllowedAttributeValue");
        String descriptiveType = req.getDescriptiveAttributeValueType().toString();
        productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.valueOf(descriptiveType));
        if (descriptiveType.equals(DescriptiveAttributeValueType.PREDEFINED.toString())) {
          PredefinedAllowedAttributeValue predefinedVal = new PredefinedAllowedAttributeValue();
          PredefinedAllowedAttributeValueRequest preReq = req.getPredefinedAllowedAttributeValue();
          BeanUtils.copyProperties(preReq, predefinedVal);
          productAttributeValue.setPredefinedAllowedAttributeValue(predefinedVal);

        }
        if (req.getAllowedAttributeValue() != null) {
          AllowedAttributeValueRequest allowedAttributeValueRequest =
              req.getAllowedAttributeValue();
          AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
          BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
          productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
        }
        productAttribute.getProductAttributeValues().add(productAttributeValue);
      }


    }
    for (Image productImageRequest : request.getImages()) {
      ProductImage productImage = new ProductImage();
      BeanUtils.copyProperties(productImageRequest, productImage);
      product.getProductImages().add(productImage);
    }
    if (!CollectionUtils.isEmpty(request.getProductItems())) {
      for (ProductItemRequest productItemRequest : request.getProductItems()) {
        ProductItem productItem = new ProductItem();
        BeanUtils.copyProperties(productItemRequest, productItem, "images");
        productItem.setVatApplicable(productItemRequest.getVatApplicable());
        if (productItem.getDangerousGoodsLevel() == null) {
          productItem.setDangerousGoodsLevel(ProductControllerErrorMessage.DEFAULT_DANGEROUS_GOOD_LEVEL);
        }
        for (Image productItemImageRequest : productItemRequest.getImages()) {
          ProductItemImage productItemImage = new ProductItemImage();
          BeanUtils.copyProperties(productItemImageRequest, productItemImage);
          productItem.getProductItemImages().add(productItemImage);
        }
        List<ProductItemAttributeValue> productItemAttributeValueList = new ArrayList<>();
        if (CollectionUtils.isNotEmpty(productItemRequest.getProductItemAttributeValues())) {
          for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemRequest
              .getProductItemAttributeValues()) {
            ProductItemAttributeValue productItemAttributeValue = ConverterUtil
                .convertProductItemAttributeValueRequestToProductItemAttributeValue(productItemAttributeValueRequest);
            productItemAttributeValueList.add(productItemAttributeValue);
          }
        }
        productItem.setProductItemAttributeValues(productItemAttributeValueList);
        product.getProductItems().add(productItem);
      }
    }
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.DELETE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete product", description = "delete product")
  @ResponseBody
  public GdnBaseRestResponse deleteProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "deleteProduct", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_DELETE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Product product = this.productService.findById(request.getId());
    this.productServiceWrapper.delete(storeId, product.getId(), product.getProductCode());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.DELETE_PRODUCT_COLLECTION, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "delete product by productCode", description = "delete product by productCode")
  @ResponseBody
  public GdnBaseRestResponse deleteProductCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam boolean needEmailNotification,
      @RequestBody DeleteProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "deleteProductCollection", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_DELETE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    try {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()), ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getNotes()), ProductControllerErrorMessage.NOTES_MUST_NOT_BE_BLANK);
      this.productWorkflowServiceWrapper
          .deleteProductCollection(storeId, request.getProductCode(), request.getNotes(), needEmailNotification,
              Boolean.FALSE);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Error while deleting product collection on double upload, productCode:{}", request.getProductCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.COLLECTION_FILTER_KEYWORD, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "get product collection summary filter by businessPartnerCode or categoryCode and keyword and activated and viewable",
      description = "get product collection summary filter by businessPartnerCode or categoryCode and keyword and activated and viewable")
  @ResponseBody
  public GdnRestListResponse<ProductCollectionResponse> filterProductCollectionSummaryByKeyword(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(required = false) String businessPartnerCode,
      @RequestParam(required = false) String categoryCode,
      @RequestParam(required = false) String keyword,
      @RequestParam(required = false) Boolean reviewPending,
      @RequestParam boolean activated, @RequestParam boolean viewable,
      @RequestParam(required = false, defaultValue = "desc") String sortBy) throws Exception {
    Pageable pageable = null;
    try {
      pageable = PageRequest.of(page, size);

      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "filterProductCollectionSummaryByKeyword", null,
              username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
              page + ":" + size + ":" + viewable + ":" + activated, null);

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

      Page<ProductCollection> productCollections;
      List<ProductCollectionResponse> productCollectionResponses = null;
      if (activated && viewable) {
        productCollections =
            getProductCollectionsForActiveProducts(storeId, categoryCode, keyword, reviewPending, sortBy, pageable);
        productCollectionResponses = this.productService
            .getProductsByProductCodesInAndActivatedTrueAndViewableTrue(storeId,
                productCollections.getContent());

      } else {
          DalamProductListRequest dalamProductListRequest =
              DalamProductListRequest.builder().storeId(storeId).businessPartnerCode(businessPartnerCode)
                  .categoryCode(categoryCode).keyword(keyword).page(pageable.getPageNumber()).size(pageable.getPageSize())
                  .activated(activated).viewable(viewable).startAge(null).endAge(null).lessThanAge(null).build();
          productCollections = this.productService.findProductsForDalamProcess(dalamProductListRequest);
        productCollectionResponses = productService.getProductCollectionsWithUpdatedFlags(storeId, productCollections);
      }
      return new GdnRestListResponse<>(null, null, true, productCollectionResponses,
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), productCollections.getTotalElements()),
          requestId);
    } catch (Exception e) {
      throw e;
    }
  }

  @RequestMapping(value = ProductControllerPath.COLLECTION_FILTER_KEYWORD_FOR_BULK_DOWNLOAD, method = RequestMethod.GET
      , produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(
          summary = "get product collection summary filter by businessPartnerCode or categoryCode and keyword and activated and viewable",
          description = "get product collection summary filter by businessPartnerCode or categoryCode and keyword and activated and viewable")
  @ResponseBody
  public GdnRestSingleResponse<ProductCodesResponse> filterProductCollectionSummaryByKeywordforBulkDownload(
          @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
          @RequestParam String requestId, @RequestParam String username,
          @RequestParam(required = false) String categoryCode,
          @RequestParam(required = false) String keyword,
          @RequestParam(required = false) Boolean reviewPending,
          @RequestParam(required = false) String sortBy,
          @RequestParam boolean activated,
          @RequestParam boolean viewable) throws Exception {
    try {
      Pageable pageable = null;
      List<String> productCodes = null;
      pageable = PageRequest.of(0, Integer.MAX_VALUE);

      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductCollectionSummaryByKeyword", null,
              username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
              0 + ":" + Integer.MAX_VALUE + ":" + viewable + ":" + activated, null);

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
      if (activated && viewable) {
        productCodes = productService
            .getActiveProductCodesFromSolr(storeId, keyword, categoryCode, reviewPending, sortBy, pageable);
      } else {
        LOGGER.info("Bulk download is supported only for active products");
      }
      ProductCodesResponse productCodesResponse = new ProductCodesResponse();
      productCodesResponse.setProductCodes(productCodes);
      return new GdnRestSingleResponse<ProductCodesResponse>(null, null, true, productCodesResponse,
              requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while retrieving product codes from Solr ,requestId:{}", requestId, e);
      throw e;
    }
  }

  private Page<ProductCollection> getProductCollectionsForActiveProducts(String storeId, String categoryCode,
      String keyword, Boolean reviewPending, String sortBy, Pageable pageable) throws Exception {
    Page<SolrProductCollectionDTO> solrProductCollectionDTOPage = null;
    List<ProductCollection> productCollectionList = null;
    try {
      solrProductCollectionDTOPage = this.productService
          .getActiveProductCollectionFromSolr(storeId, keyword, categoryCode, reviewPending, sortBy, pageable);
      productCollectionList = getProductCollectionFromSolrProductCollection(solrProductCollectionDTOPage);
    } catch (Exception e) {
      LOGGER.error("Exception caught while retrieving active products from solr", e);
    }
    return new PageImpl<>(productCollectionList, pageable, solrProductCollectionDTOPage.getTotalElements());
  }

  private List<ProductCollection> getProductCollectionFromSolrProductCollection(
      Page<SolrProductCollectionDTO> solrProductCollectionDTOPage) {
    List<ProductCollection> productCollectionList = new ArrayList<>();
    solrProductCollectionDTOPage.getContent().forEach(solrProductCollectionDTO -> {
      ProductCollection productCollection = new ProductCollection();
      BeanUtils.copyProperties(solrProductCollectionDTO, productCollection);
      productCollection.setActivated(true);
      productCollection.setViewable(true);
      productCollectionList.add(productCollection);
    });
    return productCollectionList;
  }

  @RequestMapping(value = ProductControllerPath.VALIDATE_DUPLICATE_PRODUCT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "validate duplicate product", description = "validate duplicate product")
  @ResponseBody
  public GdnRestSingleResponse<ValidateDuplicateProductResponse> validateDuplicateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("merchantCode") String merchantCode,
      @RequestBody ValidateDuplicateProductRequest validateDuplicateProductRequest) throws Exception {
    log.info("validate duplicate product. requestId : {} , merchantCode : {} , validateDuplicateProductRequest : {} ",
        requestId, merchantCode, validateDuplicateProductRequest);
    setMandatoryParameters(storeId, channelId, clientId, requestId, username);
    try {
      ValidateDuplicateProductResponse validateDuplicateProductResponse =
          productService.validateDuplicateProduct(storeId, merchantCode, validateDuplicateProductRequest);
      if (StringUtils.isEmpty(validateDuplicateProductResponse.getProductName())) {
        return new GdnRestSingleResponse<>(null, null, true, null, requestId);
      }
      return new GdnRestSingleResponse<>(
          com.gdn.x.product.constants.ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU, null, false,
          validateDuplicateProductResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Error while validating duplicate product. requestId : {} , merchantCode : {} , validateDuplicateProductRequest : {} , error - ",
          requestId, merchantCode, validateDuplicateProductRequest, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), null, false, null, requestId);
    } catch (Exception e) {
      log.error(
          "Error while validating duplicate product. requestId : {} , merchantCode : {} , validateDuplicateProductRequest : {} , error - ",
          requestId, merchantCode, validateDuplicateProductRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  private void setMandatoryParameters(String storeId, String channelId, String clientId, String requestId,
      String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
  }

  private String toJakartaTimeZone(String date) throws UnsupportedEncodingException, ParseException {
    if(StringUtils.isNotBlank(date)) {
      SimpleDateFormat DEFAULT_DATE_FORMAT = new SimpleDateFormat(
          "yyyy-MM-dd HH:mm");
      TimeZone tz = TimeZone.getTimeZone("Asia/Jakarta");
      DEFAULT_DATE_FORMAT.setTimeZone(tz);
      ZonedDateTime nowAsiaJakarta = ZonedDateTime.ofInstant(
          DEFAULT_DATE_FORMAT.parse(URLDecoder.decode(date, "UTF-8")).toInstant(),
          ZoneId.of("Asia/Jakarta"));
      return nowAsiaJakarta.format(DateTimeFormatter.ISO_INSTANT);
    }
    return null;
  }

  @RequestMapping(value = ProductControllerPath.DETAIL_FILTER_PRODUCT_CODE,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product detail filter by productCode",
      description = "get product detail filter by productCode")
  @ResponseBody
  public GdnRestSingleResponse<ProductDetailCompleteResponse> filterProductDetailByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) boolean inAllProducts, @PathVariable("productCode") String productCode,
      @RequestParam(required = false) String businessPartnerCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductDetailByProductCode", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        productCode, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    ProductDetailCompleteResponse productDetailCompleteResponse =
        this.productService.findProductDetailWithPreOrderByProductCode(storeId, productCode,
            Boolean.valueOf(inAllProducts), businessPartnerCode);
    assignAttributeValueToSKuTrueAttributes(productDetailCompleteResponse);
    try {
      productDetailCompleteResponse.setProductType(this.productBusinessPartnerService
          .getProductTypeBasedOnProductId(productDetailCompleteResponse.getId()));
    } catch (Exception e) {
      LOGGER.error("Error In Product Controller To get product Type", e);
    }
    return new GdnRestSingleResponse<>(null, null, true, productDetailCompleteResponse,
        requestId);
  }

  @RequestMapping(value = ProductControllerPath.BASIC_DETAIL_PRODUCT_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product basic detail by productCode", description = "get product basic detail by productCode")
  @ResponseBody
  public GdnRestSingleResponse<ProductResponse> getProductBasicDetailByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute =
        new LoggerAttributeModel(this, "get product basic details", null, username, requestId, storeId,
            channelId, clientId, LoggerAspect.PRODUCT_FETCH, productCode, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    ProductResponse productResponse = this.productService.findProductBasicDetailByProductCode(productCode);

    return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, Boolean.TRUE, productResponse, requestId);
  }


  protected void assignAttributeValueToSKuTrueAttributes(ProductDetailCompleteResponse productDetailResponse)
      throws Exception {
    if (!productDetailResponse.isViewable() && !productDetailResponse.isActivated()) {
      final List<String> attributeIdList = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
        for (ProductAttributeResponse productAttributeResponse : productDetailResponse
            .getProductAttributeResponses()) {
          if (productAttributeResponse.getAttribute() != null) {
            if (productAttributeResponse.getAttribute().isSkuValue()) {
              attributeIdList.add(productAttributeResponse.getAttribute().getId());
            }
          }
        }
      }
      if (CollectionUtils.isNotEmpty(attributeIdList)) {
        final List<ProductBusinessPartnerAttribute> productBusinessPartnerAttributeList =
            getSkuValueTrueAttributeList(attributeIdList, productDetailResponse.getId());
        final Map<String, ProductBusinessPartnerAttribute> attributeMap = new HashMap();
        for (ProductBusinessPartnerAttribute productBusinessPartnerAttribute : productBusinessPartnerAttributeList) {
          attributeMap.put(productBusinessPartnerAttribute.getAttributeId(),
              productBusinessPartnerAttribute);
        }
        if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
          for (ProductAttributeResponse productAttributeResponse : productDetailResponse
              .getProductAttributeResponses()) {
            if (productAttributeResponse.getAttribute() != null) {
              if (productAttributeResponse.getAttribute().isSkuValue()) {
                ProductBusinessPartnerAttribute productBusinessPartnerAttribute =
                    attributeMap.get(productAttributeResponse.getAttribute().getId());
                if (productBusinessPartnerAttribute != null) {
                  for (ProductAttributeValueResponse valueResponse : productAttributeResponse
                      .getProductAttributeValues()) {
                    if (com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType.SINGLE
                        .equals(valueResponse.getDescriptiveAttributeValueType())) {
                      valueResponse
                          .setDescriptiveAttributeValue(productBusinessPartnerAttribute.getValue());
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  private List<ProductBusinessPartnerAttribute> getSkuValueTrueAttributeList(
      List<String> attributeIdList, String productId) throws Exception {
    return this.productBusinessPartnerService
        .getSkuValueTrueAttributeList(attributeIdList, productId);
  }

  @RequestMapping(value = ProductControllerPath.ITEM_FILTER_KEYWORD_VIEWABLE,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product item summary by keyword and viewable",
      description = "get product item summary by storeId and keyword and viewable")
  @ResponseBody
  public GdnRestListResponse<ProductItemResponse> filterProductItemSummaryByKeywordAndViewable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String keyword,
      @RequestParam boolean viewable,
      @RequestParam (defaultValue = "true") boolean isOnlyExternal) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductItemSummaryByKeywordAndViewable", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItems = this.productService
        .findByKeywordAndViewable(storeId, keyword, viewable, isOnlyExternal, pageable);
    List<ProductItemResponse> wrapper = new ArrayList<ProductItemResponse>();
    for (ProductItem productItem : productItems) {
      ProductItemResponse wrapperElement = new ProductItemResponse();
      wrapperElement.setImages(new ArrayList<Image>());
      BeanUtils.copyProperties(productItem, wrapperElement);
      if (!productItem.getProductItemImages().isEmpty()) {
        Image productItemImage = new Image();
        BeanUtils.copyProperties(productItem.getProductItemImages().get(0), productItemImage);
        wrapperElement.getImages().add(productItemImage);
      }
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, wrapper,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItems.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductControllerPath.ITEM_FILTER_NAME_UPC_CODE,
      method = RequestMethod.POST, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product item summary by upcCode",
      description = "get product item summary by storeId and upcCode")
  @ResponseBody
  public GdnRestListResponse<ProductCodeResponse> filterProductItemSummaryByNameAndUpcCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productName,
      @RequestParam(required = false) String upcCode, @RequestParam String finalCategoryId,
      @RequestBody AttributeReqModelListRequest modelList) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductItemSummaryByNameAndUpcCode", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    final Page<ProductCodeResponse> productItems = this.productService
        .findByNameOrUpcCode(storeId, productName, upcCode, finalCategoryId, modelList, pageable);
    return new GdnRestListResponse<ProductCodeResponse>(null, null, true, productItems.getContent(),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItems.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductControllerPath.ITEM_FILTER_UPC_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product item summary by upcCode",
      description = "get product item summary by storeId and upcCode")
  @ResponseBody
  public GdnRestListResponse<ProductItemResponse> filterProductItemSummaryByUpcCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("upcCode") String upcCode)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductItemSummaryByUpcCode", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + upcCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItems =
        this.productService.findByUpcCode(storeId, upcCode, pageable);
    List<ProductItemResponse> wrapper = new ArrayList<ProductItemResponse>();
    for (ProductItem productItem : productItems) {
      ProductItemResponse wrapperElement = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), productItems.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_NAME, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary by name",
      description = "get product summary by storeId and name")
  @ResponseBody
  public GdnRestListResponse<ProductResponse> filterProductSummaryByName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("name") String name)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryByName", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + name, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products = this.productService.findByName(storeId, name, pageable);
    List<ProductResponse> wrapper = new ArrayList<ProductResponse>();
    for (Product product : products.getContent()) {
      ProductResponse wrapperElement = new ProductResponse();
      BeanUtils.copyProperties(product, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_NAME_VIEWABLE_ACTIVATED,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary by name and viewable and activated",
  description = "get product summary by storeId and name and viewable and activated")
  @ResponseBody
  public GdnRestListResponse<ProductResponse> filterProductSummaryByNameAndViewableAndActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("name") String name,
      @PathVariable("viewable") boolean viewable, @PathVariable("activated") boolean activated)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryByNameAndViewableAndActivated", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + name + ":" + viewable + ":" + activated, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products = this.productService.findByNameAndViewableAndActivated(storeId,
        name, viewable, activated, pageable);
    List<ProductResponse> wrapper = new ArrayList<ProductResponse>();
    for (Product product : products.getContent()) {
      ProductResponse wrapperElement = new ProductResponse();
      BeanUtils.copyProperties(product, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_PRODUCT_CODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary by productCode",
      description = "get product summary by storeId and productCode")
  @ResponseBody
  @Deprecated
  public GdnRestListResponse<ProductResponse> filterProductSummaryByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("productCode") String productCode)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryByProductCode", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products =
        this.productService.findByProductCode(storeId, productCode, pageable);
    return new GdnRestListResponse<>(null, null, true,
        getWrapperForProductResponse(products), new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_VIEWABLE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary by viewable",
      description = "get product summary by storeId and viewable")
  @ResponseBody
  @Deprecated
  public GdnRestListResponse<ProductResponse> filterProductSummaryByViewable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("viewable") boolean viewable)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryByViewable", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + viewable, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products = this.productService.findByViewable(storeId, viewable, pageable);
    List<ProductResponse> wrapper = new ArrayList<ProductResponse>();
    for (Product product : products) {
      ProductResponse wrapperElement = new ProductResponse();
      BeanUtils.copyProperties(product, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_VIEWABLE_ACTIVATED,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary by viewable and activated",
      description = "get product summary by storeId and viewable and activated")
  @ResponseBody
  public GdnRestListResponse<ProductResponse> filterProductSummaryByViewableAndActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("viewable") boolean viewable,
      @PathVariable("activated") boolean activated) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryByViewableAndActivated", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + viewable, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products =
        this.productService.findByViewableAndActivated(storeId, viewable, activated, pageable);
    List<ProductResponse> wrapper = new ArrayList<ProductResponse>();
    for (Product product : products) {
      ProductResponse wrapperElement = new ProductResponse();
      BeanUtils.copyProperties(product, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_NAME_WITH_REVIEW_STATUS, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary with review status by name",
      description = "get product summary with review status by storeId and name")
  @ResponseBody
  public GdnRestListResponse<ProductMtaResponse> filterProductSummaryWithReviewStatusByName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("name") String name)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryWithReviewStatusByName", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + name, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products = this.productService.findByName(storeId, name, pageable);
    List<ProductMtaResponse> wrapper = new ArrayList<ProductMtaResponse>();
    for (Product product : products.getContent()) {
      ProductMtaResponse wrapperElement = generateProductMtaResponse(product);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductMtaResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_NAME_VIEWABLE_ACTIVATED_WITH_REVIEW_STATUS,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(
      summary = "get product summary with review status by name and viewable and activated",
      description = "get product summary with review status by storeId and name and viewable and activated")
  @ResponseBody
  public GdnRestListResponse<ProductMtaResponse> filterProductSummaryWithReviewStatusByNameAndViewableAndActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("name") String name,
      @PathVariable("viewable") boolean viewable, @PathVariable("activated") boolean activated)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryWithReviewStatusByNameAndViewableAndActivated", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + viewable + ":" + activated + ":" + name, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products = this.productService.findByNameAndViewableAndActivated(storeId,
        name, viewable, activated, pageable);
    List<ProductMtaResponse> wrapper = new ArrayList<ProductMtaResponse>();
    for (Product product : products.getContent()) {
      ProductMtaResponse wrapperElement = generateProductMtaResponse(product);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductMtaResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_VIEWABLE_ACTIVATED_WITH_REVIEW_STATUS,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary with review status by viewable and activated",
  description = "get product summary with review status by storeId and viewable and activated")
  @ResponseBody
  public GdnRestListResponse<ProductMtaResponse> filterProductSummaryWithReviewStatusByViewableAndActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("viewable") boolean viewable,
      @PathVariable("activated") boolean activated) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryWithReviewStatusByViewableAndActivated", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + viewable + ":" + activated, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products =
        this.productService.findByViewableAndActivated(storeId, viewable, activated, pageable);
    List<ProductMtaResponse> wrapper = new ArrayList<ProductMtaResponse>();
    for (Product product : products) {
      ProductMtaResponse wrapperElement = generateProductMtaResponse(product);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductMtaResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.GENERATE_BARCODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "generate barcode", description = "generate barcode")
  @ResponseBody
  public GdnRestSimpleResponse<String> generateBarcode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "generateBarcode", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_GENERATE_BARCODE,
        null, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    String barcode = BarcodeGenerator.generateUPCCode();
    return new GdnRestSimpleResponse<String>(null, null, true, requestId, barcode);
  }

  ProductMtaResponse generateProductMtaResponse(Product product) throws Exception {
    ProductMtaResponse response = new ProductMtaResponse();
    BeanUtils.copyProperties(product, response);
    List<ProductWorkflow> productWorkflows =
        this.productService.findProductWorkflows(product.getStoreId(), product.getId());
    if ((productWorkflows != null) && !productWorkflows.isEmpty()) {
      boolean isReviewedContent = true;
      boolean isReviewedImage = true;
      for (ProductWorkflow productWorkflow : productWorkflows) {
        if (productWorkflow.getState() == ProductWorkflowLookup.STATE_REVIEW_CONTENT) {
          isReviewedContent = false;
        }
        if (productWorkflow.getState() == ProductWorkflowLookup.STATE_REVIEW_IMAGE) {
          isReviewedImage = false;
      }
      }
      response.setIsReviewedContent(isReviewedContent);
      response.setIsReviewedImage(isReviewedImage);
    }
    return response;
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.GENERATE_SHIPPING_WEIGHT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "generate shipping weight", description = "generate shipping weight")
  @ResponseBody
  @Deprecated
  public GdnRestSimpleResponse<Double> generateShippingWeight(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "generateShippingWeight", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_GENERATE_SHIPPING_WEIGHT,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(Objects.nonNull(request.getLength()),
        ProductControllerErrorMessage.LENGTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(request.getWidth()),
        ProductControllerErrorMessage.WIDTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(request.getHeight()),
        ProductControllerErrorMessage.HEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(request.getWeight()),
        ProductControllerErrorMessage.WEIGHT_MUST_NOT_BE_BLANK);
    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productCategories");
    for (ProductCategoryRequest productCategoryRequest : request.getProductCategories()) {
      ProductCategory productCategory = new ProductCategory();
      Category category = new Category();
      Catalog catalog = new Catalog();
      BeanUtils.copyProperties(productCategoryRequest, productCategory);
      BeanUtils.copyProperties(productCategoryRequest.getCategory(), category);
      BeanUtils.copyProperties(productCategoryRequest.getCategory().getCatalog(), catalog);
      catalog.setCatalogType(
          CatalogType.valueOf(productCategoryRequest.getCategory().getCatalog().getCatalogType()));
      category.setCatalog(catalog);
      productCategory.setCategory(category);
      product.getProductCategories().add(productCategory);
    }
    Double shippingWeight = this.productService.generateShippingWeight(product);
    BigDecimal bd = new BigDecimal(shippingWeight);
    shippingWeight = bd.setScale(2, BigDecimal.ROUND_CEILING).doubleValue();

    return new GdnRestSimpleResponse<Double>(null, null, true, requestId, shippingWeight);
  }

  @RequestMapping(value = ProductControllerPath.HISTORY, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product history summary",
      description = "get product history summary by product id")
  @ResponseBody
  public GdnRestListResponse<ProductHistoryResponse> getProductHistorySummary(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @PathVariable("id") String id)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductHistorySummary", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        id, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<ProductHistory> productHistoryPage =
        this.productService.findProductHistoryByStoreIdAndProductId(storeId, id, pageable);
    List<ProductHistoryResponse> wrapper = new ArrayList<ProductHistoryResponse>();


    for (ProductHistory productHistory : productHistoryPage.getContent()) {
      ProductHistoryResponse wrapperElement = new ProductHistoryResponse();
      BeanUtils.copyProperties(productHistory, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductHistoryResponse>(null, null, true, wrapper,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productHistoryPage.getTotalElements()),
        requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.SUBMIT_HISTORY, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "submit product history", description = "submit product history")
  @ResponseBody
  public GdnBaseRestResponse submitHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductHistoryRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "submitHistory", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_SUBMIT_HISTORY,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        StringUtils.isNotEmpty(request.getProductCode())
            || StringUtils.isNotEmpty(request.getUpdatedBy()) || (request.getUpdatedDate() != null),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);

    ProductHistory productHistory = new ProductHistory();
    BeanUtils.copyProperties(request, productHistory);
    this.productService.saveProductHistory(request.getProductCode(), productHistory);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductControllerPath.ROOT, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product summary",
      description = "get product summary by storeId and pageable")
  @ResponseBody
  public GdnRestListResponse<ProductResponse> getProductSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductSummary", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products = this.productService.findByStoreId(storeId, pageable);
    List<ProductResponse> wrapper = new ArrayList<ProductResponse>();
    for (Product product : products.getContent()) {
      ProductResponse wrapperElement = new ProductResponse();
      BeanUtils.copyProperties(product, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductResponse>(null, null, true, wrapper, new PageMetaData(
        pageable.getPageSize(), pageable.getPageNumber(), products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.DETAIL_WITH_REVIEW_STATUS,
      method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product by id with review status",
      description = "get product by id with review status")
  @ResponseBody
  public GdnRestSingleResponse<ProductDetailMtaResponse> getProductWithReviewStatus(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @PathVariable("id") String id)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductWithReviewStatus", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        id, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    ProductDetailMtaResponse response = null;
    Product product = this.productService.findById(id);
    if (product != null) {
      response = new ProductDetailMtaResponse();
      response.setProductItemResponses(new HashSet<ProductItemResponse>());
      response.setProductAttributeResponses(new ArrayList<ProductAttributeResponse>());
      response.setProductCategoryResponses(new ArrayList<ProductCategoryResponse>());
      BeanUtils.copyProperties(product, response);
      for (ProductItem productItem : product.getProductItems()) {
        ProductItemResponse productItemResponse = new ProductItemResponse();
        BeanUtils.copyProperties(productItem, productItemResponse);
        for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
          Image productItemImageResponse = new Image();
          BeanUtils.copyProperties(productItemImage, productItemImageResponse);
          productItemResponse.getImages().add(productItemImageResponse);
        }
        response.getProductItemResponses().add(productItemResponse);
      }
      for (ProductAttribute productAttribute : product.getProductAttributes()) {
        ProductAttributeResponse productAttributeResponse = new ProductAttributeResponse();
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(productAttribute, productAttributeResponse);
        BeanUtils.copyProperties(productAttribute.getAttribute(), attributeResponse);
        attributeResponse
            .setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
        productAttributeResponse.setAttribute(attributeResponse);
        response.getProductAttributeResponses().add(productAttributeResponse);
      }
      for (ProductCategory productCategory : product.getProductCategories()) {
        ProductCategoryResponse productCategoryResponse = new ProductCategoryResponse();
        CategoryResponse categoryResponse = new CategoryResponse();
        BeanUtils.copyProperties(productCategory, productCategoryResponse);
        BeanUtils.copyProperties(productCategory.getCategory(), categoryResponse);
        productCategoryResponse.setCategory(categoryResponse);
        response.getProductCategoryResponses().add(productCategoryResponse);
      }
      for (ProductImage productImage : product.getProductImages()) {
        Image productImageResponse = new Image();
        BeanUtils.copyProperties(productImage, productImageResponse);
        response.getImages().add(productImageResponse);
      }
      List<ProductWorkflow> productWorkflows =
          this.productService.checkProductWorkflowsAlreadyExist(product);
      if ((productWorkflows != null) && !productWorkflows.isEmpty()) {
        boolean isReviewedContent = true;
        boolean isReviewedImage = true;
        isReviewedContent = true;
        isReviewedImage = true;
        for (ProductWorkflow productWorkflow : productWorkflows) {
          if (productWorkflow.getState() == ProductWorkflowLookup.STATE_REVIEW_CONTENT) {
            isReviewedContent = false;
          }
          if (productWorkflow.getState() == ProductWorkflowLookup.STATE_REVIEW_IMAGE) {
            isReviewedImage = false;
        }
        }
        response.setIsReviewedContent(isReviewedContent);
        response.setIsReviewedImage(isReviewedImage);
      }
    }
    return new GdnRestSingleResponse<ProductDetailMtaResponse>(null, null, true, response,
        requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.CREATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save product", description = "save product")
  @ResponseBody
  @Deprecated
  public GdnRestSimpleResponse<String> saveProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveProduct", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || (request.getCreatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE);
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productCategories", "productAttributes",
        "productItems", "images");
    convertProductRequestToProduct(request, product);
    product.setActivated(true);
    product.setViewable(false);
    this.productService.save(product);
    return new GdnRestSimpleResponse<String>(null, null, true, requestId, product.getProductCode());
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.CREATE_PRODUCT_COLLECTION, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save product collection", description = "save product collection")
  @ResponseBody
  @Deprecated
  public GdnRestSimpleResponse<String> saveProductCollection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductCollectionRequest request)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveProductCollection", request.getBusinessPartnerCode(),
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || (request.getCreatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE);
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productCategories", "productAttributes",
        "productItems", "images");
    convertProductRequestToProduct(request, product);
    String productCode = this.productService.saveProductCollection(
        request.getBusinessPartnerCode(), request.getBusinessPartnerName(), product);
    return new GdnRestSimpleResponse<String>(null, null, true, requestId, productCode);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.CREATE_ACTIVATED_FALSE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "save product with activated false",
      description = "save product with activated false")
  @ResponseBody
  @Deprecated
  public GdnRestSimpleResponse<String> saveProductWithActivatedFalse(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "saveProductWithActivatedFalse", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || (request.getCreatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE);
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productCategories", "productAttributes",
        "productItems", "images");
    convertProductRequestToProduct(request, product);
    product.setActivated(false);
    product.setViewable(false);
    this.productService.save(product);
    return new GdnRestSimpleResponse<String>(null, null, true, requestId, product.getProductCode());
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.SUBMIT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "submit product", description = "submit product")
  @ResponseBody
  public GdnBaseRestResponse submitProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "submitProduct", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || (request.getUpdatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    Product product = this.productService.findById(request.getId());
    BeanUtils.copyProperties(request, product, "id", "storeId", "createdDate", "createdBy",
        "markForDelete", "productCategories", "productAttributes", "productItems");
    this.productService.submit(storeId, product);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product", description = "update product")
  @ResponseBody
  public GdnBaseRestResponse updateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateProduct", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_UPDATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    Product product = new Product();
    try {
      GdnPreconditions.checkArgument(
          !(StringUtils.isEmpty(request.getUpdatedBy()) || (Objects.isNull(request.getUpdatedDate()))),
          ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
      validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), false,
          checkOnlyNewImagesForExtension);
      product = this.productService.findById(request.getId());
      int dgLevel = 0;
      boolean internalUpdate = false;
      if (CollectionUtils.isNotEmpty(product.getProductItems()) && Objects.nonNull(product.getProductItems().get(0))) {
        dgLevel = Optional.ofNullable(product.getProductItems().get(0).getDangerousGoodsLevel()).orElse(0);
        internalUpdate = product.getProductItems().get(0).isInternalUpdate();
      }
      BeanUtils.copyProperties(request, product, "id", "storeId", "createdDate", "createdBy",
          "markForDelete", "productCategories", "productAttributes", "productItems", "images");
      convertProductRequestToProduct(request, product);
      updateDGLevelAndInternalUpdateFlag(request, product, dgLevel, internalUpdate);
      this.productServiceWrapper.update(storeId, product, request.getNotes(), request.isMarginExceed(),
        request.getBrandCode(), request.isPostLive(), request.getForceReviewNotes(), request.getBrandApprovalStatus(),
        request.getProductItems().stream().map(ProductItemRequest::isOnlyVatChanged).reduce((a, b) -> a && b).orElse(false));
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error in updating product, productCode : {} , error - ", product.getProductCode(), e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error("Error in updating product, productCode : {} , error - ", product.getProductCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage() + e.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  private void updateDGLevelAndInternalUpdateFlag(ProductRequest request, Product product, int dgLevel, boolean internalUpdate) {
    if (CollectionUtils.isNotEmpty(request.getProductItems())) {
      int dgLevelUpdated = Optional.ofNullable(request.getProductItems().get(0).getDangerousGoodsLevel()).orElse(0);
      if (!dgLevelList.contains(dgLevelUpdated)) {
        LOGGER.error("Error in updating invalid DG level: {}", dgLevelUpdated);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, INVALID_DG_LEVEL_ERROR);
      }
      if (request.isPostLive() && request.isReviewPending() && request.getProductItems().stream().allMatch(
          productItemRequest -> Objects.nonNull(productItemRequest.getDangerousGoodsLevel()) && (!Integer
              .valueOf(dgLevel).equals(productItemRequest.getDangerousGoodsLevel())))) {
        product.getProductItems().stream().map(productItem -> setDGLevel(productItem, dgLevelUpdated))
            .collect(Collectors.toList());
      } else {
        product.getProductItems().stream().map(productItem -> setExistingInternalUpdateFlag(productItem, internalUpdate))
            .collect(Collectors.toList());
      }
    }
  }

  private ProductItem setExistingInternalUpdateFlag(ProductItem productItem, boolean internalUpdate) {
    productItem.setInternalUpdate(internalUpdate);
    return productItem;
  }

  private ProductItem setDGLevel(ProductItem productItem, int dgLevel) {
    productItem.setDangerousGoodsLevel(dgLevel);
    productItem.setInternalUpdate(true);
    return productItem;
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_ACTIVATED_PRODUCTS_BULK_MASTER_DATA, method = RequestMethod.POST
      , produces = {MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update master data in bulk", description = "update master data in bulk")
  @ResponseBody
  public GdnRestSingleResponse<BulkMasterProductUpdateResponse> updateActivatedProductsBulkMasterData(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam(required = false) String username,
    @RequestBody BulkMasterProductUpdateRequest request) throws Exception{

    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getSimpleMasterProductUpdateRequests()),
        ProductControllerErrorMessage.BULK_UPDATE_LIST_MUST_NOT_BE_BLANK);
    String productCodesAsString = request
        .getSimpleMasterProductUpdateRequests().stream().map(SimpleMasterProductUpdateRequest::getProductCode)
        .collect(Collectors.joining(Constants.COMMA));
    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this,
        "updateActivatedProductsBulkMasterData", null, username, requestId, storeId,
        channelId, clientId, LoggerAspect.MASTER_DATA_BULK_UPDATE, productCodesAsString, request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    GdnPreconditions.checkArgument(
        StringUtils.isNoneBlank(request.getUpdatedBy()) && Objects.nonNull(request.getUpdatedDate()),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);

    BulkMasterProductUpdateResponse response = productServiceWrapper.bulkUpdateActivatedProducts(
        ControllerUtils.getBulkMasterProductUpdateRequestDTO(request), storeId);
    return new GdnRestSingleResponse<>(response, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_REJECTED_PRODUCT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product", description = "update product")
  @ResponseBody
  public GdnBaseRestResponse updateRejectedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateRejectedProduct", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_UPDATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || (request.getUpdatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    this.productWorkflowService.updateRejectedProduct(request);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_AND_PUBLISH_TO_PDT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update and publish product to PDT", description = "update and publish product to PDT")
  @ResponseBody
  public GdnBaseRestResponse updateAndPublishProductToPDT(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateAndPublishProductToPDT", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_UPDATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    try {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy()) || (request.getUpdatedDate() == null)),
        ProductControllerErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE);
      validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
          checkOnlyNewImagesForExtension);
    Product product = this.productService.findById(request.getId());
    BeanUtils.copyProperties(request, product, "id", "storeId", "createdDate", "createdBy",
        "markForDelete", "productCategories", "productAttributes", "productItems", "images");
    convertProductRequestToProduct(request, product);
    this.productWorkflowService
        .updateAndPublish(product, request.getNotes(), request.getBrandCode(), request.getBrandApprovalStatus());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while update and publish to PDT. request : {} , requestId : {} , error - ", request, requestId,
          e);
      return new GdnBaseRestResponse(e.getErrorMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error("Error while update and publish to PDT. request : {} , requestId : {} , error - ", request, requestId,
          e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.PUBLISH_PRODUCT_TO_PDT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "publish product to PDT", description = "publish product to PDT")
  @ResponseBody
  public GdnBaseRestResponse publishProductToPDT(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "publishProductToPDT", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_PUBLISH_PDT,
        productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    this.productService.publishToPDTByProductCode(storeId, productCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.MERGE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "merge product", description = "merge product") @ResponseBody
  public GdnBaseRestResponse mergeProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String masterProductId,
      @RequestParam String duplicateProductId,
      @RequestParam(defaultValue = "false", required = false) Boolean forceMerge) throws Exception {
    try {

      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "mergeProducts", null, username, requestId, storeId, channelId, clientId,
              LoggerAspect.PRODUCT_MERGE, masterProductId + ":" + duplicateProductId + ":" + forceMerge, null);

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

      Product masterDataProduct = this.productService.findById(masterProductId);
      Product duplicateProduct = this.productService.findById(duplicateProductId);

      if (masterDataProduct == null || duplicateProduct == null) {
        LOGGER.error(LoggerStandard.appendLogErrorTemplate(
            "Invalid MasterProductId : " + masterProductId + " or DuplicateProductId : " + duplicateProductId));
        return new GdnBaseRestResponse("Either of masterProductId or duplicateProductId is invalid", null, false,
            requestId);
      }
      MergeProductsUtility mergeUtility = new MergeProductsUtility(masterDataProduct, duplicateProduct);
      MergeStatus mergeStatus = mergeUtility.merge(forceMerge);
      if (MergeStatus.INVALID.equals(mergeStatus)) {
        LOGGER.error(LoggerStandard.appendLogErrorTemplate(
            "Merge request is invalid for duplicate product id : " + duplicateProductId
                + " and master data product id : " + masterProductId + " with error : " + mergeUtility
                .getErrorInMerge()));
        return new GdnBaseRestResponse(mergeUtility.getErrorInMerge(), null, false, requestId);
      }
      if (MergeStatus.MASTER_DATA_UPDATE.equals(mergeStatus)) {
        this.productService.update(masterDataProduct, true, null, null, null);
        LOGGER
            .debug("master data product updated with latest product infos masterDataProduct : {} ", masterDataProduct);
      }
      List<ProductItemResponse> newMasterProductItems = this.productService.findProductItemByProductId(masterProductId);
      List<ProductItemResponse> duplicateProductItems =
          this.productService.findProductItemByProductId(duplicateProductId);
      Map<String, String> oldToNewProductItemIdMap =
          mergeUtility.getOldToNewProductItemIdMap(newMasterProductItems, duplicateProductItems);
      LOGGER.debug("calculated new to old product item id mapping. oldToNewProductItemIdMap : {} ",
          oldToNewProductItemIdMap);

      if (MapUtils.isEmpty(oldToNewProductItemIdMap) && CollectionUtils.isNotEmpty(duplicateProductItems)) {
        LOGGER.error(LoggerStandard.appendLogErrorTemplate(
            "Merge request is invalid for duplicate product id : " + duplicateProductId
                + " and master data product id : " + masterProductId + " with error : Predefined "
                + "attributes are not matching with master product's attributes"));
        return new GdnBaseRestResponse(
            ErrorCategory.INVALID_FORMAT + " : Merge request is invalid. Predefined " + "attributes are not matching.",
            null, false, requestId);
      }

      if (MergeStatus.MASTER_DATA_UPDATE.equals(mergeStatus)) {
        masterDataProduct = updateProductItemImages(masterProductId, duplicateProductItems, oldToNewProductItemIdMap);
        this.productService.publishToPDTByProductCode(masterDataProduct.getStoreId(), masterDataProduct.getProductCode());
      }

      List<ProductBusinessPartner> productBusinessPartners = this.productBusinessPartnerService
          .findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId, duplicateProductId);
      LOGGER.debug("Product Business partners to be updated after merge : {} ", productBusinessPartners);
      if (CollectionUtils.isNotEmpty(productBusinessPartners)) {
        for (ProductBusinessPartner productBusinessPartner : productBusinessPartners) {
          productBusinessPartner.setProductId(masterProductId);
          productBusinessPartner.setProductName(masterDataProduct.getName());
          this.productBusinessPartnerService.update(productBusinessPartner);
          ProductBusinessPartner productBusinessPartnerWcs =
              this.productBusinessPartnerService.findById(productBusinessPartner.getId());
          for (ProductItemBusinessPartner productItemBusinessPartner : productBusinessPartnerWcs
              .getProductItemBusinessPartners()) {
            productItemBusinessPartner.setProductBusinessPartner(null);
          }
          this.productService.updateWCSAndInventoryForMergedProduct(masterDataProduct, productBusinessPartnerWcs,
              oldToNewProductItemIdMap);
          LOGGER
              .debug("Updated product business partner and WCS information for partner : {} ", productBusinessPartner);
        }
      }
      LOGGER.warn("Going to delete product after complete merge : {} ", duplicateProductId);
      this.productServiceWrapper.delete(storeId, duplicateProduct.getId(), duplicateProduct.getProductCode());

      return new GdnBaseRestResponse(mergeUtility.getErrorInMerge(), null,
          StringUtils.isEmpty(mergeUtility.getErrorInMerge()), requestId);
    } catch (Exception e) {
      LOGGER.error("Error while merging duplicate product, duplicateProductId:{}", duplicateProductId, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), null,
          false, requestId);
    }
  }

  private Product updateProductItemImages(String masterProductId,
      List<ProductItemResponse> duplicateProductItems, Map<String, String> oldToNewProductItemIdMap)
      throws Exception {
    Map<String, String> newToOldProductItemIdMap = new HashMap<>();
    for (Map.Entry<String, String> duplicateProdItem : oldToNewProductItemIdMap.entrySet()){
      newToOldProductItemIdMap.put(duplicateProdItem.getValue(), duplicateProdItem.getKey());
    }
    Product newMasterDataProduct = this.productService.findById(masterProductId);
    List<ProductItem> masterDataProductItems = newMasterDataProduct.getProductItems();
    List<ProductItemImage> defaultProductImages = new ArrayList<>();
    for(ProductImage productImage : newMasterDataProduct.getProductImages()){
      ProductItemImage productItemImage = new ProductItemImage();
      productItemImage.setStoreId(productImage.getStoreId());
      productItemImage.setLocationPath(productImage.getLocationPath());
      productItemImage.setMainImages(productImage.isMainImages());
      productItemImage.setSequence(productImage.getSequence());
      defaultProductImages.add(productItemImage);
    }

    for(ProductItem masterDataProductItem : masterDataProductItems){
      if(!masterDataProductItem.isMarkForDelete()){
        masterDataProductItem.setViewable(true);
        masterDataProductItem.setActivated(true);
      }
      if(CollectionUtils.isEmpty(masterDataProductItem.getProductItemImages())){
        String duplicateProductItemId = newToOldProductItemIdMap.get(masterDataProductItem.getId());
        for(ProductItemResponse dupProdItemResp : duplicateProductItems){
          if(dupProdItemResp.getId().equals(duplicateProductItemId)){
            masterDataProductItem.setProductItemImages(defaultProductImages);
            masterDataProductItem.setUpcCode(dupProdItemResp.getUpcCode());
            masterDataProductItem.setDangerousGoodsLevel(dupProdItemResp.getDangerousGoodsLevel());
            break;
          }
        }
      }
    }
    this.productService.updateForMerge(newMasterDataProduct);
    return newMasterDataProduct;
  }

  @RequestMapping(value = ProductControllerPath.VALIDATE_BARCODE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "validate barcode", description = "validate barcode")
  @ResponseBody
  public GdnRestSimpleResponse<Boolean> validateBarcode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("barcode") String barcode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "validateBarcode", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_VALIDATE_BARCODE,
        barcode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    return new GdnRestSimpleResponse<Boolean>(null, null, true, requestId,
        this.productService.validateBarcode(storeId, barcode));
  }

  private void setBopisDimension(ProductCreationRequest request) {
    Integer productType = request.getProductItemRequests().get(0).getProductType();
    if (Objects.nonNull(productType) && productType.equals(ProductType.BOPIS.getProductType())) {
      request.setLength((double) 0);
      request.setWidth((double) 0);
      request.setHeight((double) 0);
      request.setWeight((double) 0);
      request.setShippingWeight((double) 0);
    }
  }

  public void validateProductCreationBasedOnSwitch(ProductCreationRequest productCreationRequest) {
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(productCreationRequest.getProductCode())),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(productCreationRequest.getBrandCode())),
        ProductControllerErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(CollectionUtils.isEmpty(productCreationRequest.getProductItemRequests())),
        ProductControllerErrorMessage.PRODUCT_ITEMS_MUST_NOT_BE_BLANK);
  }

  private void validateCreateOrUpdateProduct(ProductRequest request, ProfileResponse profileResponse,
      boolean instoreNewFlowEnabled, Boolean offOn2ChannelActive, Boolean b2cActivated,
      List<ProductItemCreationRequest> productItemRequests, boolean validateEmptyDescription,
      boolean checkOnlyNewImagesForExtension) throws Exception {
    boolean performValidationOnDescriptionAndShipping =
        ValidationUtil.performValidationOnDescriptionAndShipping(profileResponse, instoreNewFlowEnabled,
            offOn2ChannelActive, b2cActivated, productItemRequests);
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getName())),
        ProductControllerErrorMessage.NAME_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(request.getName().length() <= ProductControllerErrorMessage.MAXIMUM_PRODUCT_NAME_LENGTH,
        ProductControllerErrorMessage.NAME_LENGTH_MUST_NOT_EXCEED_MAX_ALLOWED);
    ValidationUtil.updateEmptyDescriptionForPureInStoreProduct(request, performValidationOnDescriptionAndShipping);
    GdnPreconditions.checkArgument(!(request.getDescription() == null),
        ProductControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getLongDescription() == null),
        ProductControllerErrorMessage.LONG_DESCRIPTION_MUST_NOT_BE_BLANK);
    if (performValidationOnDescriptionAndShipping && validateEmptyDescription) {
      GdnPreconditions.checkArgument(!Arrays.equals(StringUtils.EMPTY.getBytes(), request.getDescription()),
          ProductControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!Arrays.equals(StringUtils.EMPTY.getBytes(), request.getLongDescription()),
          ProductControllerErrorMessage.LONG_DESCRIPTION_MUST_NOT_BE_BLANK);
    }
    String description = new String(request.getDescription(), StandardCharsets.UTF_8);
    if (BULK_CLIENT_ID.equals(GdnMandatoryRequestParameterUtil.getClientId())) {
      description = ControllerUtils.getDescriptionWithoutParagraphTags(description);
    }
    String descriptionWithOutTags = ControllerUtils.getFilteredUSPAndDescription(description);
    GdnPreconditions.checkArgument(descriptionWithOutTags.length() <= maximumCharactersInDescription, String
        .format(ProductControllerErrorMessage.DESCRIPTION_MUST_NOT_BE_MORE_THAN_MAX_CHARACTERS,
            maximumCharactersInDescription));
    GdnPreconditions.checkArgument(description.length() <= maximumCharactersWithoutFormattingDescription,
        String.format(ProductControllerErrorMessage.CHARACTER_LIMIT_REACHED_PLEASE_REDUCE_FORMATTING,
            maximumCharactersWithoutFormattingDescription));
    if (Objects.nonNull(request.getUniqueSellingPoint())) {
      String uspWithoutTags = ControllerUtils.getFilteredUSPAndDescription(request.getUniqueSellingPoint());
      GdnPreconditions
          .checkArgument(uspWithoutTags.length() <= ProductControllerErrorMessage.MAXIMUM_UNIQUE_SELLING_POINT_LENGTH,
              ProductControllerErrorMessage.UNIQUE_SELLING_POINT_MUST_NOT_BE_MORE_THAN_400_CHARACTERS);
    }
    GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getSpecificationDetail())),
        ProductControllerErrorMessage.SPECIFICATION_DETAIL_MUST_NOT_BE_BLANK);
    ValidationUtil.updateNullDimensionsForPureInStoreProduct(
        ValidationUtil.isAnyValueNull(request.getLength(), request.getWidth(), request.getHeight(),
            request.getWeight(), request.getShippingWeight()), request, performValidationOnDescriptionAndShipping);
    GdnPreconditions.checkArgument(!(request.getLength() == null),
        ProductControllerErrorMessage.LENGTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getWidth() == null),
        ProductControllerErrorMessage.WIDTH_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getHeight() == null),
        ProductControllerErrorMessage.HEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getWeight() == null),
        ProductControllerErrorMessage.WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getShippingWeight() == null),
        ProductControllerErrorMessage.SHIPPING_WEIGHT_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getProductCategories().isEmpty()),
        ProductControllerErrorMessage.PRODUCT_CATEGORIES_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(!(request.getProductAttributes().isEmpty()),
        ProductControllerErrorMessage.PRODUCT_ATTRIBUTES_MUST_NOT_BE_BLANK);
    ProductSystemParameter validImageExtension = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, SystemParameterConstants.VALID_IMAGE_EXTENSION);
    GdnPreconditions.checkArgument(ValidationUtil.validateImageExtension(request,
            Arrays.asList(validImageExtension.getValue().split(Constants.COMMA)), checkOnlyNewImagesForExtension),
        ProductControllerErrorMessage.PRODUCT_IMAGES_HAS_INVALID_EXTENSION);
    ProductSystemParameter productSystemParameter = productSystemParameterService
        .findByStoreIdAndVariable(Constants.DEFAULT_STORE_ID, Constants.YOUTUBE_URL_VALIDATION_SWITCH);
    if (Objects.isNull(request.getUniqueSellingPoint())) {
      request.setUniqueSellingPoint(StringUtils.EMPTY);
    }
    String[] apiKeys = youTubeDataApiKey.split(",");
    int apiKeyIndex = (int) (Math.random() * apiKeys.length);
    String youTubeApiKey = apiKeys[apiKeyIndex];
    LOGGER.debug("Using youtube api key index: {} for url: {}", apiKeyIndex, request.getUrl());
    boolean youTubeUrlResponse = ValidateUrlUtil.validateYouTubeUrl(request.getUrl(),
                    youTubeApiKey, youTube, Boolean.valueOf(productSystemParameter.getValue()), youtubeRegex);
    if (BULK_CLIENT_ID.equals(GdnMandatoryRequestParameterUtil.getClientId()) && !youTubeUrlResponse) {
      request.setUrl(StringUtils.EMPTY);
    } else if (!youTubeUrlResponse) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.INVALID_URL);
    }
  }

  private void validatePrice(ProductCreationRequest request, Integer minimumPrice) {
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      GdnPreconditions.checkArgument(!(productItemCreationRequest.getPrice() == null),
          ProductControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(!(productItemCreationRequest.getSalePrice() == null),
          ProductControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(productItemCreationRequest.getPrice() >= minimumPrice
              && productItemCreationRequest.getSalePrice() >= minimumPrice,
          ProductControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID + minimumPrice);
    }
  }

  private void validateItemPickupPointPrice(ProductCreationRequest request, Integer minimumPrice) {
    for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
      for (PickupPointCreateRequest pickupPointCreateRequest : productItemCreationRequest.getPickupPoints()) {
        GdnPreconditions.checkArgument(!(pickupPointCreateRequest.getPrice() == null),
            ProductControllerErrorMessage.PRICE_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(!(pickupPointCreateRequest.getSalePrice() == null),
            ProductControllerErrorMessage.SALE_PRICE_MUST_NOT_BE_BLANK);
        GdnPreconditions.checkArgument(pickupPointCreateRequest.getPrice() >= minimumPrice
                && pickupPointCreateRequest.getSalePrice() >= minimumPrice,
            ProductControllerErrorMessage.MINIMUM_PRICE_VALUE_INVALID + minimumPrice);
        GdnPreconditions.checkArgument(!(pickupPointCreateRequest.getSalePrice() > pickupPointCreateRequest.getPrice()),
            ProductControllerErrorMessage.SALE_PRICE_GREATER_THAN_PRICE_VALUE);
        if(Objects.nonNull(pickupPointCreateRequest.getB2bFields())
            && Objects.nonNull(pickupPointCreateRequest.getB2bFields().getPrice()) &&
            pickupPointCreateRequest.getB2bFields().getPrice() < minimumPrice) {
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              ProductControllerErrorMessage.B2B_MINIMUM_PRICE_VALUE_INVALID + minimumPrice);
        }
      }
    }
  }

  public void validateProductCategory(String categoryCode, List<String> salesChannel,
    String businessPartnerCode, Integer productType, boolean externalUser, String merchantType){
    CategoryAndCnDetailDto categoryAndCnDetailDto = productOutbound.validCnCategory(categoryCode);
    if(!categoryAndCnDetailDto.isCnCategory()) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductControllerErrorMessage.CN_VALIDATION_ERROR_MESSAGE);
    }
    validateCategoryBOPISEligible(categoryAndCnDetailDto, productType, externalUser, merchantType);
    validateForB2BExclusiveCategory(salesChannel, businessPartnerCode, categoryAndCnDetailDto);
  }

  private void validateCategoryBOPISEligible(CategoryAndCnDetailDto categoryAndCnDetailDto,
    Integer productType, boolean externalUser, String merchantType) {
    if (bopisCategoryRestrictionEnabled && externalUser && Set.of(
      bopisCategoryValidationMerchantTypes.split(Constants.COMMA)).contains(merchantType)) {
      if (ProductType.BOPIS.getProductType().equals(productType)
        && !categoryAndCnDetailDto.getCategoryResponse().isBopisEligible()) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ProductControllerErrorMessage.SHIPPING_UNSUPPORTED_FOR_CATEGORY_ERROR_MESSAGE);
      }
    }
  }

  private static void validateForB2BExclusiveCategory(List<String> salesChannel, String businessPartnerCode,
    CategoryAndCnDetailDto categoryAndCnDetailDto) {
    if (businessPartnerCode.equals(Constants.INTERNAL)) {
      return;
    }
    if (categoryAndCnDetailDto.getCategoryResponse().isB2bExclusive() && !salesChannel.contains(Constants.B2B_SELLER_CHANNEL)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, ProductControllerErrorMessage.B2B_VALIDATION_ERROR_MESSAGE);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.PROCESS_IMAGE, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "process image at controller", description = "process image at controller")
  @ResponseBody
  public GdnBaseRestResponse processImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode,
      @RequestParam(defaultValue = "false", required = false) Boolean retryProcessImage)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "processImage", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_PROCESS_IMAGE,
        productCode + ":" + retryProcessImage, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    this.productWorkflowService.processImage(productCode);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.REJECT_PROCESS_IMAGE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "reject process image at controller",
      description = "reject process image at controller")
  @ResponseBody
  public GdnBaseRestResponse rejectProcessImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "rejectProcessImage", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_REJECT_PROCESS_IMAGE,
        productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    LOGGER.info("Product-workflow-tracker : Reject product image for productCode : {}", productCode);
    this.productWorkflowService.rejectImage(productCode);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.APPROVE_IMAGE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "approve image at controller", description = "approve image at controller")
  @ResponseBody
  public GdnBaseRestResponse approveImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode,
      @RequestParam(defaultValue = "false", required = false) Boolean retryApproveImage)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approveImage", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_IMAGE,
        productCode + ":" + retryApproveImage, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    LOGGER.info("Product-workflow-tracker : Approve product image for productCode : {}", productCode);
    this.productWorkflowService.approveImage(productCode);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.APPROVE_CONTENT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "approve content at controller", description = "approve content at controller")
  @ResponseBody
  public GdnBaseRestResponse approveContent(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode,
      @RequestParam(defaultValue = "false", required = false) Boolean retryApproveContent)
      throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approveContent", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_CONTENT,
        productCode + ":" + retryApproveContent, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    this.productWorkflowService.approveContent(productCode);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.APPROVE_CONTENT_ACTIVE_PRODUCT, method =
      RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "approve content at controller for already active product", description =
      "approve content at controller for already active product")
  @ResponseBody
  public GdnBaseRestResponse approveContentActiveProduct(@RequestParam final String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody StringListRequest productCodeList) throws Exception {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    try {
      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approveContentActiveProduct", null,
          username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_CONTENT_ACTIVATE,
          null + ":" + productCodeList.toString(), null);

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

      GdnPreconditions.checkArgument(!CollectionUtils.isEmpty(productCodeList),
          ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
      for (String productCode : productCodeList) {
        try {
          this.productService.approveContent(storeId, productCode, true, true);
        } catch (Exception e) {
          LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
        }
      }
      isSuccess = true;
    } catch (Exception e) {
      throw e;
    }
    return new GdnBaseRestResponse(errorMessage, null, isSuccess, requestId);
  }

  @RequestMapping(value = ProductControllerPath.CREATE_PRODUCT, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "create product at controller", description = "create product at controller")
  @ResponseBody
  public GdnRestSimpleResponse<String> create(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "false") Boolean validateCategory,
      @RequestBody CreateProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "create", request.getBusinessPartnerCode(),
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
        request.getProductCode(), request.toString());
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);

    if(validateCategory){
      this.productService.validateCategory(requestId, username, request.getBusinessPartnerCode(),
          request.getProductCategories().get(0).getCategory().getId());
    }

    String productCode = this.productWorkflowService
        .create(request.getBusinessPartnerCode(), request.getBusinessPartnerName(), request.getProductCreationType(),
            request);

    if (StringUtils.isEmpty(request.getBusinessPartnerCode())) {
      this.productWorkflowService.createDirect(productCode);
    }
    return new GdnRestSimpleResponse<String>(requestId, productCode);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.APPROVE_DRAFT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "approve draft at controller", description = "approve draft at controller")
  @ResponseBody
  public GdnBaseRestResponse approveDraft(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approveDraft", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_DRAFT,
        productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    try {
      this.productWorkflowServiceWrapper.approveDraft(storeId, productCode);
      return new GdnBaseRestResponse(requestId);
    } catch (Exception e) {
      LOGGER.error("Exception in approving draft for the product, productCode:{}", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.APPROVE_QC, method = RequestMethod.GET, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "approve QC at controller", description = "approve QC at controller")
  @ResponseBody
  public GdnBaseRestResponse approveQC(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "approveQC", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_APPROVE_QC,
        productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    this.productWorkflowService.approveQC(productCode);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductControllerPath.FILTER_PRODUCT_CODE_EXACT,
      method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product summary by productCode",
      description = "get product summary by storeId and productCode")
  @ResponseBody
  public GdnRestListResponse<ProductResponse> filterProductSummaryByProductCodeExactMatch(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(
          defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @PathVariable("productCode") String productCode) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductSummaryByProductCodeExactMatch", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + productCode, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<Product> products =
        this.productService.findByProductCodeExactMatch(storeId, productCode, pageable);
    return new GdnRestListResponse<>(null, null, true, getWrapperForProductResponse(products),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            products.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductControllerPath.COUNT_VIEWABLE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product summary by viewable",
      description = "get product summary by storeId and viewable")
  @ResponseBody
  public GdnRestSimpleResponse<Integer> getProductCountsByViewable(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("viewable") boolean viewable) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductCountsByViewable", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        String.valueOf(viewable), null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    int productCount = this.productService.getProductCountByViewable(storeId, viewable);
    return new GdnRestSimpleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
        requestId, productCount);
  }

  private List<ProductResponse> getWrapperForProductResponse(Page<Product> products) {
    List<ProductResponse> wrapper = new ArrayList<>();
    for (Product product : products.getContent()) {
      ProductResponse wrapperElement = new ProductResponse();
      BeanUtils.copyProperties(product, wrapperElement);
      wrapper.add(wrapperElement);
    }
    return wrapper;
  }

  @RequestMapping(value = ProductControllerPath.COLLECTION_NOTIFY_AGE_BETWEEN, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "notify for recently approved products filter by age between",
      description = "notify for recently approved products filter by age between")
  @ResponseBody
  public GdnBaseRestResponse notifyMerchantOfRecentApprovedProducts(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(
          defaultValue = "12") Integer interval) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "notifyMerchantOfRecentApprovedProducts", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_NOTIFY_EMAIL,
        interval.toString(), null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));

    Calendar instance = Calendar.getInstance();
    Date endAge = instance.getTime();
    instance.add(Calendar.HOUR_OF_DAY, -interval);
    Date startAge = instance.getTime();
    instance.setTime(endAge);
    instance.add(Calendar.SECOND, -1);
    endAge = instance.getTime();
    this.productService.notifyMerchantOfRecentApprovedProducts(startAge, endAge);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_PRODUCT_CONTENT, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product content", description = "update product content")
  @ResponseBody
  public GdnBaseRestResponse updateProductContent(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateProductContent", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_UPDATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    this.productService.updateProductContent(request);
    return new GdnBaseRestResponse(requestId);
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.UPDATE_PRODUCT_IMAGE, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product image", description = "update product image")
  @ResponseBody
  public GdnBaseRestResponse updateProductImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductRequest request) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "updateProductImage", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_IMAGE_UPDATE,
        request.getProductCode(), request.toString());

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
        checkOnlyNewImagesForExtension);
    this.productService.updateProductImage(request);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductControllerPath.PRODUCT_DETAILS_BY_PRODUCT_CODES, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product details by product code list", description = "get product details "
      + "by product code list")
  @ResponseBody
  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody StringListRequest productCodeList) {
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductDetailsByProductCodes", null,
          username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
          null, productCodeList.toString());

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

      GdnPreconditions.checkArgument(productCodeList != null, "productCodeList cannot be null");
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      LOGGER.debug("invoking get product details by product codes api. mandatoryRequestParam: {}",
          mandatoryRequestParam);
      return this.productService.getProductDetailsByProductCodes(requestId, username, productCodeList);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.appendLogErrorTemplate(e.getMessage(), e));
      return new GdnRestListResponse<>(e.getMessage(), null, false,
          new ArrayList<ProductDetailResponse>(), new PageMetaData(0, 0, 0), requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.FILTER_ITEM_NAME_AND_CATEGORY_ID, method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product item summary by product item name and category id",
      description = "get product item summary by product item name and category id")
  @ResponseBody
  public GdnRestListResponse<ProductItemResponse> filterProductItemSummaryByProductItemNameAndCategoryId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam(
          defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size,
      @RequestParam String productItemName, @RequestParam String categoryId) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "filterProductItemSummaryByProductItemNameAndCategoryId", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        page + ":" + size + ":" + productItemName + ":" + categoryId, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItems =
        this.productService.findByProductItemNameAndCategoryId(storeId, productItemName, categoryId, pageable);
    List<ProductItemResponse> wrapper = new ArrayList<ProductItemResponse>();
    for (ProductItem productItem : productItems) {
      ProductItemResponse wrapperElement = new ProductItemResponse();
      wrapperElement.setImages(new ArrayList<Image>());
      BeanUtils.copyProperties(productItem, wrapperElement);
      if (!productItem.getProductItemImages().isEmpty()) {
        Image productItemImage = new Image();
        BeanUtils.copyProperties(productItem.getProductItemImages().get(0), productItemImage);
        wrapperElement.getImages().add(productItemImage);
      }
      wrapper.add(wrapperElement);
    }
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, wrapper,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItems.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductControllerPath.GET_CATEGORY_HIERARCHY_BY_PRODUCT_NAME,
      method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "get category hierarchy by product name",
      description = "get category hierarchy by product name")
  public GdnRestListResponse<CategoryHierarchyResponse> getCategoryHierarchyByKeyword(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String keyword) throws Exception {
    try {
      Pageable pageable = PageRequest.of(page, size);
      Map<String, Object> datas =
          this.productService.getCategoryHierarchyByProductNameOrProductCode(storeId, keyword,
            pageable, null);
      List<CategoryHierarchyResponse> categoryHierarchyResponses =
          (List<CategoryHierarchyResponse>) datas.get(RESULT);
      if (CollectionUtils.isNotEmpty(categoryHierarchyResponses)) {
        size = categoryHierarchyResponses.size();
      }
      return new GdnRestListResponse<>(null, null, true, categoryHierarchyResponses,
          new PageMetaData(size, page, size), requestId);
    } catch (Exception e) {
      LOGGER.error("error getting category hierarchy by product name or or product code.", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @SuppressWarnings("unchecked")
  @RequestMapping(value = ProductControllerPath.GET_CATEGORY_HIERARCHY_BY_KEYWORD_PRODUCT_COUNT,
      method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "get category hierarchy with product count by product name or product code",
                description = "get category hierarchy with product count by product name or product code")
  public GdnRestListResponse<CategoryHierarchyProductCountResponse> getCategoryHierarchyByKeywordWithProductCount(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String keyword,
      @RequestParam(required = false) String businessPartnerCode) throws Exception {
    try {
      Pageable pageable = PageRequest.of(page, size);
      Map<String, Object> datas =
          this.productService.getCategoryHierarchyByProductNameOrProductCode(storeId, keyword,
              pageable, businessPartnerCode);
      List<CategoryHierarchyProductCountResponse> response = new ArrayList<>();
      if (datas != null && datas.containsKey(RESULT)) {
        List<CategoryHierarchyResponse> categoryHierarchyResponses =
            (List<CategoryHierarchyResponse>) datas.get(RESULT);
        for (CategoryHierarchyResponse category : categoryHierarchyResponses) {
          CategoryHierarchyProductCountResponse categoryResponse =
              new CategoryHierarchyProductCountResponse();
          BeanUtils.copyProperties(category, categoryResponse);
          categoryResponse.setCategoryHierarchy(new ArrayList<>());
          for (CategoryResponse categoryHierarchy : category.getCategoryHierarchy()) {
            CategoryResponse target = new CategoryResponse();
            BeanUtils.copyProperties(categoryHierarchy, target);
            categoryResponse.getCategoryHierarchy().add(target);
          }
          categoryResponse.setProductCount((Long) datas.get(category.getCategoryCode()));
          response.add(categoryResponse);
        }
      } else {
        LOGGER.info("For product create flow, user {} has searched : '{}', and found no result.", username, keyword);
      }
      return new GdnRestListResponse<>(null, null, true, response,
          new PageMetaData(response.size(), 0, response.size()),
          requestId);
    } catch (Exception e) {
      LOGGER.error("error getting category hierarchy with product count by product name or product code.", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.ITEM_FILTER_LIST_PRODUCT_NAME_AND_CATEGORY_CODE,
      method = RequestMethod.GET,
      produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "get product item by product name and category code",
                description = "get product item by product name and category code")
  public GdnRestListResponse<ProductItemResponse> getProductItemByProductNameAndCategoryCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productName,
      @RequestParam String categoryCode, @RequestParam(defaultValue = "true") Boolean isOnlyExternal) throws Exception {
    try {
      Pageable pageable = PageRequest.of(page, size);
      Page<ProductItemResponse> productItemResponses = this.productService
          .getProductItemByKeywordAndCategoryCode(productName, categoryCode, pageable, isOnlyExternal);
      return new GdnRestListResponse<>(null, null, true, productItemResponses.getContent(),
          new PageMetaData(productItemResponses.getSize(), page,
              productItemResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error("error getting product items by product name and category code.", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.ITEM_FILTER_KEYWORD_AND_CATEGORY_CODES, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  @Operation(summary = "find product item by keyword and category codes", description = "find product item by keyword and category codes")
  public GdnRestListResponse<ProductItemResponse> findProductItemByKeywordAndCategoryCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam(defaultValue = "true") Boolean isOnlyExternal,
      @RequestBody ProductSearchRequest request) throws Exception {
    try {
      Pageable pageable = PageRequest.of(page, size);
      LOGGER.info("For product create flow, user {} has searched : '{}' for category codes : {}", username, request
          .getKeyword(), request.getCategoryCodes());
      Page<ProductItemResponse> productItemResponses =
          this.productService.findProductItemByKeywordAndCategoryCodes(request.getKeyword(), request.getCategoryCodes(), pageable, isOnlyExternal);
      return new GdnRestListResponse<>(null, null, true, productItemResponses.getContent(),
          new PageMetaData(productItemResponses.getSize(), page, productItemResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error("error getting product items by keyword: {} , and category codes: {}, requestId: {}.", request.getKeyword(), request.getCategoryCodes(), requestId, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.ITEM_DETAIL_FILTER_BY_PRODUCT_NAME_AND_CATEGORY_CODES,
      method = RequestMethod.POST, produces = {MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "get product item detail list by product name and category codes",
                description = "get product item detail list by product name and category codes")
  public GdnRestListResponse<ProductItemDetailResponse> getProductItemDetailByProductNameAndCategoryCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(defaultValue = "true") Boolean isOnlyExternal,
      @RequestBody ProductSearchRequest request) throws Exception {
    try {
      Pageable pageable = PageRequest.of(page, size);
      Page<ProductItemDetailResponse> productItemResponses = this.productService
          .getProductItemDetailByKeywordAndCategoryCodes(request.getKeyword(), request.getCategoryCodes(), pageable, isOnlyExternal);
      return new GdnRestListResponse<>(null, null, true, productItemResponses.getContent(),
          new PageMetaData(productItemResponses.getSize(), page,
              productItemResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error("error getting product items by product name and category code.", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.CLEAR_MASTER_PRODUCT_CACHE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "clear master product cache by product code",
      description = "clear master product cache by product code")
  public GdnBaseRestResponse clearMasterProductCache(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String productCode) throws Exception {
    try {
      this.productService.clearMasterProductCache(productCode);
      return new GdnBaseRestResponse(true);
    } catch (Exception e) {
      LOGGER.error("error clearing master product cache, productCode={}", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.PRODUCT_COLLECTION_COUNT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @ResponseBody
  @Operation(summary = "count product collection by specified date range",
      description = "count product collection by specified date range")
  public GdnRestSingleResponse<ProductCollectionCountRestResponse> countProductCollectionBySpecifiedDateRange(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username,
      @RequestParam(required = false) String businessPartnerCode,
      @RequestParam(required = false) String categoryCode,
      @RequestParam(required = false) String keyword,
      @RequestParam boolean activated,
      @RequestParam boolean viewable) throws Exception {
    try {
      ProductCollectionCountRequest request = new ProductCollectionCountRequest();
      request.setStoreId(storeId);
      request.setCategoryCode(categoryCode);
      request.setBusinessPartnerCode(businessPartnerCode);
      request.setKeyword(keyword);
      request.setActivated(activated);
      request.setViewable(viewable);

      ProductCollectionCountResponse response =
          this.productService.countProductCollectionBySpecifiedDateRange(request);

      ProductCollectionCountRestResponse result = new ProductCollectionCountRestResponse();
      BeanUtils.copyProperties(response, result);
      return new GdnRestSingleResponse<ProductCollectionCountRestResponse>(result, requestId);
    } catch (Exception e) {
      LOGGER.error("error when getting product collection count by specified date range", e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.BULK_PRODUCT_WIP_DELETE, method = RequestMethod
      .POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete product by productCode", description = "delete product by productCode")
  @ResponseBody
  public GdnRestSingleResponse<PostLiveProductCountResponse> bulkDeleteProductWip(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String businessPartnerCode, @RequestParam String username,
      @RequestBody BulkDeleteProductWipRequest request) throws Exception {
    try {
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "bulkDeleteProductWIP", null, username, requestId, storeId,
              channelId, clientId, LoggerAspect.PRODUCT_DELETE,
              String.valueOf(request.getProductLevel1Ids()), request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
          LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getProductLevel1Ids()),
          ProductControllerErrorMessage.PRODUCT_LEVEL1_IDS_MUST_NOT_BE_BLANK);
      PostLiveProductCountResponse postLiveProductIdCount = productLevel1CollectionService
          .findProductCodesByStoreIdAndProductIdsInAndMarkForDeleteFalse(storeId, request.getProductLevel1Ids(),
              username, DEFAULT_NOTE, businessPartnerCode);
      return new GdnRestSingleResponse(null, null, true, postLiveProductIdCount, requestId);
    } catch (Exception e) {
      LOGGER.error("failed to delete product, productLevel1Ids : {}", request.getProductLevel1Ids(),
          e);
      return new GdnRestSingleResponse(e.getMessage(), null, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.DELETE_PRODUCT_IN_PRODUCT_COLLECTION_SOLR,
      method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE,
      consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete product by Id in prd_collection solr", description = "delete product by Id in prd_collection solr")
  @ResponseBody
  public GdnBaseRestResponse deleteProductInSolrProductCollectionById(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody GenericStringListRequest ids) {
    try {
      log.info("Delete product in prd_collection by Id list. requestId : {} , ids : {} ", requestId, ids);
      productService.deleteProductInSolrProductCollectionByIds(ids.getStringList());
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while deleting the product in prd_collection by Id list. ids : {} , error - ", ids, e);
      return new GdnBaseRestResponse(e.getErrorMessage(), ErrorCategory.VALIDATION.getCode(), false, requestId);
    } catch (Exception e) {
      log.error("Error while deleting the product in prd_collection by Id list. ids : {} , error - ", ids, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @AuditLog
  @RequestMapping(value = ProductControllerPath.GENERATE_PRODUCT_CODE, method = RequestMethod
      .GET, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete product by productCode", description = "delete product by productCode")
  @ResponseBody
  public GdnRestSimpleResponse<String> getProductCodeSequence(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username) {
    String productCode = StringUtils.EMPTY;
    try {
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "getProductCodeSequence", null, username, requestId,
              storeId, channelId, clientId, LoggerAspect.PRODUCT_CODE_SEQUENCE, null, null);

      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
          LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.info("getProductCodeSequence : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      productCode = productService.generateProductCode();

    } catch (RuntimeException e) {
      LOGGER.error("failed to get next product code sequence", e);
      return new GdnRestSimpleResponse<String>(e.getMessage(), StringUtils.EMPTY, Boolean.FALSE,
          requestId, productCode);
    }
    return new GdnRestSimpleResponse<String>(requestId, productCode);
  }

  @AuditLog
  @PostMapping(value = ProductControllerPath.CREATE_NEW_PRODUCT, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "create new product flow", description = "MTA calls to create new product")
  @ResponseBody
  public GdnBaseRestResponse createProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductCreationRequest request, @RequestParam(required = false) String flowType) {
    ApiErrorCode apiErrorCode = null;
    try {
      CommonUtils.setProductCreationType(request, clientId, flowType);
      CommonUtils.setNonNullListValuesForLogs(request);
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "create", request.getBusinessPartnerCode(), username,
              requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
              request.getProductCode(), request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
          LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.info("createProduct : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      setBopisDimension(request);
      validateCreateOrUpdateProduct(request, new ProfileResponse(), false, null, null, new ArrayList<>(), true,
          checkOnlyNewImagesForExtension);
      ValidationUtil.validateDuplicateProductAttributes(request);
      ValidationUtil.validateCreateProductItemsRequest(request, validateItemAndProductAttributeValue, variantRestrictedValues,
          variantRestrictedValuesFlag);
      if (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())) {
        ValidationUtil.validatePreOrder(request.getPreOrder(), preOrderMaximumDays, preOrderMaximumWeek, convertPreOrderDateToJKT);
      }
      productCreationValidation.validateProduct(request, familyColourValidationSwitch, imageSourceDirectory,
       fullImageSourceDirectory,
          maxStockLimit, validateBrandCode, true, false, false);
      ValidationUtil.validateDescriptiveFieldsForProductCreation(request, sanitiseProductName, sanitiseAttributeValues);
      setMdcParameters(storeId, channelId, clientId, requestId, username);
      if (StringUtils.isNotEmpty(request.getBusinessPartnerCode())) {
        Integer minimumPrice = productService.getMinimumPrice(storeId);
        validatePrice(request, minimumPrice);
        validateProductCategory(request.getProductCategories().get(0).getCategory().getCategoryCode(), new ArrayList<>(),
          request.getBusinessPartnerCode(), request.getProductType(), request.isExternalUser(),
          StringUtils.EMPTY);
        if (request.getProductItemRequests().stream().anyMatch(productItemBusinessPartnerRequest -> CollectionUtils
            .isNotEmpty(productItemBusinessPartnerRequest.getProductItemWholesalePriceRequests()))) {
          ProductCreationValidation
              .validateWholeSalePriceSettingOnFlow1(request, maxWholesalePriceRequests, minimumPrice);
          apiErrorCode = wholesaleValidationUtil
              .validateWholesaleConfigOnFlow1(request.getProductCategories().get(0).getCategory().getCategoryCode(),
                  request.getProductItemRequests(), true);
        }
      }
      boolean isLogisticsSaveSuccess = productWorkflowServiceWrapper.create(storeId, request, false, false, null);
      if (!isLogisticsSaveSuccess) {
        return new GdnBaseRestResponse(ApiErrorCode.LOGISTICS_DATA_NOT_SAVED.getDesc(),
            ApiErrorCode.LOGISTICS_DATA_NOT_SAVED.getCode(), true, requestId);
      }
    } catch (ApiDataNotFoundException e) {
      LOGGER.error(
          "createProduct : failed to save Product for productCode : {}, errorCode : {}, errorMessage : {}, exceptionType : {}, requestId : {}, flowType : {},productCreationRequest : {}",
          request.getProductCode(), e.getErrorCode(), e.getErrorMsg(), e.getExceptionType(), requestId, flowType,
          request);
      trackerService.trackProductCreationFailure(requestId, flowType, request, e.getMessage());
      return new GdnBaseRestResponse(e.getErrorMsg(), e.getErrorCode().toString(), Boolean.FALSE, requestId);
    }
    catch (ApplicationRuntimeException e) {
      LOGGER.error(
          "createProduct : failed because of validation save Product for productCode : {}, requestId : {}, flowType : {}, productCreationRequest : {}",
          request.getProductCode(), requestId, flowType, request, e);
      trackerService.trackProductCreationFailure(requestId, flowType, request, e.getMessage());
      return new GdnBaseRestResponse(e.getErrorMessage().replace(e.getErrorCodes().getMessage(), StringUtils.EMPTY),
          e.getErrorCodes().getCode(), Boolean.FALSE, requestId);
    } catch (Exception e) {
      LOGGER.error(
          "createProduct : failed to save Product for productCode : {}, requestId : {}, flowType : {}, productCreationRequest : {}",
          request.getProductCode(), requestId, flowType, request, e);
      trackerService.trackProductCreationFailure(requestId, flowType, request, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(), e.getMessage(), Boolean.FALSE, requestId);
    }
    return new GdnBaseRestResponse(Objects.nonNull(apiErrorCode) ? apiErrorCode.getDesc() : null,
        Objects.nonNull(apiErrorCode) ? apiErrorCode.getCode() : null, true, requestId);
  }

  @AuditLog
  @PostMapping(value = ProductControllerPath.CREATE_NEW_PRODUCT_V2, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "create new product flow", description = "MTA calls to create new product")
  @ResponseBody
  public GdnBaseRestResponse createNewProduct(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductCreationRequest request, @RequestParam(required = false) String flowType)
      throws JsonProcessingException {
    ApiErrorCode apiErrorCode = null;
    Map<String, OmniChannelSkuResponse> existingSellerSkusAndProductDetailsMap = new HashMap<>();
    try {
      CommonUtils.setProductCreationType(request, clientId, flowType);
      CommonUtils.setNonNullListValuesForLogs(request);
      LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "create", request.getBusinessPartnerCode(), username,
              requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_CREATE,
              request.getProductCode(), request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
          LoggerStandard.getGenericLogTemplate(loggerAttribute));
      ProductController.LOGGER.info("createProduct : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      if (validateCreateProductBasedOnSwitch) {
        validateProductCreationBasedOnSwitch(request);
      }
      setBopisDimension(request);
      ProfileResponse profileResponse = productService.getProfileResponse(request.getBusinessPartnerCode());
      if (sellerPenaltyEnabledPhase1 && CommonUtils.getBusinessPartnerFlagValue(profileResponse,
        Constants.PRODUCT_CONSEQUENCE_LIMITATION)) {
        return new GdnBaseRestResponse(ApiErrorCode.SELLER_PENALTY_RESTRICTION.getDesc(),
          ApiErrorCode.SELLER_PENALTY_RESTRICTION.getCode(), false, requestId);
      }
      validateCreateOrUpdateProduct(request, profileResponse, instoreNewFlowEnabled, request.isOff2OnChannelActive(),
          request.isB2cActivated(), request.getProductItemRequests(), true, checkOnlyNewImagesForExtension);
      ValidationUtil.validateCreateProductItemsRequest(request, validateItemAndProductAttributeValue,
          variantRestrictedValues, variantRestrictedValuesFlag);
      if(validateCreateShippingAndDimensions) {
        ApiErrorCode apiErrorCodeForDimensions =
          ValidationUtil.validateShippingAndDimension(request, maxProductDimensionLimit,
            validateProductType, instoreNewFlowEnabled);
        if(Objects.nonNull(apiErrorCodeForDimensions)){
          return new GdnBaseRestResponse(apiErrorCodeForDimensions.getDesc(),
            apiErrorCodeForDimensions.getCode(), false, requestId);
        }
      }
      if(productLimitSwitchEnabled) {
        if (CommonUtils.getBusinessPartnerFlagValue(profileResponse, Constants.PRODUCT_LIMIT)) {
          ApiErrorCode productLimitApiErrorCode =
              productService.checkProductLimitExceeded(request.getBusinessPartnerCode(), request.getStoreId());
          if (Objects.nonNull(productLimitApiErrorCode)) {
            return new GdnBaseRestResponse(productLimitApiErrorCode.getDesc(), productLimitApiErrorCode.getCode(),
                false, requestId);
          }
        }
      }
      Integer productType = Objects.nonNull(request.getProductType()) ?
        request.getProductType() :
        (request.getProductItemRequests().stream().map(ProductItemCreationRequest::getProductType)
          .filter(Objects::nonNull).findFirst().orElse(null));
      Optional<ApiErrorCode> apiErrorCodeForBopis =
        fetchApiErrorCodeBopisValidations(request, productType, profileResponse);
      if (apiErrorCodeForBopis.isPresent()) {
        return new GdnBaseRestResponse(apiErrorCodeForBopis.get().getDesc(),
          apiErrorCodeForBopis.get().getCode(), false, requestId);
      }
      if (StringUtils.isNotBlank(request.getSizeChartCode()) && sizeChartAdditionForProduct) {
        ApiErrorCode apiErrorCodeForSizeChart = productService.validateSizeChart(request.getSizeChartCode(), storeId);
        if (Objects.nonNull(apiErrorCodeForSizeChart)) {
          return new GdnBaseRestResponse(apiErrorCodeForSizeChart.getDesc(), apiErrorCodeForSizeChart.getCode(), false,
              requestId);
        }
      }
      validateMPP(storeId, request, profileResponse);
      ValidationUtil.validateFreeSample(request);
      ValidationUtil.validateDistinctProductAttributeValues(request, sizeChartValueTypeDelimiter);

      List<String> salesChannel =
          ValidationUtil.validateB2cAndB2bActivatedFlags(request, profileResponse, instoreNewFlowEnabled,
              setDefaultB2CActivated);
      boolean isSellerOmg = CommonUtils.getBusinessPartnerFlagValue(profileResponse, Constants.BLIBLI_OMG);
      if (Objects.nonNull(request.getPreOrder()) && Boolean.TRUE.equals(request.getPreOrder().getIsPreOrder())) {
        ApiErrorCode apiErrorCodeForPreOrder =
          ValidationUtil.getValidationResponseForPreOrder(request.getPreOrder(),
            preOrderMaximumDays, preOrderMaximumWeek, isSellerOmg, preOrderConfig.isPoQuotaFeatureSwitch());
        if(Objects.nonNull(apiErrorCodeForPreOrder)){
          return new GdnBaseRestResponse(apiErrorCodeForPreOrder.getDesc(),
            apiErrorCodeForPreOrder.getCode(), false, requestId);
        }
      }
      if (populateDefaultPromoskuValue && !Constants.INTERNAL.equals(request.getBusinessPartnerCode())) {
        request.setPromoSKU(true);
      }
      productCreationValidation.validateProduct(request, familyColourValidationSwitch, imageSourceDirectory,
          fullImageSourceDirectory, maxStockLimit, validateBrandCode, false, validateDuplicateUpcCodeCreate, isSellerOmg);
      productCreationValidation.bundleProductValidation(request, profileResponse);
      productCreationValidation.validateProductSizeChartAttribute(request, channelId);
      productCreationValidation.validateBasePriceForB2BSeller(request, profileResponse);
      ConverterUtil.unifyProductAttributeAndProductAttributeValueTypeRequest(request, valueTypeAdditionForDefiningAttributes, sizeChartValueTypeDelimiter);
      ValidationUtil.validateDescriptiveFieldsForProductCreation(request, sanitiseProductName, sanitiseAttributeValues);
      validateProductCategory(request.getProductCategories().get(0).getCategory().getCategoryCode(),
        salesChannel, Optional.of(request).map(ProductCreationRequest::getBusinessPartnerCode)
          .orElse(StringUtils.EMPTY), productType, request.isExternalUser(),
        Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
          .map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY));
      productService.validatePickupPointsAndFbb(requestId, request, profileResponse, existingSellerSkusAndProductDetailsMap);
      setMdcParameters(storeId, channelId, clientId, requestId, username);
      if (StringUtils.isNotEmpty(request.getBusinessPartnerCode())) {
        Integer minimumPrice = productService.getMinimumPrice(storeId);
        validateItemPickupPointPrice(request, minimumPrice);
        for (ProductItemCreationRequest productItemCreationRequest : request.getProductItemRequests()) {
          if (productItemCreationRequest.getPickupPoints().stream().anyMatch(
              pickupPoint -> CollectionUtils.isNotEmpty(pickupPoint.getProductItemWholesalePriceRequests()))) {
            ProductCreationValidation
                .validateWholeSalePriceSettingOnFlow1AtL5(request, maxWholesalePriceRequests, minimumPrice);
            apiErrorCode = wholesaleValidationUtil
                .validateWholesaleConfigOnFlow1(request.getProductCategories().get(0).getCategory().getCategoryCode(),
                    request.getProductItemRequests(), false);
          }
        }
      }
      boolean isLogisticsSaveSuccess =
          productWorkflowServiceWrapper.create(storeId, request, false, true, profileResponse);
      if (!isLogisticsSaveSuccess) {
        return new GdnBaseRestResponse(ApiErrorCode.LOGISTICS_DATA_NOT_SAVED.getDesc(),
            ApiErrorCode.LOGISTICS_DATA_NOT_SAVED.getCode(), true, requestId);
      }
    } catch (ApiDataNotFoundException e) {
      LOGGER.error(
          "createProduct : failed to save Product for productCode : {}, errorCode : {}, errorMessage : {}, exceptionType : {}, requestId : {}, flowType : {},productCreationRequest : {}",
          request.getProductCode(), e.getErrorCode(), e.getErrorMsg(), e.getExceptionType(), requestId, flowType,
          request);
      if (StringUtils.isNotBlank(e.getMessage()) && e.getMessage()
          .contains(ApiErrorCode.OMNI_CHANNEL_SKU_ALREADY_EXISTS_OR_DUPLICATE.getDesc()) && MapUtils.isNotEmpty(
          existingSellerSkusAndProductDetailsMap)) {
        e.setErrorMsg(objectMapper.writeValueAsString(
            new ProductCreationFailureResponse(existingSellerSkusAndProductDetailsMap)));
      }
      trackerService.trackProductCreationFailure(requestId, flowType, request, e.getMessage());
      return new GdnRestSingleResponse<>(e.getErrorMsg(), e.getErrorCode().getCode(), Boolean.FALSE, null, requestId);
    }
    catch (ApplicationRuntimeException e) {
      LOGGER.error(
          "createProduct : failed because of validation save Product for productCode : {}, requestId : {}, flowType : {}, productCreationRequest : {}",
          request.getProductCode(), requestId, flowType, request, e);
      trackerService.trackProductCreationFailure(requestId, flowType, request, e.getMessage());
      return new GdnRestSingleResponse<>(e.getErrorMessage().replace(e.getErrorCodes().getMessage(), StringUtils.EMPTY),
          e.getErrorCodes().getCode(), Boolean.FALSE, null, requestId);
    } catch (Exception e) {
      LOGGER.error(
          "createProduct : failed to save Product for productCode : {}, requestId : {}, flowType : {}, productCreationRequest : {}",
          request.getProductCode(), requestId, flowType, request, e);
      trackerService.trackProductCreationFailure(requestId, flowType, request, e.getMessage());
      return new GdnBaseRestResponse(e.getMessage(), e.getMessage(), Boolean.FALSE, requestId);
    }
    return new GdnBaseRestResponse(Objects.nonNull(apiErrorCode) ? apiErrorCode.getDesc() : null,
        Objects.nonNull(apiErrorCode) ? apiErrorCode.getCode() : null, true, requestId);
  }

  private Optional<ApiErrorCode> fetchApiErrorCodeBopisValidations(ProductCreationRequest request,
    Integer productType, ProfileResponse profileResponse) {
    ApiErrorCode bpBopisApiErrorCode =
      productService.checkBpBopisEligibility(productType, profileResponse);
    ApiErrorCode apiErrorCodeForCNCBopis =
      ValidationUtil.validateCncWithBopis(request, bopisCNCRestrictionEnabled);
    Optional<ApiErrorCode> apiErrorCodeForBopis =
      Stream.of(bpBopisApiErrorCode, apiErrorCodeForCNCBopis).filter(Objects::nonNull).findFirst();
    return apiErrorCodeForBopis;
  }

  private void validateMPP(String storeId, ProductCreationRequest request, ProfileResponse profileResponse) throws Exception {
    boolean productIsMPP = ValidationUtil.checkIfProductIsMPP(request);
    if(productIsMPP) {
      if (!productService.checkIfMPPIsAllowed(storeId, request.getBusinessPartnerCode(), profileResponse)) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ProductControllerErrorMessage.MPP_NOT_ALLOWED_FOR_SELLER);
      }
    }
  }

  private void setMdcParameters(String storeId, String channelId, String clientId,
      String requestId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, requestId);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
  }

  @RequestMapping(value = ProductControllerPath.GET_STUCK_PRODUCTS, method = RequestMethod
      .GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get stuck product", description = "get stuck product")
  @ResponseBody
  public GdnRestSingleResponse<StuckProductResponse> getStuckProducts(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam String username,
      @RequestParam(defaultValue = "0") int retryBatchSizeCount) {
    StuckProductResponse stuckProductResponse = new StuckProductResponse();
    try {
    LoggerAttributeModel loggerAttribute =
          new LoggerAttributeModel(this, "getProductCodeSequence", null, username, requestId,
             storeId, channelId, clientId, LoggerAspect.PRODUCT_CODE_SEQUENCE, null, null);
    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(),
        LoggerStandard.getGenericLogTemplate(loggerAttribute));
    LOGGER.info("PBP API of get stuck products : {}", MDC.get(LoggerParam.GENERIC_LOGGER.getParam()));
      if (retryBatchSizeCount == 0) {
        retryBatchSizeCount = cronJobStuckProductBatchSize;
      }
    productServiceWrapper.getStuckProductCodeAndState(retryBatchSizeCount);
    } catch (Exception e) {
      LOGGER.error("getStuckProducts PBP : failed to get stuck products : {}", e);
      return new GdnRestSingleResponse<StuckProductResponse>(e.getMessage(), null, false, null, requestId);
    }
    return new GdnRestSingleResponse<StuckProductResponse>(stuckProductResponse, requestId);
  }

  @RequestMapping(value = ProductControllerPath.DETAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product by id", description = "get product by id")
  @ResponseBody
  public GdnRestSingleResponse<ProductDetailResponse> getProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("id") String id) throws Exception {

    LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProduct", null,
        username, requestId, storeId, channelId, clientId, LoggerAspect.PRODUCT_FETCH,
        id, null);

    MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
    ProductController.LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

    ProductDetailResponse response = this.productService.findDetailById(id);
    return new GdnRestSingleResponse<ProductDetailResponse>(null, null, true, response, requestId);
  }

  @AuditLog
  @PostMapping(value = ProductControllerPath.SCREENING_PRODUCTS_BULK_ACTIONS, produces = MediaType
      .APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Bulk Actions", description = "Bulk actions on products")
  @ResponseBody
  public GdnBaseRestResponse doScreeningProductsBulkActions(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("actionType") String actionType,
      @RequestBody ScreeningProductBulkActionsRequest request) {
    try {
      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this,
          "doBulkActions", null, username, requestId, storeId, channelId,
          clientId, LoggerAspect.BULK_ACTIONS, actionType, request.toString());
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      if(Objects.isNull(BulkActionType.getBulkActionTypeByBulkActionName(actionType))) {
        LOGGER.error(INVALID_BULK_ACTION_TYPE, actionType);
        return new GdnBaseRestResponse(INVALID_BULK_ACTION_TYPE, StringUtils.EMPTY, false, requestId);
      }
      productServiceWrapper.doScreeningProductsBulkActions(storeId,
          BulkActionType.getBulkActionTypeByBulkActionName(actionType), request);
    } catch (Exception e) {
      LOGGER.error(FAILED_BULK_ACTION, request.getProductCodes(), e);
      return new GdnBaseRestResponse(e.getMessage(), StringUtils.EMPTY, Boolean.FALSE, requestId);
    }
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductControllerPath.PRODUCT_REVISION_HISTORY, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product resubmit info", description = "Get product resubmit info by productCode")
  @ResponseBody
  public GdnRestListResponse<ProductRevisionInfoResponse> getProductRevisionHistory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) {
    try {
      LoggerAttributeModel loggerAttribute = new LoggerAttributeModel(this, "getProductRevisionHistory", null,
          username, requestId, storeId, channelId, clientId, LoggerAspect.REVISION_HISTORY,
          productCode, null);
      MDC.put(LoggerParam.GENERIC_LOGGER.getParam(), LoggerStandard.getGenericLogTemplate(loggerAttribute));
      LOGGER.debug(LoggerStandard.getLogInfoTemplate(loggerAttribute));

      List<ProductRevisionInfoResponse> response = productServiceWrapper.getProductRevisionInfo(storeId, productCode);
      return new GdnRestListResponse<>(response, new PageMetaData(response.size(), 1, response.size()), requestId);
    }catch (Exception e) {
      LOGGER.error(ERROR_WHILE_FETCHING_REVISION_HISTORY, productCode, e);
      return new GdnRestListResponse<>(ERROR_WHILE_FETCHING_REVISION_HISTORY,
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.GET_SCREENER_NOTES, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product screening notes", description = "Get product screening notes")
  @ResponseBody
  public GdnRestSingleResponse<SingleValueResponse> getScreeningNotes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) {
    SingleValueResponse singleValueResponse =
        new SingleValueResponse(productServiceWrapper.getScreeningNotes(storeId, productCode));
    return new GdnRestSingleResponse<>(singleValueResponse, requestId);
  }

  @RequestMapping(value = ProductControllerPath.QR_CODE_NOTIFICATION, method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Generate QR code download notification", description = "Generate QR code download notification")
  @ResponseBody
  public GdnBaseRestResponse generateQRCodeNotification(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam String businessPartnerCode, @RequestParam String filePath) {
    try {
      productNotificationService.sendProductQRCodeDownloadNotification(businessPartnerCode, filePath);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      LOGGER.error(QR_CODE_NOTIFICATION_ERROR, businessPartnerCode, filePath, e);
      return new GdnRestListResponse<>(QR_CODE_NOTIFICATION_ERROR, ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.RESIZE_IMAGE, method = RequestMethod.PUT, produces = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "Resize image", description = "ResizeImage")
  @ResponseBody
  public GdnBaseRestResponse resizeImage(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false) String productCode, @RequestParam(required = false) boolean deltaIndex,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "100") int size)
      throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    try {
      productServiceWrapper.resizeImages(storeId, productCode, deltaIndex, page, size);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while resizing image product_code:{}", productCode, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_PRODUCT_AS_POST_LIVE, method = RequestMethod.PUT, produces = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "Update product as post live", description = "Update product as post live")
  @ResponseBody
  public GdnBaseRestResponse updateProductAsPostLive(
      @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode) throws Exception {
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    try {
      productService.updateProductAsPostLiveTrue(storeId, productCode);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while updating product as postLive : {}", productCode, e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.PRODUCT_FILTER, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Search for L1 product data. B2B Api", description = "Search for L1 product data. B2B Api")
  @ResponseBody
  public GdnRestListResponse<ProductFilterResponse> getProductsByFilter(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody ProductFilterRequest productFilterRequest,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    LOGGER.info("Fetching products, L1 level data, for request : {}", productFilterRequest);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, clientId);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, channelId);
    try{
      Page<ProductFilterResponse> productFilterResponsePage =
          this.productService.getProductListFilter(storeId, productFilterRequest, page, size);
      return new GdnRestListResponse<>(null, null, true, productFilterResponsePage.getContent(),
          new PageMetaData(size, page, productFilterResponsePage.getTotalElements()), requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while fetching product data for : {}", productFilterRequest, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_PRODUCT_CATEGORY, method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update product category", description = "Update product category")
  @ResponseBody
  public GdnBaseRestResponse updateProductCategory(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode, @RequestParam String categoryCode) throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      productServiceWrapper.updateProductCategory(storeId, productCode, categoryCode, true);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("Exception caught while changing product category: {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while changing product category: {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_REVIEW_TYPE, method = RequestMethod.PUT, produces = MediaType
      .APPLICATION_JSON_VALUE)
  @Operation(summary = "Update review type", description = "Update review type")
  @ResponseBody
  public GdnBaseRestResponse updateReviewType(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode, @PathVariable("reviewType") String reviewType) throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      productService.updateReviewType(storeId, productCode, reviewType);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.MINIMUM_PRICE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get the minimum price", description = "get the minimum price")
  @ResponseBody
  public GdnRestSimpleResponse<Integer> getMinimumPrice(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username) throws Exception {
    try {
      Integer response = this.productService.getMinimumPrice(storeId);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, response);
    } catch (Exception e) {
      LOGGER.error("Exception caught while fetching minimum price", e);
      return new GdnRestSimpleResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId, null);
    }
  }

  @RequestMapping(value = ProductControllerPath.ROLLBACK_MIGRATED_PRODUCT_SKU, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "rollback migrated productSku", description = "rollback migrated productSku")
  @ResponseBody
  public GdnBaseRestResponse rollbackMigratedProductSku(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestBody List<String> productSkus) throws Exception {
    try {
      productMigrationWrapperService.updateMigratedProductCode(productSkus);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while rolling back migrated product", e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.RETRY_RESIZE_EDITED_IMAGE, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "retry resize edited images", description = "retry resize edited images")
  @ResponseBody
  public GdnBaseRestResponse retryResizeEditedImage(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode) throws Exception {
    try {
      this.productWorkflowService.retryResizeEditedImages(productCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while retry resize edited images ", e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.GET_PRODUCT_SKUS_BY_PRODUCT_CODE, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get list of skus for product code", description = "get list of skus for product code")
  @ResponseBody
  public GdnRestSimpleResponse<ProductSkuResponseList> getProductSkusByProductCode(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCode) throws Exception {
    List<String> productSkus;
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
          ProductControllerErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK);
      productSkus = productBusinessPartnerService.getProductSkusByProductCode(productCode);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, new ProductSkuResponseList(productSkus));
    } catch (Exception e) {
      LOGGER.error("Exception caught fetching L3s for productCode : {} ", productCode, e);
      return new GdnRestSimpleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId, null);
    }
  }

  @RequestMapping(value = ProductControllerPath.PUBLISH_EDITED_EVENT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish edited product event", description = "publish edited product event")
  @ResponseBody
  public GdnBaseRestResponse publishEditedEvent(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @RequestParam String reviewType, @PathVariable("productCode") String productCode) throws Exception {
    try {
      GdnPreconditions.checkArgument(REVIEW_TYPE_LIST.contains(reviewType), "Invalid review Type");
      productService.publishEditedProduct(storeId, productCode, reviewType);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while publishing edited product event for productCode : {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.PUBLISH_REVISION_EVENT, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "publish edited product event", description = "publish edited product event")
  @ResponseBody
  public GdnBaseRestResponse publishRevisedEvent(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode) {
    try {
      LOGGER.info("Republishing revised event for productCode : {} ", productCode);
      productServiceWrapper.publishRevisedEvent(storeId, productCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while publishing revised product event for productCode : {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.GET_VENDOR_NOTES, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product vendor notes", description = "Get product vendor notes")
  @ResponseBody
  public GdnRestSimpleResponse<VendorNotesResponse> getVendorNotes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) {
    try {
      VendorNotesResponse vendorNotesResponse = productServiceWrapper.getVendorNotes(storeId, productCode);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, vendorNotesResponse);
    } catch (Exception e) {
      LOGGER.error("Exception caught while fetching vendor notes for productCode : {} ", productCode, e);
      return new GdnRestSimpleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId, null);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_VENDOR_NOTES, method = RequestMethod.PUT, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Get product vendor notes", description = "Get product vendor notes")
  @ResponseBody
  public GdnBaseRestResponse updateVendorNotes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode,
      @RequestBody VendorNotesRequest vendorNotesRequest) {
    try {
      productServiceWrapper.updateVendorNotes(storeId, productCode, vendorNotesRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while updating vendor notes for productCode : {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.NEED_REVISION_SUBMIT, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Need revision submit", description = "Need revision submit")
  @ResponseBody
  public GdnRestSimpleResponse<EditProductV2Response> needRevisionSubmit(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody NeedRevisionSubmitRequest needRevisionSubmitRequest) {
    LOGGER.info("Submitting need revision for request: {} ", needRevisionSubmitRequest);
    try {
      EditProductResponse editProductResponse =
          productServiceWrapper.needRevisionSubmit(storeId, username, needRevisionSubmitRequest);
      EditProductV2Response response = ResponseHelper.toEditProductV2Response(editProductResponse);
      return new GdnRestSimpleResponse<>(null, null, true, requestId, response);
    } catch (Exception e) {
      LOGGER.error("Exception caught when submitting need revision for request: {} ", needRevisionSubmitRequest, e);
      return new GdnRestSimpleResponse<>(null, null, false, requestId, null);
    }
  }

  @RequestMapping(value = ProductControllerPath.CHECK_AUTO_APPROVAL_ELIGIBILITY, method = RequestMethod.POST, produces =
    MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Return auto approval type", description = "Return auto approval type")
  public GdnRestSingleResponse<AutoApprovalTypeResponse> getAutoApprovalType(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(defaultValue = "false") boolean onlyCategoryChange,
      @PathVariable("productCode") String productCode, @RequestBody AutoApprovalTypeRequest autoApprovalTypeRequest) {
    LOGGER.info("Fetching auto approval type for product code : {}, request : {}", productCode, autoApprovalTypeRequest);
    try {
      AutoApprovalTypeResponse autoApprovalType =
          this.productServiceWrapper.findAutoApprovalTypeByRequest(storeId, username, productCode, onlyCategoryChange,
              autoApprovalTypeRequest);
      return new GdnRestSingleResponse<>(null, null, true, autoApprovalType, requestId);
    } catch (Exception e) {
      LOGGER.error("Error fetching auto approval type for product code : {}, error - ", productCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.GET_PRODUCT_STATUS, method = RequestMethod.GET, produces =
          MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get product state", description = "Get product state")
  public GdnRestSingleResponse<SimpleStringResponse> getProductStatus(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @PathVariable("productCode") String productCode) {
    LOGGER.info("Fetching status for product code : {} ", productCode);
    try {
      return new GdnRestSingleResponse<>(null, null, true,
              SimpleStringResponse.builder().result(this.productService.getProductStatus(storeId, productCode)).build(), requestId);
    } catch (Exception e) {
      LOGGER.error("Error fetching status for product code : {}, error - ", productCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.FETCH_IN_PROGRESS_PRODUCTS_BY_MERCHANT_CODE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get in progress products by merchant code", description = "Get in progress products by merchant code")
  @ResponseBody
  public GdnRestListResponse<InProgressProductResponse> fetchInProgressProductsByMerchantCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @PathVariable("merchantCode") String merchantCode,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "100") int size) {
    try {
      InProgressProductResponsePageResponse inProgressProductResponsePageResponse =
          productService.findInProgressProductsByMerchantCode(storeId, merchantCode, page, size);
      return new GdnRestListResponse<>(null, null, true,
          inProgressProductResponsePageResponse.getInProgressProductResponses(),
        new PageMetaData(inProgressProductResponsePageResponse.getTotalNumFound(), page,
          inProgressProductResponsePageResponse.getTotalNumFound()), requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while fetching in progress products for merchantCode :{} ", merchantCode, e);
      return new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null,
          new PageMetaData(size, page, 0), requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.RETRY_AUTO_NEED_REVISION, method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Return auto approval type", description = "Return auto approval type")
  public GdnRestSingleResponse<RetryAutoNeedRevisionResponse> retryAutoNeedRevision(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody RetryNeedRevisionRequest retryNeedRevisionRequest) {
    LOGGER.info("Auto need revision request : {}", retryNeedRevisionRequest);
    try {
      MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
      RetryAutoNeedRevisionResponse retryAutoNeedRevisionResponse =
          productServiceWrapper.retryAutoNeedRevision(retryNeedRevisionRequest, true);
      return new GdnRestSingleResponse<>(null, null, retryAutoNeedRevisionResponse.isSuccess(),
          retryAutoNeedRevisionResponse, requestId);
    } catch (Exception e) {
      LOGGER.error("Error auto revision for request : {}, error - ", retryNeedRevisionRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.GET_COGS_VALUE_BY_MATERIAL_CODE, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get cogs value by material code", description = "Get cogs value by material code")
  @ResponseBody
  public GdnRestSingleResponse<CogsValueResponse> getCogsValueByMaterialCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("materialCode") String materialCode) {
    try {
      CogsValueResponse cogsValueResponse = productService.fetchCogsValueByMaterialCode(storeId, materialCode);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, cogsValueResponse, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while fetching cogs value for materialCode :{} ", materialCode, e);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_PBP_PRODUCT_WORKFLOW, method =
    RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update pbp_product_worflow by productCode", description = "Update pbp_product_worflow by productCode")
  @ResponseBody
  public GdnBaseRestResponse updatePbpWorkflowByProductCode(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam String username, @PathVariable("productCode") String productCode,
    @RequestParam String state) {
    log.info("Updating pbp_product_workflow for product : {}, to state : {}", productCode, state);
    try {
      this.productService.updatePbpProductWorkflowState(storeId, productCode, state);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error updating pbp_product_workflow for product : {}, to state : {}, error - ",
        productCode, state, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_REVIEW_PENDING, method = RequestMethod.PUT,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update product review pending", description = "Update product review pending")
  @ResponseBody
  public GdnBaseRestResponse updateReviewPending(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode, @RequestParam boolean reviewPending) throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      productServiceWrapper.updateReviewPending(storeId, productCode, reviewPending);
      return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("Exception caught while changing product review pending flag: {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while changing product review pending flag: {} ", productCode, e);
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.UPDATE_ACTIVATED_AND_VIEWABLE, method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update product viewable flag", description = "Update product viewable flag")
  @ResponseBody
  public GdnBaseRestResponse updateViewable(@RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode, @RequestParam boolean activated, @RequestParam boolean viewable)
      throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    productServiceWrapper.updateActivatedAndViewable(storeId, productCode, activated, viewable);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, requestId);
  }

  @RequestMapping(value = ProductControllerPath.EDIT_PRICE_STOCK_VARIANTS_INFO, method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Edit L5 price stock", description = "Edit L5 price stock")
  @ResponseBody
  public GdnRestSingleResponse<ItemsPriceStockImagesUpdateResponse> editPriceStockVariantsInfo(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ProductVariantUpdateRequest productVariantUpdateRequest) throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    ItemsPriceStockImagesUpdateResponse itemsPriceStockImagesUpdateResponse = new ItemsPriceStockImagesUpdateResponse();
    try {
      if (webpConversionEnabled && !productVariantUpdateRequest.isNeedCorrection()) {
        CommonUtils.overrideImageExtensionToWebpInMoreThan50VCase(productVariantUpdateRequest);
      }
      ProductLevel3 productLevel3 = new ProductLevel3();
      productLevel3.setBusinessPartnerCode(productVariantUpdateRequest.getBusinessPartnerCode());
      productVariantUpdateRequest.setOnlyL5Update(true);
      itemsPriceStockImagesUpdateResponse = productService
          .editPriceStockVariantsInfo(storeId, productLevel3, productVariantUpdateRequest,
            new EditProductResponse(), false);
      return new GdnRestSingleResponse<>(null, null, true, itemsPriceStockImagesUpdateResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      LOGGER.error("Exception caught while update L5 data with request : {} ", productVariantUpdateRequest, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false,
          itemsPriceStockImagesUpdateResponse, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while update L5 data with request : {} ", productVariantUpdateRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, itemsPriceStockImagesUpdateResponse, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.DELETE_TERMINATED_SELLER_PRODUCTS, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Delete terminated seller products", description = "Delete terminated seller products")
  @ResponseBody
  public GdnBaseRestResponse deleteTerminatedSellerProducts(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productSku") String productSku) throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
    try {
      productServiceWrapper.deleteTerminatedSellerNonSharedProducts(storeId, productSku);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while deleteTerminatedSellerProducts productSku : {} ", productSku, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.GET_HALAL_PRODUCT_HISTORY, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get halal product history", description = "Get halal product history by productSku")
  @ResponseBody
  public GdnRestListResponse<HalalProductHistoryResponse> getHalalProductHistory(@RequestParam String storeId,
      @RequestParam String requestId, @RequestParam String username, @PathVariable("productSku") String productSku,
      @RequestParam(defaultValue = "0") Integer page, @RequestParam(defaultValue = "10") Integer size) {
    log.info("Getting halal product history. storeId : {}, requestId : {}, productSku : {} ", storeId, requestId,
        productSku);
    try {
      Page<HalalProductHistoryResponse> halalProductHistoryResponses =
          halalHistoryUpdateService.getHalalProductHistory(storeId, productSku, PageRequest.of(page, size));
      return new GdnRestListResponse<>(null, null, true, halalProductHistoryResponses.getContent(),
          new PageMetaData(size, page, halalProductHistoryResponses.getTotalElements()), requestId);

    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching halal product history. storeId : {}, requestId : {}, productSku : {} ",
          storeId, requestId, productSku);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, null, null,
          requestId);
    } catch (Exception e) {
      log.error("Error while fetching halal product history. storeId : {}, requestId : {}, productSku : {} ",
          storeId, requestId, productSku);
      return new GdnRestListResponse<>(e.getMessage(), null, false, null, null, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.PROCESS_PRODUCT_VENDOR_SEARCH_AUTO_HEAL, method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Process product which are not found in vendor search", description = "Process product which are not found in vendor search")
  @ResponseBody
  public GdnBaseRestResponse processProductVendorSearchAutoHeal(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode) {
    log.info("Processing product eligible for vendor search autoheal. storeId : {}, productCode : {} ", storeId,
        productCode);
    try {
      productServiceWrapper.processProductVendorSearchAutoHeal(storeId, productCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while processing product eligible for vendor search autoheal. storeId : {}, productCode : {} ",
          storeId, requestId, productCode);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.APPEAL_IN_PROGRESS_PRODUCT, method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update appeal product for in progress", description = "Update appeal product for in progress")
  public GdnRestSingleResponse<AppealProductResponse> updateAppealInProgressProduct(
    @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
    @RequestParam String requestId, @RequestParam String username,
    @RequestBody AppealProductRequest appealProductRequest) throws Exception {
    MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId,
      username, null);
    AppealProductResponse productResponse = new AppealProductResponse();
    try {
      productResponse = productAppealService.updateAppealProductForInProgressProducts(storeId,
        appealProductRequest);
      if (StringUtils.isNotEmpty(productResponse.getErrorCode())) {
        return new GdnRestSingleResponse<>(productResponse.getErrorMessage(),
          productResponse.getErrorCode(), false, productResponse, requestId);
      }
      return new GdnRestSingleResponse<>(null, null, true, productResponse, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while appealing product: {} ",
        appealProductRequest.getProductCode(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, productResponse, requestId);
    }
  }

  @GetMapping(value = ProductControllerPath.GET_APPEAL_PRODUCT_ELIGIBILITY, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch appeal product config", description = "Fetch appeal product config")
  public GdnRestSingleResponse<AppealProductConfigResponse> fetchAppealProductConfig(
    @RequestParam String storeId, @RequestParam String requestId,
    @PathVariable("businessPartnerCode") String businessPartnerCode) {
    AppealProductConfigResponse appealProductConfigResponse = new AppealProductConfigResponse();
    try {
      log.info("Fetching appeal product config for bp-code {}", businessPartnerCode);
      appealProductConfigResponse =
        productAppealService.fetchAppealProductConfig(storeId, businessPartnerCode);
      return new GdnRestSingleResponse<>(null, null, true, appealProductConfigResponse, requestId);
    } catch (Exception e) {
      LOGGER.error("Exception caught while getting appeal product config bp: {} ",
        businessPartnerCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, appealProductConfigResponse,
        requestId);
    }
  }

  @PostMapping(value = ProductControllerPath.MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product basic details by itemSkus", description = "get product basic details by itemSkus")
  public GdnBaseRestResponse migrateProductAndL5DetailByProductSku(@RequestParam String storeId,
    @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
    @RequestParam(required = false) String username,
    @RequestBody ProductAndL5MigrationRequest productAndL5MigrationRequest) throws Exception {
    try {
      productMigrationWrapperService.migrateProductAndL5DetailsByProductSku(storeId,
        productAndL5MigrationRequest, username);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      return new GdnBaseRestResponse(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        requestId);
    }
  }

  @PostMapping(value = ProductControllerPath.GET_DISTINCT_BUSINESS_PARTNER_CODE_FOR_REVISED_PRODUCT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get distinct business partner code for revised product", description = "get distinct business partner code for revised product")
  public GdnRestListResponse<BusinessPartnerCodeResponseList> getDistinctBusinessPartnerCodesForNeedRevisionProducts(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestBody NeedRevisionProductsRequest needRevisionProductsRequest, @RequestParam int page, @RequestParam int size) throws Exception {
    try {
      Pageable pageRequest = PageRequest.of(page, size);
      Page<String> businessPartnerCodesPage =
          productService.getDistinctBusinessPartnerCodesForNeedRevisionProduct(storeId, needRevisionProductsRequest,
              pageRequest);
      BusinessPartnerCodeResponseList businessPartnerCodeResponseList =  new BusinessPartnerCodeResponseList();
      businessPartnerCodeResponseList.setBusinessPartCodeList(businessPartnerCodesPage.getContent());
      return new GdnRestListResponse<>(null, null, true, Collections.singletonList(businessPartnerCodeResponseList),
          new PageMetaData(size, page, businessPartnerCodesPage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Error while fetching distinct business partner code for need revision products for requestId : {}, needRevisionProductsRequest : {} , error - ",
          requestId, needRevisionProductsRequest, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, requestId);
    } catch (Exception e) {
      log.error(
          "Error while fetching distinct business partner code for need revision products for requestId : {}, needRevisionProductsRequest : {} , error - ",
          requestId, needRevisionProductsRequest, e);
      return new GdnRestListResponse<>(ErrorMessages.SYSTEM_ERROR, ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @GetMapping(value = ProductControllerPath.GET_ALL_PRODUCTS_BY_BUSINESS_PARTNER_CODE_FOR_LAST_X_DAYS, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all products for a business partner code in last x number of days", description = "get all products for a business parnter code in last x number of days")
  public GdnRestListResponse<ProductCodeAndNameDetails> getAllProductsByBusinessPartnerCodeForLastXDays(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String businessPartnerCode,
      @RequestParam int page, @RequestParam int size) {
    try {
      Pageable pageable = PageRequest.of(page, size);
      Page<ProductCodeAndNameDetails> productCodeAndNameDetails =
          productService.fetchProductsByBusinessPartnerCodeForLastXDays(storeId, businessPartnerCode, pageable);
      return new GdnRestListResponse<>(null, null, true, productCodeAndNameDetails.getContent(),
          new PageMetaData(page, size, productCodeAndNameDetails.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error(
          "Error while fetching need revision products details for requestId : {}, business partner code : {} , error - ",
          requestId, businessPartnerCode, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false, requestId);
    } catch (Exception e) {
      log.error(
          "Error while fetching need revision products details for requestId : {}, business partner code : {} , error - ",
          requestId, businessPartnerCode, e);
      return new GdnRestListResponse<>(ErrorMessages.SYSTEM_ERROR, ErrorCategory.UNSPECIFIED.getCode(), false,
          requestId);
    }
  }

  @RequestMapping(value = ProductControllerPath.CHECK_IF_SELLER_SKU_EXISTS, method = RequestMethod.POST)
  @Operation(summary = "Check if seller omni channel sku already exists", description = "Check if seller omni channel sku already exists")
  GdnRestSingleResponse<OmniChannelMapAndSkuResponse> checkIfSellerSkuExists(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody OmniChannelExistsRequest omniChannelSkuRequest) throws Exception {
    try {
      return new GdnRestSingleResponse<>(productLevel3V2Wrapper.checkIfOmniChannelSkuExists(storeId,
          new OmniChannelSkuRequest(omniChannelSkuRequest.getSellerCode(), omniChannelSkuRequest.getSellerSkus())),
          requestId);
    } catch (Exception e) {
      log.error("Error while checking if seller sku already exists for request {} error  ", omniChannelSkuRequest, e);
      return new GdnRestSingleResponse<>(e.getMessage(), null, false, null, requestId);
    }
  }

  @GetMapping(value = ProductControllerPath.GET_PRODUCTS_BY_BRAND_NAME, produces =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get all products mapped to a brand name", description = "get all products"
      + " mapped to a brand name")
  public GdnRestListResponse<ProductAndBrandResponse> getProductsByBrandName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String brandName,
      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size) {
    try {
      log.info("Get all products mapped to the brand - {}", brandName);
      Pageable pageable = PageRequest.of(page, size);
      Page<ProductAndBrandResponse> productCodeAndNameDetails =
          productService.getProductsByBrandName(storeId, brandName, pageable);
      return new GdnRestListResponse<>(null, null, true, productCodeAndNameDetails.getContent(),
          new PageMetaData(size, page, productCodeAndNameDetails.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching products mapped to the brand - {} , error - ", brandName, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false,
          requestId);
    } catch (Exception e) {
      log.error("Error while fetching products mapped to the brand - {} , error - ", brandName, e);
      return new GdnRestListResponse<>(ErrorMessages.SYSTEM_ERROR,
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @PostMapping(value = ProductControllerPath.UPDATE_BRAND, consumes =
      MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Update brand of a product", description = "Update brand of a product")
  public GdnBaseRestResponse updateBrand(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username,
      @RequestBody ProductBrandUpdateRequest productBrandUpdateRequest) {
    try {
      log.info("Update brand value of the product: {} with request: {}",
          productBrandUpdateRequest.getProductCode(), productBrandUpdateRequest);
      productServiceWrapper.updateProductBrandValue(storeId, productBrandUpdateRequest);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while update brand value for product: {} with request: {} , error - ",
          productBrandUpdateRequest.getProductCode(), productBrandUpdateRequest, e);
      return new GdnRestListResponse<>(e.getErrorMessage(), e.getErrorCodes().getMessage(), false,
          requestId);
    } catch (Exception e) {
      log.error("Error while update brand value for product: {} with request: {} , error - ",
          productBrandUpdateRequest.getProductCode(), productBrandUpdateRequest, e);
      return new GdnRestListResponse<>(ErrorMessages.SYSTEM_ERROR,
          ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }
}
