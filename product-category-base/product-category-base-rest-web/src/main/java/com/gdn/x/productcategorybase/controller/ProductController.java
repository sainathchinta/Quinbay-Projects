package com.gdn.x.productcategorybase.controller;

import static com.google.common.base.Preconditions.checkNotNull;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.security.NoSuchAlgorithmException;
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
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.ProductBrandDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMasterDataUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataUpdateResponse;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.exception.ValidationException;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.ObjectUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.supercsv.io.CsvBeanReader;
import org.supercsv.io.CsvBeanWriter;
import org.supercsv.io.ICsvBeanReader;
import org.supercsv.io.ICsvBeanWriter;
import org.supercsv.prefs.CsvPreference;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnDigestUtil;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.notification.dto.GdnRestSimpleResponse;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.BrandControllerErrorMessage;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.ProductApiPath;
import com.gdn.x.productcategorybase.controller.util.ConverterUtil;
import com.gdn.x.productcategorybase.controller.util.ProductControllerUtil;
import com.gdn.x.productcategorybase.controller.util.ValueTypeUtil;
import com.gdn.x.productcategorybase.csv.ProductCsv;
import com.gdn.x.productcategorybase.csv.ProductItemCsv;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.dto.ActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.GeneratedProductImagesPathDto;
import com.gdn.x.productcategorybase.dto.GeneratedProductItemImagesPathDto;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ImagePathDTO;
import com.gdn.x.productcategorybase.dto.ListHolderRequest;
import com.gdn.x.productcategorybase.dto.LocationPathAndCommonImage;
import com.gdn.x.productcategorybase.dto.ProductActivateImageDTO;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ProductAndItemLevelUpdatesDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductImagesDTO;
import com.gdn.x.productcategorybase.dto.ReplaceProductItemImagesDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeAndValueByTypeRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.BatchVatUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMultipleIdRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.NeedRevisionConfigRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductImageEditRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemImageUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemMultipleUpcCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodesSkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductItemImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleStringListRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.request.solr.AttributeReqModel;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.BasicInfoProductResponse;
import com.gdn.x.productcategorybase.dto.response.BatchVatUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductItemImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.ImagePathResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.MapResponse;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemCompleteResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductMasterDataResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ProductSalesCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.gdn.x.productcategorybase.dto.response.SingleBaseResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.ActivateImage;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
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
import com.gdn.x.productcategorybase.entity.solr.SolrProductModel;
import com.gdn.x.productcategorybase.entity.solr.SolrProductResponse;
import com.gdn.x.productcategorybase.service.AllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ImageServiceWrapper;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.TrackerService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.service.solr.SolrProductFilterService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ProductUtil;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.annotation.PostConstruct;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@RestController
@RequestMapping(value = ProductApiPath.BASE_PATH)
@Tag(name = "ProductController", description = "Master Product Service API")
public class ProductController {

  public static final String DANGEROUS_GOODS_LEVEL_MUST_NOT_BE_BLANK =
      "dangerousGoodsLevel must not be blank";
  private static final String RESULT = "result";
  public static final String PRODUCT_CODE_MUST_NOT_BE_BLANK = "productCode must not be blank";
  public static final String PRODUCT_CODES_MUST_NOT_BE_BLANK = "productCodeList cannot be null";
  public static final String INVOKING_PRODUCT_DETAIL = "invoking product detail list by product codes api. mandatoryRequestParam: {}";
  public static final String INVOKING_PRODUCT_BASIC_INFO_DETAIL= "invoking basic info product detail list by product codes api. mandatoryRequestParam: {}";
  private static final Logger LOG = LoggerFactory.getLogger(ProductController.class);
  private static final String PRODUCT_EXPORT_CSV_FILENAME = "product-export.csv";
  @SuppressWarnings("unused")
  private static final String PRODUCT_ITEM_EXPORT_CSV_FILENAME = "productitem-export.csv";
  @SuppressWarnings("unused")
  private static final String PRODUCT_ITEM_WITH_SAME_UPC_EXIST =
      "Product item with same UPC already exists";

  private static final String NO_DATA_ERROR = "No data found.";
  private static final String SHA_256 = "SHA-256";
  private static final String UTF_8 = "UTF-8";

  @Autowired
  private AllowedAttributeValueService allowedAttributeValueService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Autowired
  private ProductItemService productItemService;

  @Autowired
  private ProductService service;

  @Autowired
  private SolrProductFilterService solrProductFilterService;

  @Autowired
  private ImageService imageService;

  @Autowired
  private ImageServiceWrapper imageServiceWrapper;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ProductItemServiceWrapper productItemServiceWrapper;

  @Autowired
  private TrackerService trackerService;

  @Value("${image.source.directory}")
  private String imageSourceDirectory;

  @Value("${full.image.source.directory}")
  private String fullImageSourceDirectory;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantsSwitch;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Value("${skip.missing.variants.while.creation}")
  private boolean skipMissingVariantsWhileCreation;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${validate.item.attribute.null}")
  private boolean validateItemAttributeNull;

  @Value("${validate.item.name}")
  private boolean validateItemName;

  @Value("${duplicate.productCode.constraint.value}")
  private String duplicateProductCodeConstraint;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  @Autowired
  private FileStorageService fileStorageService;

  @Autowired
  private ObjectMapper objectMapper;

  @PostConstruct
  public void init() {
    ConverterUtil.setFileStorageService(fileStorageService);
  }

  @RequestMapping(value = ProductApiPath.FILTER_ITEM_NAME_UPC_CODE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get duplicate product names or by upc code", description =
      "get duplicate product items starting or ending with product name or upc code and "
          + "pageable")
  public GdnRestListResponse<ProductCodeResponse> getDuplicateProductItemsOrByUpcCode(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size,
      @RequestParam(defaultValue = "NO-UPC") String upcCode, @RequestParam String productName,
      @RequestBody List<AttributeReqModel>
          attributeModelList, @RequestParam String finalCategoryId)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    final List<ProductCodeResponse> productCodeResponses = new ArrayList<>();
    try {
      if (StringUtils.isEmpty(finalCategoryId)) {
        LOG.error("Skipping solr call for fetching duplicate products as final parent category is" +
            " empty for upc code {} and product name {} for request {}", upcCode, productName,
            requestId);
      } else {
        SolrProductModel model = new SolrProductModel(productName, upcCode, finalCategoryId,
            null, null);
        final Set<SolrProductResponse> codeResponses = this.solrProductFilterService
            .filterDuplicateProducts(model, size);
        codeResponses.stream().forEach((solrResponse)->productCodeResponses.add(
            new ProductCodeResponse(solrResponse.getProductCode(), solrResponse.getProductName())));
      }
    } catch (Exception e) {
      LOG.debug("Exception: ", e, e);
      return new GdnRestListResponse<>(e.getMessage(), StringUtils.EMPTY, Boolean.FALSE, null, null, requestId);
    }
    return new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, Boolean.TRUE, productCodeResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productCodeResponses.size()), requestId);
  }

  @RequestMapping(value = ProductApiPath.ACTIVATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "activate product", description = "activate product and all product item "
      + "of that product that has valid upc and sku")
  public GdnBaseRestResponse activateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    LOG.info("Invoking activateProduct: {}", request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy())
            || request.getCreatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    productServiceWrapper.activateProduct(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.CHECK_PRODUCT_BY_PRODUCT_CODE, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "check product by product code", description = "check product by product "
  + "code")
  public GdnBaseRestResponse checkProductByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode)
      throws Exception {
    long count = this.service.countByProductCode(storeId, productCode);
    GdnBaseRestResponse response;
    if (count == 0) {
      response = new GdnBaseRestResponse(null, null, false, requestId);
    }
    else {
      response = new GdnBaseRestResponse(null, null, true, requestId);
    }
    return response;
  }

  @RequestMapping(value = ProductApiPath.CHECK_PRODUCT_ITEM_BY_SKU_CODE, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "check product item by sku code", description = "check product item by sku"
      + " code")
  public GdnBaseRestResponse checkProductItemBySkuCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String skuCode)
      throws Exception {
    long count = this.productItemService.countBySkuCode(storeId, skuCode);
    GdnBaseRestResponse response;
    if (count == 0) {
      response = new GdnBaseRestResponse(null, null, false, requestId);
    }
    else {
      response = new GdnBaseRestResponse(null, null, true, requestId);
    }
    return response;
  }

  private Product convertRequestToProduct(String storeId, boolean isMerge, ProductRequest request,
      List<NewlySavedItemResponse> newlySavedItemResponseList, boolean creation)
      throws UnsupportedEncodingException, NoSuchAlgorithmException, JsonProcessingException {
    GdnPreconditions.checkArgument(
        !((request.getProductAttributes() == null) || request.getProductAttributes().isEmpty()),
        "product attributes must not null or empty");
    for (ProductAttributeRequest productAttribute : request.getProductAttributes()) {
      GdnPreconditions.checkArgument(
          (CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues())),
          "product attribute value must not null or empty");
    }
    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productAttributes", "productItems",
        "productCategories", "images", "longDescription", "distributionInfo",
        "aiGeneratedFieldsResponse");

    Map<AttributeAndValueByTypeRequest, Object> attributeAndAttributeValues =
        getAttributeAndAttributeValues(storeId, request.getProductAttributes(), request.getProductCode());
    for (ProductCategoryRequest productCategoryRequest : request.getProductCategories()) {
      product.getProductCategories()
          .add(this.convertRequestToProductCategory(storeId, product, productCategoryRequest));
    }
    ConverterUtil.setProductDetailsFromProductRequest(storeId, product, request,
        attributeAndAttributeValues, productSuitabilityFeatureEnabled, creation);
    if(isMerge){
      for(ProductItemRequest productItemRequest : request.getProductItems()) {
        product.getProductItems().add(
            ConverterUtil.convertRequestToProductItems(storeId, productItemRequest, false,
                creation));
      }
    }
    if (addDeleteVariantsSwitch && CollectionUtils.isNotEmpty(request.getNewlyAddedProductItems())) {
      log.info("Adding new items : {} for request {} ", product.getProductCode(), request.getNewlyAddedProductItems());
      for (ProductItemRequest productItemRequest : request.getNewlyAddedProductItems()) {
        StringBuilder itemHash = new StringBuilder(product.getProductCode());
        Map<String, Map<String, String>> attributeCodeValueAndValueTypeMap =
            ValueTypeUtil.getAttributeCodeValueAndValueTypeMap(product);
        productItemRequest.getProductItemAttributeValues().stream().map(
            productItemAttributeValueRequest -> ValueTypeUtil.concatAttributeIdValueAndValueType(
                productItemAttributeValueRequest, attributeCodeValueAndValueTypeMap,
                sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes, validateItemAttributeNull)).forEach(itemHash::append);
        productItemRequest.setHash(GdnDigestUtil.getDigestFromString(SHA_256, UTF_8, itemHash.toString()));
        String sequence = this.productItemService.getSequenceTransaction(product.getProductCode());
        productItemRequest.setSkuCode(product.getProductCode() + Constants.HYPHEN + sequence);
        getNewlyAddedItemResponse(newlySavedItemResponseList, product, productItemRequest);
        ProductItem productItem =
            ConverterUtil.convertRequestToProductItems(storeId, productItemRequest, true, creation);
        productItem.setVatApplicable(true);
        productItem.setNewlyAddedItem(true);
        productItem.setCreatedMerchant(product.getCreatedMerchant());
        log.info("Newly added productItemRequest : {} for productCode : {}  ", productItem, product.getProductCode());
        productItem.setProduct(product);
        product.getProductItems().add(productItem);
      }
    }
    if (ranchIntegrationEnabled && MapUtils.isNotEmpty(request.getDistributionInfoRequest())) {
      product.setDistributionInfo(objectMapper.writeValueAsString(request.getDistributionInfoRequest()));
    }
    if (StringUtils.isEmpty(product.getStoreId())) {
      product.setStoreId(storeId);
    }
    if (Objects.nonNull(request.getVideoAddEditRequest())) {
      product.setVideo(
        ConverterUtil.convertVideoAddEditRequestToDTO(request.getVideoAddEditRequest()));
    }
    if (Objects.nonNull(request.getAiGeneratedFieldsResponse())) {
      product.setAiGeneratedFields(
          objectMapper.writeValueAsString(request.getAiGeneratedFieldsResponse()));
    }
    return product;
  }

  private static void getNewlyAddedItemResponse(List<NewlySavedItemResponse> newlySavedItemResponseList,
      Product product, ProductItemRequest productItemRequest) {
    NewlySavedItemResponse newlySavedItemResponse = new NewlySavedItemResponse();
    newlySavedItemResponse.setGeneratedItemName(productItemRequest.getGeneratedItemName());
    newlySavedItemResponse.setItemCode(productItemRequest.getSkuCode());
    newlySavedItemResponse.setLength(product.getLength());
    newlySavedItemResponse.setWidth(product.getWidth());
    newlySavedItemResponse.setHeight(product.getHeight());
    newlySavedItemResponse.setWeight(product.getWeight());
    newlySavedItemResponse.setShippingWeight(product.getShippingWeight());
    newlySavedItemResponse.setDangerousGoodsLevel(productItemRequest.getDangerousGoodsLevel());
    newlySavedItemResponse.setMainImageUrl(
        productItemRequest.getImages().stream().filter(Image::isMainImages).filter(Image::isActive)
            .filter(image -> !Optional.ofNullable(image.getOriginalImage()).orElse(false)).map(Image::getLocationPath)
            .findFirst().orElse(StringUtils.EMPTY));
    if (StringUtils.isBlank(newlySavedItemResponse.getMainImageUrl())) {
      newlySavedItemResponse.setMainImageUrl(
          productItemRequest.getImages().stream().filter(Image::isActive).map(Image::getLocationPath).findFirst()
              .orElse(StringUtils.EMPTY));
    }
    newlySavedItemResponseList.add(newlySavedItemResponse);
  }

  /**
   * create map for all Attributes and there allowed values corresponding to product
   *
   * @param storeId           StoreId default "10001"
   * @param productAttributes list of product attributes request
   * @param productCode
   * @return
   */
  private Map<AttributeAndValueByTypeRequest, Object> getAttributeAndAttributeValues(String storeId,
      List<ProductAttributeRequest> productAttributes, String productCode) {
    Set<String> allowedAttributeValuesIds = new HashSet<>();
    Set<String> predefinedAllowedAttributeValuesIds = new HashSet<>();
    Set<String> predefinedAllowedAttributeValuesCodes = new HashSet<>();

    //fetch attribute ids and values ids
    List<String> attributesIds = productAttributes.stream().map(
        (ProductAttributeRequest productAttribute) -> ConverterUtil.getAttributeIds(allowedAttributeValuesIds,
            predefinedAllowedAttributeValuesIds, predefinedAllowedAttributeValuesCodes, productAttribute))
        .collect(Collectors.toList());

    return attributeService.getAttributeAndValueMap(storeId, allowedAttributeValuesIds, predefinedAllowedAttributeValuesIds,
        predefinedAllowedAttributeValuesCodes, attributesIds, productSuitabilityFeatureEnabled, productCode);
  }

  protected ProductCategory convertRequestToProductCategory(String storeId, Product product,
      ProductCategoryRequest productCategoryRequest) {
    ProductCategory productCategory = new ProductCategory(product, this.categoryService
        .findByStoreIdAndId(storeId, productCategoryRequest.getCategory().getId()), storeId);
    BeanUtils.copyProperties(productCategoryRequest, productCategory, "category", Constants.ID);
    if (StringUtils.isEmpty(productCategory.getStoreId())) {
      productCategory.setStoreId(storeId);
    }
    return productCategory;
  }

  @RequestMapping(value = ProductApiPath.CREATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "create new product", description = "create new product and generate "
      + "product item")
  public GdnBaseRestResponse createNewProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy()) || request.getCreatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(request.getProductCategories() != null,
        ErrorMessage.ENTITY_REQUIRED_PRODUCT_CATEGORY_FOR_SAVE_MESSAGE.getMessage());

    Product product = this.convertRequestToProduct(storeId, false, request, new ArrayList<>(), true);
    if (StringUtils.isEmpty(product.getStoreId())) {
      product.setStoreId(storeId);
    }
    String id = this.service.save(product);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.CREATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM,
                  method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "create new product with specification detail generated by system",
   description =
      "create new product and generate product item with specification detail generated "
          + "by system")
  public GdnBaseRestResponse createNewProductWithSpecificationDetailGenaratedBySystem(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy())
            || request.getCreatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(request.getProductCategories() != null,
        ErrorMessage.ENTITY_REQUIRED_PRODUCT_CATEGORY_FOR_SAVE_MESSAGE.getMessage());
    Product product = this.convertRequestToProduct(storeId, false, request, new ArrayList<>(), true);
    ConverterUtil.generateProductItemsFromRequest(product, request);
    if (StringUtils.isEmpty(product.getStoreId())) {
      product.setStoreId(storeId);
    }
    String id = this.service.saveProductWithSpecificationDetailGenaratedBySystem(product);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }


  @RequestMapping(value = ProductApiPath.DEACTIVATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "deactivate product", description = "deactivate product and all product "
      + "item of that product")
  public GdnBaseRestResponse deactivateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    LOG.info("Invoking deactivateProduct: {}", request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy())
            || request.getCreatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());

    productServiceWrapper.deactivateProduct(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.DISCARD, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "discard current product", description = "markfor delete selected product "
      + "and all productitem")
  public GdnBaseRestResponse discardProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    log.info("Discarding the product of productCode : {} and request : {}", request.getProductCode(), request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getCreatedBy())
            || request.getCreatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());

    this.productServiceWrapper.markForDeleteProductAndEvictCache(storeId, request.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.MIGRATE_FINAL_IMAGE_FROM_GFS_TO_GCS, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "migrate final image from gfs to gcs", description = "migrate final image "
      + "from gfs to gcs")
  public GdnBaseRestResponse migrateFinalImageFromGfsToGcs(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<String> productCodeList)
      throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodeList),
        ErrorMessage.PRODUCT_CODE_LIST_MUST_NOT_BE_EMPTY.getMessage());
    try {
      this.productServiceWrapper.migrateFinalImageFromGfsToGcs(productCodeList, storeId);
    } catch (Exception e) {
      log.error("Error while migrating finalImage from Gfs to Gcs. productCodeList : {} , error - ", productCodeList,
          e);
      return new GdnBaseRestResponse(e.getMessage(), null, false, requestId);
    }
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.ITEM_FILTER_SKU_CODES, method = RequestMethod.POST,
                  consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "filter product item summary by skuCodes", description = "filter product "
      + "item summary by skuCodes")
  public GdnRestListResponse<ProductItemDetailResponse> filterProductItemBySkuCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SkuCodesRequest request,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages)
      throws Exception {
    List<ProductItem> productItems =
        this.productItemService.findBySkuCodes(storeId, request.getSkuCodes(), request.isFetchArchived());
    Pageable pageable = PageRequest.of(0, productItems.size());
    List<ProductItemDetailResponse> productItemDetailResponses =
        new ArrayList<ProductItemDetailResponse>();
    for (ProductItem productItem : productItems) {
      ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
      productItemDetailResponse.setProductItemAttributeValueResponses(
          new ArrayList<ProductItemAttributeValueResponse>());
      productItemDetailResponse.setImages(new ArrayList<Image>());
      BeanUtils.copyProperties(productItem, productItemDetailResponse, "productItemAttributeValues",
          "productItemImages");
      ProductResponse productResponse = new ProductResponse();
      BeanUtils.copyProperties(productItem.getProduct(), productResponse, "longDescription");
      productResponse.setLongDescription(productItem.getProduct().getDescription());
      productItemDetailResponse.setProductResponse(productResponse);
      for (ProductItemAttributeValue productItemAttributeValue : productItem
          .getProductItemAttributeValues()) {
        ProductItemAttributeValueResponse productItemAttributeValueResponse =
            new ProductItemAttributeValueResponse();
        productItemAttributeValueResponse.setAttributeResponse(new AttributeResponse());
        BeanUtils.copyProperties(productItemAttributeValue.getAttribute(),
            productItemAttributeValueResponse.getAttributeResponse(), "allowedAttributeValues",
            "predefinedAllowedAttributeValues");
        productItemAttributeValueResponse.getAttributeResponse()
            .setAttributeType(productItemAttributeValue.getAttribute().getAttributeType().name());
        productItemDetailResponse.getProductItemAttributeValueResponses()
            .add(productItemAttributeValueResponse);
      }
      List<ProductItemImage> productItemImages = productItem.getProductItemImages();
      productItemImages = ConverterUtil.filterProductItemImages(originalImages, productItemImages);
      for (ProductItemImage productItemImage : productItemImages) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemDetailResponse.getImages().add(image);
      }
      productItemDetailResponses.add(productItemDetailResponse);
    }
    return new GdnRestListResponse<ProductItemDetailResponse>(null, null, true,
        productItemDetailResponses, new PageMetaData(pageable.getPageSize(),
            pageable.getPageNumber(), productItemDetailResponses.size()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_BRAND, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by brand", description = "get product by store id and brand "
      + "and pageable")
  public GdnRestListResponse<ProductResponse> getProductByBrand(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String brandName)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByBrandLike(storeId, brandName, pageable));
  }

  @GetMapping(value = ProductApiPath.PRODUCT_CODE_BY_BRAND, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get Product by brand", description = "get product by store id and brand "
      + "and pageable")
  public GdnRestListResponse<ProductCodeResponse> getProductCodeByBrand(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String brandName)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductCodeResponse> response =
        this.service.findByStoreIdAndBrandName(storeId, brandName, pageable);
    return new GdnRestListResponse<>(null, null, true, response.getContent(),
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            response.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_CATEGORIES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by list of categories", description = "get Product by list of"
      + " categories")
  public GdnRestListResponse<ProductResponse> getProductByCategories(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestBody CategoryMultipleIdRequest request) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<Product> productPage =
        this.service.findByStoreIdAndCategoriesCode(storeId, request.getCategoryCode(), pageable);
    List<ProductResponse> productResponses = new ArrayList<ProductResponse>();
    for (Product product : productPage.getContent()) {
      ProductResponse response = new ProductResponse();
      BeanUtils.copyProperties(product, response, "longDescription");
      response.setLongDescription(product.getDescription());
      productResponses.add(response);
    }
    return new GdnRestListResponse<>(null, null, true, productResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_CATEGORY, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by category", description = "get product by store id and "
      + "category and pageable")
  public GdnRestListResponse<ProductResponse> getProductByCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String categoryId)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByCategoryId(storeId, categoryId, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_NAME, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by name", description = "get product by store id and name and"
      + " pageable")
  public GdnRestListResponse<ProductResponse> getProductByName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByName(storeId, name, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_NAME_CREATED_BY, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by name and created by", description = "get product by store "
      + "id and name and created by and pageable")
  public GdnRestListResponse<ProductResponse> getProductByNameAndCreatedBy(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name,
      @RequestParam String createdBy) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByNameAndCreatedBy(storeId, name, createdBy, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_NAME_VIEWABLE_ACTIVATED, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by name, viewable and activated", description = "get product "
      + "by store id, name, viewable and activated and pageable")
  public GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name,
      @RequestParam boolean viewable, @RequestParam boolean activated) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable, this.service
        .findByNameAndViewableAndActivated(storeId, name, viewable, activated, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_NAME_VIEWABLE_ACTIVATED_UPDATED_BY, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by name, viewable and activated", description = "get product "
      + "by store id, name, viewable and activated and pageable")
  public GdnRestListResponse<ProductResponse> getProductByNameAndViewableAndActivatedAndUpdatedBy(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String name,
      @RequestParam boolean viewable, @RequestParam boolean activated,
      @RequestParam String updatedBy) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByNameAndViewableAndActivatedAndUpdatedBy(storeId, name, viewable,
            activated, updatedBy, pageable));
  }

  /**
   * @param storeId
   * @param channelId
   * @param clientId
   * @param requestId
   * @param username
   * @param page
   * @param size
   * @param productCode
   * @return
   * @throws Exception
   * @deprecated use
   * {@link #getProductByProductCodeExactMatch(String, String, String, String, String, Integer,
   * Integer, String)}
   */
  @RequestMapping(value = ProductApiPath.FILTER_PRODUCT_CODE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by product code", description = "get product by store id and "
      + "product code and pageable")
  @Deprecated
  public GdnRestListResponse<ProductResponse> getProductByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productCode)
      throws Exception {
    LOG.error("Calling API to get products from product-code, this API is deprecated, should not " +
        "be called : requestId {}, product code {}", requestId, productCode);
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByProductCode(storeId, productCode, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_SHIPPING_WEIGHT_BIGGER, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by shipping weight bigger or equal", description = "get "
      + "product by store id and shipping weight bigger or equal and pageable")
  public GdnRestListResponse<ProductResponse> getProductByShippingWeightBiggerOrEqual(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam Double shippingWeight)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByShippingWeightBiggerOrEqualThan(storeId, shippingWeight, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_SHIPPING_WEIGHT_LESSER, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by shipping weight lesser or equal", description = "get "
      + "product by store id and shipping weight lesser or equal and pageable")
  public GdnRestListResponse<ProductResponse> getProductByShippingWeightLesserOrEqual(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam Double shippingWeight)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByShippingWeightLesserOrEqualThan(storeId, shippingWeight, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_UNIQUE_SELLING_CODE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by unique selling code", description = "get product by store "
      + "id and unique selling code and pageable")
  public GdnRestListResponse<ProductResponse> getProductByUniqueSellingCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String uniqueSellingCode)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByUniqueSellingCodeLike(storeId, uniqueSellingCode, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_VIEWABLE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by viewable", description = "get product by store id and "
      + "viewable and pageable")
  @Deprecated
  public GdnRestListResponse<ProductResponse> getProductByViewable(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam boolean viewable)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    LOG.error("Calling deprecated API to get products from viewable criteria for request {}, " +
        "viewable {}, page: {}", requestId, viewable, new Object[]{pageable});
    return this.populateListResponse(requestId, pageable,
        this.service.findByViewable(storeId, viewable, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_VIEWABLE_ACTIVATED, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by viewable and activated", description = "get product by "
      + "store id and viewable and activated and pageable")
  public GdnRestListResponse<ProductResponse> getProductByViewableAndActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam boolean viewable,
      @RequestParam boolean activated) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByViewableAndActivated(storeId, viewable, activated, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_WEIGHT_BIGGER, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by weight bigger or equal", description = "get product by "
      + "store id and weight bigger or equal and pageable")
  public GdnRestListResponse<ProductResponse> getProductByWeightBiggerOrEqual(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam Double weight)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByWeightBiggerOrEqualThan(storeId, weight, pageable));
  }

  @RequestMapping(value = ProductApiPath.FILTER_WEIGHT_LESSER, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by weight lesser or equal", description = "get product by "
      + "store id and weight lesser or equal and pageable")
  public GdnRestListResponse<ProductResponse> getProductByWeightLesserOrEqual(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam Double weight)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    return this.populateListResponse(requestId, pageable,
        this.service.findByWeightLesserOrEqualThan(storeId, weight, pageable));
  }

  @RequestMapping(value = ProductApiPath.DETAIL, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail of product", description = "get list of product by id and "
      + "pageable parameter")
  public GdnRestSingleResponse<ProductDetailResponse> getProductDetail(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @PathVariable("id") String productId,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages)
      throws Exception {
    Product product = productServiceWrapper.getCompleteProductDetailByProductIdAndMarkForDeleteFalse(storeId, productId);
    ProductDetailResponse productDetailResponse = ConverterUtil.convertProductToProductDetailResponse(product,
        originalImages);
    return new GdnRestSingleResponse<>("", "", true, productDetailResponse, requestId);
  }

  @RequestMapping(value = ProductApiPath.DETAIL_BY_PRODUCT_CODE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail of product by product code", description = "get detail of "
      + "product by product code")
  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode,
      @RequestParam(required = false) boolean inAllProducts,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages)
      throws Exception {
    final List<String> categoryNames = new ArrayList<String>();
    final List<String> categoryNamesInEnglish = new ArrayList<>();
    Product product = inAllProducts ?
        productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(storeId, productCode) :
        productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId, productCode);
    ConverterUtil.sortProductImagesBySequenceId(product);
    sortProductItemImages(product);
    ProductDetailResponse productDetailResponse = inAllProducts ?
        ConverterUtil.convertProductToProductDetailResponse(product, inAllProducts, originalImages, true) :
        ConverterUtil.convertProductToProductDetailResponse(product, originalImages);
    if (!CollectionUtils.isEmpty(productDetailResponse.getProductCategoryResponses())) {
      final String categoryCode =
          productDetailResponse.getProductCategoryResponses().get(0).getCategory().getCategoryCode();
      final List<Category> categories = this.categoryService.findCategoryHierarchyByCategoryCode(storeId, categoryCode);
      productDetailResponse = ConverterUtil.setCategoryNamesProductDetailResponse(categories, productDetailResponse);
      ConverterUtil.setVideoDTO(product, productDetailResponse);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, productDetailResponse, requestId);
    } else {
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DETAIL_BY_PRODUCT_CODE_REPLACE_CATEGORY_INFO, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail of product by product code", description = "get detail of "
      + "product by product code")
  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailByProductCodeReplaceCategoryInfo(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode,
      @PathVariable("categoryCode") String categoryCode,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages)
      throws Exception {
    final List<String> categoryNames = new ArrayList<String>();
    final List<String> categoryNamesInEnglish = new ArrayList<>();
    Product product =
        productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId, productCode);
    ConverterUtil.sortProductImagesBySequenceId(product);
    sortProductItemImages(product);
    List<Category> categories;
    ProductCategory productCategoryPcb =
        product.getProductCategories().stream().filter(productCategory -> !productCategory.isMarkForDelete())
            .findFirst().orElseGet(() -> new ProductCategory());
    categories = this.categoryService.findCategoryHierarchyByCategoryCode(storeId, categoryCode);
    if (!categoryCode.equalsIgnoreCase(productCategoryPcb.getCategory().getCategoryCode())) {
      productCategoryPcb.setCategory(categories.get(0));
      product.setProductCategories(new ArrayList<>(Arrays.asList(productCategoryPcb)));
    }
    ProductDetailResponse productDetailResponse =
        ConverterUtil.convertProductToProductDetailResponse(product, originalImages);
    if (!CollectionUtils.isEmpty(productDetailResponse.getProductCategoryResponses())) {
      if (!CollectionUtils.isEmpty(categories)) {
        final int categoriesSize = categories.size() - 1;
        for (int i = categoriesSize; i >= 0; i--) {
          String categoryName = categories.get(i).getName();
          categoryNames.add(categoryName);
          String categoryNameInEnglish = categories.get(i).getNameEnglish();
          categoryNamesInEnglish.add(categoryNameInEnglish);
        }
      }
      productDetailResponse.setCategories(categoryNames);
      productDetailResponse.setCategoriesEnglish(categoryNamesInEnglish);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, productDetailResponse, requestId);
    } else {
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.BASIC_DETAIL_BY_PRODUCT_CODE, method = RequestMethod.GET
      , produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get basic details of product by product code", description = "get detail "
  + "of product by product code")
  public GdnRestSingleResponse<ProductResponse> getProductBasicDetailByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode) throws Exception {
    LOG.debug("invoking get-product-basic-details API for product={}, username={}", productCode, username);
    Product product = this.service.getProductByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, productCode);
    ProductResponse productResponse = ConverterUtil.createBasicProductResponse(product);
    return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, productResponse, requestId);
  }

  @RequestMapping(value = ProductApiPath.DETAILS_BY_PRODUCT_CODES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_XML_VALUE, MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product details by productCode list", description =
      "get product details " + "by productCode list")
  public GdnRestListResponse<ProductDetailResponse> getProductDetailListByProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody List<String> productCodeList,
      @RequestParam(required = false) boolean inAllProducts,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages) {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    List<ProductDetailResponse> responseList = new ArrayList<>();
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      GdnPreconditions.checkArgument(productCodeList != null, "productCodeList cannot be null");
      mandatoryRequestParam = MandatoryRequestParam
          .generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      LOG.debug("invoking product detail list by product codes api. mandatoryRequestParam: {}",
          mandatoryRequestParam);
      for (String productCode : productCodeList) {
        try {
          Product product = inAllProducts ?
              productServiceWrapper.getCompleteProductDetailByProductCodeInAllProducts(storeId, productCode) :
              productServiceWrapper.getCompleteProductDetailByProductCodeAndMarkForDeleteFalse(storeId, productCode);
          ProductDetailResponse productDetailResponse =
              ConverterUtil.convertProductToProductDetailResponse(product, originalImages);
          responseList.add(productDetailResponse);
        } catch (Exception e) {
          LOG.error("error retrieving productDetailResponse for productCode: {}", productCode, e);
        }
      }
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error(
          "error invoking product detail list by product codes api. mandatoryRequestParam: {}",
          mandatoryRequestParam, e);
    }
    return new GdnRestListResponse<>(errorMessage, null, isSuccess, responseList,
        new PageMetaData(responseList.size(), 0, responseList.size()), requestId);
  }

  @RequestMapping(value = ProductApiPath.DETAILS_BY_PRODUCT_CODES_FOR_BULK_DOWNLOAD, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_XML_VALUE, MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product details by productCode list", description =
      "get product details " + "by productCode list")
  public GdnRestListResponse<MasterProductResponse> getProductDetailListByProductCodesforBulkDownload(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody List<String> productCodeList) {
    String errorMessage = StringUtils.EMPTY;
    boolean isSuccess = false;
    List<MasterProductResponse> responseList = new ArrayList<>();
    MandatoryRequestParam mandatoryRequestParam = null;
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodeList), PRODUCT_CODES_MUST_NOT_BE_BLANK);
      mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      LOG.debug(INVOKING_PRODUCT_DETAIL, mandatoryRequestParam);
      List<Product> productList = this.service.getProductWithProductCategoriesAndItemsByStoreIdAndProductCodes(storeId, productCodeList);
      responseList = productList.stream().map(ConverterUtil::convertProductToMasterProductResponse)
          .collect(Collectors.toList());
      isSuccess = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error("error invoking product detail list by product codes api. product Codes list: {}",
          productCodeList, e);
    }
    return new GdnRestListResponse<>(errorMessage, null, isSuccess, responseList,
        new PageMetaData(responseList.size(), 0, responseList.size()), requestId);
  }

  @PostMapping(value = ProductApiPath.PRODUCT_BASIC_INFO_DETAILS_BY_PRODUCT_CODES, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product basic info details by productCode list", description = "get product details "
      + "by productCode list")
  public GdnRestListResponse<BasicInfoProductResponse> getBasicInfoProductDetailsListByProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestBody List<String> productCodeList) {
    String errorMessage = StringUtils.EMPTY;
    boolean success = false;
    MandatoryRequestParam mandatoryRequestParam = null;
    List<BasicInfoProductResponse> productList = new ArrayList<>();
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productCodeList), PRODUCT_CODES_MUST_NOT_BE_BLANK);
      mandatoryRequestParam =
          MandatoryRequestParam.generateMandatoryRequestParam(storeId, channelId, clientId, requestId, username, null);
      LOG.debug(INVOKING_PRODUCT_BASIC_INFO_DETAIL, mandatoryRequestParam);
      productList = this.service.getProductBasicInfoByProductCodes(storeId, productCodeList);
      success = true;
    } catch (Exception e) {
      errorMessage = e.getMessage();
      LOG.error("error invoking product detail list by product codes api. product Codes list: {} ", productCodeList, e);
    }
    return new GdnRestListResponse<>(errorMessage, null, success, productList,
        new PageMetaData(productList.size(), 0, productList.size()), requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_ITEM_ATTR_VALUES, method = RequestMethod.GET
      , produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "create new product with specification detail generated by system",
   description =
      "create new product and generate product item with specification detail generated "
          + "by system")
  public GdnBaseRestResponse getProductItemAttrValues(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productId)
      throws Exception {
    List<ProductItem> prodItem =
        this.productItemService.findProductItemByProductIdInitAttrValue(storeId, productId);
    final StringBuilder result = new StringBuilder("");
    for (ProductItem pi : prodItem) {
      result.append("|item " + pi.getSkuCode());
      for (ProductItemAttributeValue piAv : pi.getProductItemAttributeValues()) {
        result.append("attr " + piAv.getAttribute().getName() + " " + piAv.getValue() + ",");
      }
    }
    return new GdnBaseRestResponse(result.toString(), null, true, requestId);
  }


  @RequestMapping(value = ProductApiPath.GET_PRODUCT_ITEM_ATTR_VALUE_DETAIL, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get product item attribute value details", description = "get product "
      + "item attribute value details")
  public GdnRestListResponse<ProductItemResponse> getProductItemAttrValueDetails(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productId) throws Exception {
    List<ProductItem> prodItem =
        this.productItemService.findProductItemByProductIdInitAttrValue(storeId, productId);
    List<ProductItemResponse> productItemResponses = new ArrayList<>();

    for (ProductItem productItem : prodItem) {
      ProductItemResponse response = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, response, "product", "productItemAttributeValues",
          "productItemImages");
      List<ProductItemAttributeValueResponse> productItemAttributeValueResponses =
          new ArrayList<>();
      for (ProductItemAttributeValue productItemAttributeValue : productItem
          .getProductItemAttributeValues()) {
        ProductItemAttributeValueResponse productItemAttributeValueResponse =
            new ProductItemAttributeValueResponse();
        BeanUtils.copyProperties(productItemAttributeValue, productItemAttributeValueResponse,
            "attribute");
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(productItemAttributeValue.getAttribute(), attributeResponse,
            "allowedAttributeValues", "predefinedAllowedAttributeValues");
        productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
        productItemAttributeValueResponses.add(productItemAttributeValueResponse);
      }
      response.setProductItemAttributeValueResponses(productItemAttributeValueResponses);

      productItemResponses.add(response);
    }

    return new GdnRestListResponse<>(null, null, true, productItemResponses,
        new PageMetaData(productItemResponses.size(), 1, productItemResponses.size()), requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_MULTIPLE_UPC_CODE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by multiple upc code", description = "get product item "
      + "by store id and multiple upc code and pageable")
  public GdnRestListResponse<ProductItemResponse> getProductItemByMultipleUpcCode(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestBody ProductItemMultipleUpcCodesRequest request) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage;
    if (StringUtils.isEmpty(request.getSkuCode())) {
      productItemPage =
          this.productItemService.findByMultipleUpcCode(storeId, request.getUpcCodes(), pageable);
    } else {
      productItemPage = this.productItemService.findByMultipleUpcCodeExcludeOneItem(storeId,
          request.getUpcCodes(), request.getSkuCode(), pageable);
    }
    List<ProductItemResponse> productItemResponses = new ArrayList<>();
    for (ProductItem product : productItemPage.getContent()) {
      ProductItemResponse response = new ProductItemResponse();
      BeanUtils.copyProperties(product, response);
      productItemResponses.add(response);
    }
    return new GdnRestListResponse<>(null, null, true, productItemResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItemPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_ITEM_NAME, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by product item name", description = "get product item "
      + "by store id and product item name and pageable")
  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemName(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productItemName)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage = this.productItemService
        .findByStoreIdAndGeneratedItemName(storeId, productItemName, pageable);
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    for (ProductItem product : productItemPage.getContent()) {
        Image imageResponse = null;
        for (ProductItemImage productItemImage : product.getProductItemImages()) {
          if (productItemImage.isMainImages()) {
            imageResponse = new Image();
            BeanUtils.copyProperties(productItemImage, imageResponse);
            break;
          }
        }
        ProductItemResponse response = new ProductItemResponse();
        BeanUtils.copyProperties(product, response);
        response.setImages(new ArrayList<Image>());
        if (imageResponse != null){
          response.getImages().add(imageResponse);
        }
        productItemResponses.add(response);
      }
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItemPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_ITEM_NAME_CATEGORY_ID, method = RequestMethod.GET
      , produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by product item name and category id", description =
 "get product item by store id, product item name, category id and pageable")
  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemNameAndCategoryId(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productItemName,
      @RequestParam String categoryId) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage = this.productItemService
        .findByStoreIdAndGeneratedItemNameAndCategoryId(storeId, productItemName, categoryId, pageable);
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    for (ProductItem product : productItemPage.getContent()) {
        Image imageResponse = null;
        for (ProductItemImage productItemImage : product.getProductItemImages()) {
          if (productItemImage.isMainImages()) {
            imageResponse = new Image();
            BeanUtils.copyProperties(productItemImage, imageResponse);
            break;
          }
        }
        ProductItemResponse response = new ProductItemResponse();
        BeanUtils.copyProperties(product, response);
        response.setImages(new ArrayList<Image>());
        if (imageResponse != null){
          response.getImages().add(imageResponse);
        }
        productItemResponses.add(response);
      }
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItemPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_BY_NAME_AND_CATEGORY_ID, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by product item name and category id with product "
      + "details", description =
      "get product item by store " + "id, product item name, category id and pageable")
  public GdnRestListResponse<ProductItemDetailResponse> getProductItemByNameAndCategoryId(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productItemName,
      @RequestParam String categoryId) throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage = this.productItemService
        .findByStoreIdAndGeneratedItemNameAndCategoryId(storeId, productItemName, categoryId, pageable);
    List<ProductItemDetailResponse> productItemResponses =
        ConverterUtil.convertProductItemsToProductItemDetailResponseList(productItemPage.getContent());
    return new GdnRestListResponse<>(null, null, true, productItemResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), productItemPage.getTotalElements()),
        requestId);
  }


  @RequestMapping(value = ProductApiPath.FILTER_ITEM_NAME_OR_UPC_CODE, method = RequestMethod.GET
      , produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by product item name or upc code", description = "get "
      + "product item by store id and product item name or upc code and pageable")
  public GdnRestListResponse<ProductItemResponse> getProductItemByProductItemNameOrUpcCode(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String itemNameOrUpcCode,
      @RequestParam boolean viewable, @RequestParam(defaultValue = "true") boolean isOnlyExternal)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage =
        this.productItemService.findByStoreIdAndViewableAndGeneratedItemNameOrUpcCode(storeId,
            viewable, isOnlyExternal, itemNameOrUpcCode, pageable);
    List<ProductItemResponse> productItemResponses = new ArrayList<ProductItemResponse>();
    for (ProductItem product : productItemPage.getContent()) {
      Image imageResponse = null;
      for (ProductItemImage productItemImage : product.getProductItemImages()) {
        if (productItemImage.isMainImages() && !productItemImage.isMarkForDelete()) {
          imageResponse = new Image();
          BeanUtils.copyProperties(productItemImage, imageResponse);
          break;
        }
      }
      ProductItemResponse response = new ProductItemResponse();
      BeanUtils.copyProperties(product, response);
      response.setImages(new ArrayList<Image>());
      if (imageResponse != null){
        response.getImages().add(imageResponse);
      }
      productItemResponses.add(response);
    }
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItemPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_UPC_CODE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by upc code", description = "get product item by store "
      + "id and upc code and pageable")
  @Deprecated
  public GdnRestListResponse<ProductItemResponse> getProductItemByUpcCode(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String upcCode)
      throws Exception {
    LOG.error("Calling deprecating API to get product by upc code for upc code {}, page {}, size " +
        "{}", upcCode, page, size);
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage =
        this.productItemService.findByUpcCode(storeId, upcCode, pageable);
    List<ProductItemResponse> productItemResponses = convertToProductItemResponse(productItemPage);
    return new GdnRestListResponse<ProductItemResponse>(null, null, true, productItemResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItemPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.DETAIL_BY_SKU_CODE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail of product item by sku code", description = "get detail of "
      + "product item by sku code")
  public GdnRestSingleResponse<ProductItemDetailResponse> getProductItemDetailBySkuCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("skuCode") String skuCode,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages)
      throws Exception {
    ProductItem productItem = this.productItemService.findByStoreIdAndSkuCode(storeId, skuCode);
    ProductItemDetailResponse productItemDetailResponse = new ProductItemDetailResponse();
    BeanUtils.copyProperties(productItem, productItemDetailResponse);
    ProductResponse productResponse = new ProductResponse();
    BeanUtils.copyProperties(productItem.getProduct(), productResponse);
    List<Image> images = new ArrayList<>();
    List<ProductItemImage> productItemImages = productItem.getProductItemImages();
    productItemImages = ConverterUtil.filterProductItemImages(originalImages, productItemImages);
    for (ProductItemImage productItemImage : productItemImages) {
      Image image = new Image();
      BeanUtils.copyProperties(productItemImage, image);
      images.add(image);
    }
    List<ProductItemAttributeValueResponse> productItemAttributeValueResponses = new ArrayList<>();
    for (ProductItemAttributeValue productItemAttributeValue : productItem
        .getProductItemAttributeValues()) {
      if (!productItemAttributeValue.isMarkForDelete()) {
        ProductItemAttributeValueResponse productItemAttributeValueResponse =
            new ProductItemAttributeValueResponse();
        BeanUtils.copyProperties(productItemAttributeValue, productItemAttributeValueResponse);
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(productItemAttributeValue.getAttribute(), attributeResponse,
            "attributeType", "allowedAttributeValues", "predefinedAllowedAttributeValues");
        attributeResponse.setAttributeType(
            productItemAttributeValue.getAttribute().getAttributeType().toString());
        productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
        productItemAttributeValueResponses.add(productItemAttributeValueResponse);
      }
    }
    productItemDetailResponse.setProductResponse(productResponse);
    productItemDetailResponse.setImages(images);
    productItemDetailResponse
        .setProductItemAttributeValueResponses(productItemAttributeValueResponses);
    return new GdnRestSingleResponse<>("", "", true, productItemDetailResponse, requestId);
  }

  @RequestMapping(method = RequestMethod.GET, produces = {MediaType.APPLICATION_JSON_VALUE,
      MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of products", description = "get list of product and pageable"
      + " parameter")
  public GdnRestListResponse<ProductResponse> getProductSummary(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    Page<Product> productPage = this.service.findByStoreId(storeId, PageRequest.of(page, size));
    List<ProductResponse> responses = new ArrayList<ProductResponse>();
    for (Product product : productPage.getContent()) {
      ProductResponse response = new ProductResponse();
      BeanUtils.copyProperties(product, response, "longDescription");
      response.setLongDescription(product.getDescription());
      responses.add(response);
    }
    return new GdnRestListResponse<ProductResponse>(responses,
        new PageMetaData(size, page, productPage.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_MARK_FOR_DELETE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get summary of products", description = "get list of product by mark for "
      + "delete and pageable parameter")
  public GdnRestListResponse<ProductResponse> getProductSummaryByMarkForDelete(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam boolean markForDelete)
      throws Exception {
    Page<Product> productPage =
        this.service.findByMarkForDelete(storeId, markForDelete, PageRequest.of(page, size));
    List<ProductResponse> responses = new ArrayList<ProductResponse>();
    for (Product product : productPage.getContent()) {
      ProductResponse response = new ProductResponse();
      BeanUtils.copyProperties(product, response, "longDescription");
      response.setLongDescription(product.getDescription());
      responses.add(response);
    }
    return new GdnRestListResponse<ProductResponse>(responses,
        new PageMetaData(size, page, productPage.getTotalElements()), requestId);
  }

  @RequestMapping(value = ProductApiPath.SUMMARY_WITH_CATEGORIES, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get detail summary of products", description = "get list of product and "
      + "pageable parameter")
  public GdnRestListResponse<ProductDetailResponse> getProductSummaryWithCategories(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size) throws Exception {
    Page<Product> productPage =
        this.service.findByStoreIdInitProductCategories(storeId, PageRequest.of(page, size));
    List<ProductDetailResponse> responses = new ArrayList<>();
    for (Product product : productPage.getContent()) {
      ProductDetailResponse productDetailResponse = new ProductDetailResponse();
      BeanUtils.copyProperties(product, productDetailResponse, "longDescription");
      productDetailResponse.setLongDescription(product.getDescription());
      List<ProductCategoryResponse> productCategoryResponses = new ArrayList<>();
      for (ProductCategory productCategory : product.getProductCategories()) {
        productCategoryResponses
            .add(ConverterUtil.convertProductCategoryToResponse(productCategory));
      }
      productDetailResponse.setProductCategoryResponses(productCategoryResponses);
      responses.add(productDetailResponse);
    }
    return new GdnRestListResponse<>(responses,
        new PageMetaData(size, page, productPage.getTotalElements()), requestId);
  }

  private GdnRestListResponse<ProductResponse> populateListResponse(String requestId, Pageable
      pageable, Page<Product> productPage) throws Exception {
    List<ProductResponse> productResponses = new ArrayList<>();
    for (Product product : productPage.getContent()) {
      ProductResponse response = new ProductResponse();
      BeanUtils.copyProperties(product, response, "productItems", "productAttributes",
          "productCategories", "productImages", "longDescription");
      response.setLongDescription(product.getDescription());
      productResponses.add(response);
    }
    return new GdnRestListResponse<>(null, null, true, productResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.REPUBLISH_PRODUCT, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "republish products", description = "republish products and all product "
      + "item of that product")
  public GdnBaseRestResponse republishProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String operationType,
      @RequestBody List<String> productCodes) throws Exception {
    this.service.republishProductByProductCodes(storeId, productCodes, operationType);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.REPUBLISH_PRODUCT_TO_AGP, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "republish products to aggregate platform", description = "republish "
      + "products by product code")
  public GdnBaseRestResponse republishProductToAgp(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "true") boolean split, @RequestBody List<String> productCodes)
      throws Exception {
    productServiceWrapper.republishProductByProductCodesToAgp(storeId, productCodes, split);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.SAVE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "save product item", description = "save product item")
  public GdnBaseRestResponse saveProductItem(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    LOG.info("Invoking save method for : {}", request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy())
            && request.getUpdatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(
        (request.getProductItems() != null) && !request.getProductItems().isEmpty(),
        ErrorMessage.PRODUCT_ITEMS_MUST_DEFINE.getMessage());
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    for (ProductItemRequest productItemRequest : request.getProductItems()) {
      GdnPreconditions.checkArgument(
          !StringUtils.isEmpty(productItemRequest.getId()) && !StringUtils.isEmpty(productItemRequest.getSkuCode()),
          "Product item id must not empty");
      GdnPreconditions.checkArgument(productItemRequest.getDangerousGoodsLevel() != null,
          ProductController.DANGEROUS_GOODS_LEVEL_MUST_NOT_BE_BLANK);
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(productItemRequest, productItem, "product", "activated", "hash",
          "productItemAttributeValues", "images");
      productItem.setUpdatedBy(request.getUpdatedBy());
      productItem.setUpdatedDate(request.getUpdatedDate());
      productItem.setProductItemImages(
          ConverterUtil.convertRequestToProductItemImage(request, productItemRequest, productItem));
      productItems.add(productItem);
    }

    /**
     * this line of code delete because the system now allow duplicate UPC_CODE if
     * (productItemUpc.size() < request.getProductItems().size()) { throw new
     * ApplicationException(ErrorCategory.VALIDATION, PRODUCT_ITEM_WITH_SAME_UPC_EXIST); }
     */
    this.productServiceWrapper.saveProductItemDetails(storeId, productItems, request.getProductCode());

    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  /*
  This api is not used anywhere.
   */
  @RequestMapping(value = ProductApiPath.UPDATE_REJECTED_PRODUCT, method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update deleted product item", description = "update deleted product item")
  public GdnBaseRestResponse updateRejectedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    LOG.info("Invoking updateRejectedProduct : {}", request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy())
            || request.getUpdatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    Product product = this.convertRequestToProduct(storeId, true, request, new ArrayList<>(), false);
    Product savedProduct = productServiceWrapper
        .getCompleteProductDetailByProductCodeInAllProducts(storeId, request.getProductCode());
    updateToActiveState(savedProduct);
    updateToActiveState(product);
    BeanUtils.copyProperties(product, savedProduct, "productAttributes", "productItems",
        "productCategories", "images");
    savedProduct = this.service.checkBrandChanges(savedProduct);
    this.productServiceWrapper.regenerateProductAndEvictCache(storeId, savedProduct, product, false, false, true, false,
        false, false, true, new ProductAndItemLevelUpdatesDTO(), new HashSet<>());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  private Product updateToActiveState(Product product) {
    product.setMarkForDelete(false);
    product.setActivated(false);
    product.setViewable(false);
    if(!CollectionUtils.isEmpty(product.getProductAttributes())){
      for(ProductAttribute attribute : product.getProductAttributes()){
        attribute.setMarkForDelete(false);
        if(!CollectionUtils.isEmpty(attribute.getProductAttributeValues())){
          for(ProductAttributeValue value : attribute.getProductAttributeValues()){
            value.setMarkForDelete(false);
          }
        }
      }
    }
    if(!CollectionUtils.isEmpty(product.getProductCategories())){
      for(ProductCategory category : product.getProductCategories()){
        category.setMarkForDelete(false);
      }
    }

    if(!CollectionUtils.isEmpty(product.getProductImages())){
      for(ProductImage image : product.getProductImages()){
        image.setMarkForDelete(false);
      }
    }

    if(!CollectionUtils.isEmpty(product.getProductItems())){
      for(ProductItem item : product.getProductItems()){
        item.setMarkForDelete(false);
        for(ProductItemAttributeValue itemAttributeValue : item.getProductItemAttributeValues()){
          itemAttributeValue.setMarkForDelete(false);
        }
      }

    }

    return product;
  }

  @RequestMapping(value = ProductApiPath.UPDATE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product", description = "update product")
  public GdnRestListResponse<NewlySavedItemResponse> updateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false) boolean isMergeRequest,
      @RequestParam(required = false) boolean onlyVatChanged,
      @RequestParam(required = false) boolean resetExtractedAttributeValue,
      @RequestParam(required = false, defaultValue = "false") boolean ignoreSalesCategoryPublish,
      @RequestBody ProductRequest request) throws Exception {
    try {
      LOG.info("Invoking updateProduct: {}", request);
      GdnPreconditions.checkArgument(
          !(StringUtils.isEmpty(request.getUpdatedBy())
              || request.getUpdatedDate() == null),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
          ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
      List<NewlySavedItemResponse> newlySavedItemResponseList = new ArrayList<>();
      validateIfAnyNewlyAddedItemsGeneratedItemNameIsNull(request);
      Product product =
          this.convertRequestToProduct(storeId, isMergeRequest, request, newlySavedItemResponseList,
              false);
      Product savedProduct = productServiceWrapper
          .getCompleteProductDetailByProductId(storeId, request.getId());
      boolean productDetailChanged = ProductUtil.isProductDetailChanged(product, savedProduct);
      Set<String> updatedFields = CommonUtil.getUpdatedFieldForDescription(savedProduct,product);
      ProductAndItemLevelUpdatesDTO productAndItemLevelUpdatesDTO = CommonUtil.productAndItemLevelUpdates(product, savedProduct);
      CommonUtil.validateAndSetVideoToRequest(savedProduct, product, request);
      BeanUtils.copyProperties(product, savedProduct, "createdMerchant", "productAttributes",
          "productItems", "productCategories", "productImages", "version", "createdBy",
          "createdDate", "distributionInfo", "aiGeneratedFields");
      if(!isMergeRequest) {
        product = this.service.sortAndGenerateProductItem(product);
      }
      savedProduct = this.service.checkBrandChanges(savedProduct);
      if (StringUtils.isEmpty(product.getStoreId())) {
        product.setStoreId(storeId);
      }
      if (ranchIntegrationEnabled) {
        if (StringUtils.isNotBlank(product.getDistributionInfo())) {
          savedProduct.setDistributionInfo(product.getDistributionInfo());
        }
        Map<String, ProductItemUomInfoDTO> skuCodeToProductItemUomInfoDTO = new HashMap<>();
        for(ProductItemRequest productItemRequest: request.getProductItems()){
          skuCodeToProductItemUomInfoDTO.put(productItemRequest.getSkuCode(),
              productItemRequest.getProductItemUomInfoDTO());
        }

        for (ProductItem productItem : product.getProductItems()) {
          ConverterUtil.setItemUomInfo(storeId, request, productItem,
              skuCodeToProductItemUomInfoDTO.get(productItem.getSkuCode()));
        }
      }
      savedProduct = this.productServiceWrapper.regenerateProductAndEvictCache(storeId, savedProduct, product, request.getPristineCategory(), onlyVatChanged,
        request.isScoreUpdated(), productDetailChanged, false, resetExtractedAttributeValue, ignoreSalesCategoryPublish, productAndItemLevelUpdatesDTO,
          updatedFields);
      service.setProductItemIdForNewlyAddedItems(newlySavedItemResponseList, savedProduct);
      return new GdnRestListResponse<>(null, null, true, newlySavedItemResponseList,
          new PageMetaData(newlySavedItemResponseList.size(), 0, newlySavedItemResponseList.size()), requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while updating the product : {} ", request.getProductCode(), e);
      return new GdnRestListResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, requestId);
    } catch (Exception e) {
      log.error("Error while updating the product : {} ", request.getProductCode(), e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  private void validateIfAnyNewlyAddedItemsGeneratedItemNameIsNull(ProductRequest request) {
    boolean anyItemNameIsNull = request.getNewlyAddedProductItems().stream().anyMatch(
        (productItemRequest -> StringUtils.isBlank(productItemRequest.getGeneratedItemName())));
    if (validateItemName && anyItemNameIsNull) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.ITEM_NAME_SHOULD_NOT_BE_BLANK.getMessage());
    }
  }

  @PostMapping(value = ProductApiPath.UPDATE_MASTER_DATA, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product master data", description = "update product master data")
  public GdnRestSingleResponse<ProductMasterDataUpdateResponse> updateMasterData(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductMasterDataUpdateRequest request) {
    try {
      log.info("Invoking updateMasterData: {}", request);
      productServiceWrapper.updateMasterDataAndEvictCache(storeId, username, request);
      Map<String, Map<String, String>> productImagesErrorMap=null;
      if(CollectionUtils.isNotEmpty(request.getCommonImages())) {
        productImagesErrorMap =
            productServiceWrapper.updateCommonImagesAndPublishHistory(storeId, username, request);
      }
      return new GdnRestSingleResponse<>(null, null, true,
          ProductMasterDataUpdateResponse.builder().productImagesErrorMap(productImagesErrorMap)
              .build(), requestId);
    } catch (ValidationException e) {
      log.error("Validation error while updating product master data: {}", e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getErrorCode(), false,
          ProductMasterDataUpdateResponse.builder()
              .errorMessage(ErrorMessage.valueOf(e.getErrorMessage())).build(), requestId);
    } catch (Exception e) {
      log.error("Error while updating product master data: {}", e.getMessage(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_SIMPLE_MASTER_DATA, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product", description = "update product")
  public GdnRestSingleResponse<SimpleMasterProductUpdateResponse> updateSimpleMasterData(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleMasterProductUpdateRequest request) {
    try {
      LOG.info("Invoking updateSimpleMasterData : {}", request);
      GdnPreconditions.checkArgument(
          StringUtils.isNotBlank(request.getUpdatedBy()) && Objects.nonNull(request.getUpdatedDate()),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getProductCode()),
          ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = ConverterUtil.getSimpleMasterProductUpdateDTO(request);
      SimpleMasterProductUpdateResponseDTO responseDTO = productServiceWrapper.updateMasterProductData(storeId,
          simpleMasterProductUpdateDTO);
      SimpleMasterProductUpdateResponse response = ConverterUtil.getSimpleMasterProductUpdateResponse(responseDTO);
      return new GdnRestSingleResponse<>(response, requestId);
    }catch (ApplicationRuntimeException e){
      return new GdnRestSingleResponse<>(e.getMessage(), e.getErrorCodes().getCode(),
          false, null, requestId);
    }
    catch (Exception e){
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.CLEAR_PRODUCT_CACHE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product", description = "update product")
  public GdnBaseRestResponse clearProductCache(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productId,
      @RequestParam String productCode) throws Exception {
    LOG.info("Clearing cache for single product: {} {}", productId, productCode);
    service.evictAllProductDetailCacheByProductCode(storeId, productCode);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.CLEAR_PRODUCT_CACHE_BY_PRODUCT_CODES, method =
      RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes =
 MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "clear product cache by product codes", description = "clear product cache"
      + " by product codes")
  public GdnBaseRestResponse clearProductCacheByProductCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody List<String> productCodes)
      throws Exception {
    LOG.info("Clearing cache for products: {}", productCodes);
    try {
      service.evictProductCacheByProductCodes(storeId, productCodes);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while clearing product cache for product codes  : {}", productCodes, e);
      return new GdnBaseRestResponse(null, null, false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.CLEAR_PRODUCT_CACHE_SYNC, method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "clear product detail cache", description = "clear product detail cache")
  public GdnBaseRestResponse clearProductCacheSync(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productId,
      @RequestParam String productCode) {
    LOG.info("clearing cache for single product: with id = {}, productCode = {}", productId, productCode);
    service.evictAllProductDetailCacheByProductCode(storeId, productCode);
    return new GdnBaseRestResponse(StringUtils.EMPTY, StringUtils.EMPTY, Boolean.TRUE, requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_FOR_MERGE, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product for merge", description = "update product for merge")
  public GdnBaseRestResponse updateProductForMerge(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    LOG.info("Invoking updateProductForMerge : {}", request);
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy())
            || request.getUpdatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());

    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productAttributes", "productItems",
        "productCategories", "images", "longDescription");
    for(ProductItemRequest productItemReq : request.getProductItems()){
      LOG.info("MERGE: product item {} {} request came with upc code {} and sku code {}",
          productItemReq.getId(), productItemReq.getGeneratedItemName(),
          productItemReq.getUpcCode(), productItemReq.getSkuCode());
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(productItemReq, productItem, "images");
      productItem.setProductItemImages(ConverterUtil.convertRequestToProductItemImage(request, productItemReq, productItem));
      productItem.setProduct(product);
      LOG.info("MERGE: product item {} {} with upc code {} sku code {}", productItem.getId(),
          productItem.getGeneratedItemName(), productItem.getUpcCode(), productItem.getSkuCode());
      if(productItem.getUpcCode() != null) {
        LOG.info("MERGE: activating product item {} {} with upc {} and sku code {}",productItem.getId(),
            productItem.getGeneratedItemName(), productItem.getUpcCode(), productItem.getSkuCode());
        this.productItemService.activateProductItem(storeId, productItem);
      }
    }
    applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId, product.getId());
    return new GdnBaseRestResponse(null, null, true, requestId);
  }


  @RequestMapping(value = ProductApiPath.UPDATE_WITH_SPECIFICATION_DETAIL_GENERATED_BY_SYSTEM,
                  method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product with specification detail generated by system",
             description = "update product with specification detail generated by system")
  public GdnBaseRestResponse updateProductWithSpecificationDetailGeneratedBySystem(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductRequest request) throws Exception {
    GdnPreconditions.checkArgument(
        !(StringUtils.isEmpty(request.getUpdatedBy())
            || request.getUpdatedDate() == null),
        ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getId()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    Product product =
        this.convertRequestToProduct(storeId, false, request, new ArrayList<>(), false);
    Product savedProduct =
        service.getProductDetailsWithoutImagesByProductCodeAndMarkForDeleteFalse(storeId, request.getId());
    BeanUtils.copyProperties(product, savedProduct, "createdMerchant", "productAttributes",
        "productItems", "productCategories", "images", "version", "createdBy", "createdDate");
    product = this.service.sortAndGenerateProductItem(product);
    if (StringUtils.isEmpty(product.getStoreId())) {
      product.setStoreId(storeId);
    }
    this.service.updateProductWithSpecificationDetailGeneratedBySystem(storeId, savedProduct,
        product);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.UPLOAD_PRODUCT, method = RequestMethod.POST, produces = {
      "text/csv"}, consumes = {MediaType.MULTIPART_FORM_DATA_VALUE})
  @Operation(summary = "upload product", description = "upload product")
  public byte[] uploadProduct(HttpServletRequest request, HttpServletResponse response,
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestPart("productCsvFile") MultipartFile productCsvFile) throws Exception {
    List<String> productIds = new ArrayList<String>();
    ICsvBeanReader beanReader = null;
    try {
      beanReader =
          new CsvBeanReader(new InputStreamReader(productCsvFile.getInputStream(), "UTF-8"),
              CsvPreference.STANDARD_PREFERENCE);
      beanReader.getHeader(true);
      ProductCsv productCsv = null;
      ProductCsv currentProductCsv = null;
      Product currentProduct = null;
      String currentAttributeId = null;
      ProductAttribute currentProductAttribute = null;
      HashMap<String, String> productCategoryIds = null;
      while ((productCsv = beanReader.read(ProductCsv.class, ProductCsv.INPUT_HEADER,
          ProductCsv.INPUT_PROCESSORS)) != null) {
        storeId = productCsv.getStoreId();
        if (!productCsv.equals(currentProductCsv)) {
          if (currentProduct != null) {
            productIds.add(this.service.save(currentProduct));
          }
          productCategoryIds = new HashMap<String, String>();
          currentProductCsv = productCsv;
          currentProduct = new Product();
          BeanUtils.copyProperties(productCsv, currentProduct, "description", "longDescription");
          currentProduct.setDescription(productCsv.getDescription().getBytes());
        }

        if (productCategoryIds.get(productCsv.getCategoryId()) == null) {
          productCategoryIds.put(productCsv.getCategoryId(), productCsv.getCategoryId());
          Category category = this.categoryService.findByStoreIdAndId(productCsv.getStoreId(),
              productCsv.getCategoryId());
          currentProduct.getProductCategories()
              .add(new ProductCategory(currentProduct, category, productCsv.getStoreId()));
        }
        if (productCategoryIds.size() == 1) {
          if (!productCsv.getAttributeId().equals(currentAttributeId)) {
            Attribute attribute = this.attributeService.findById(productCsv.getStoreId(),
                productCsv.getAttributeId());
            checkNotNull(attribute, "not found attribute with id " + productCsv.getAttributeId());
            currentAttributeId = productCsv.getAttributeId();
            currentProductAttribute = new ProductAttribute(attribute, currentProduct,
                productCsv.getProductAttributeName(), true, productCsv.getSequence(),
                productCsv.getStoreId());
            currentProduct.getProductAttributes().add(currentProductAttribute);
          }
          if (productCsv.getDescriptiveAttributeValueType().equals("NONE")) {
            AllowedAttributeValue allowedAttributeValue = this.allowedAttributeValueService
                .findByStoreIdAndId(productCsv.getStoreId(), productCsv.getAllowedAttributeId());
            checkNotNull(allowedAttributeValue,
                "not found allowed attribute value with id " + productCsv.getAllowedAttributeId());
            currentProductAttribute.getProductAttributeValues()
                .add(new ProductAttributeValue(currentProductAttribute, allowedAttributeValue, null,
                    DescriptiveAttributeValueType.NONE));
          }
          else {
            currentProductAttribute.getProductAttributeValues()
                .add(new ProductAttributeValue(currentProductAttribute, null,
                    productCsv.getDescriptiveAttributeValue(), DescriptiveAttributeValueType
                        .valueOf(productCsv.getDescriptiveAttributeValueType())));
          }
        }
      }
      if (currentProduct != null) {
        String id = this.service.save(currentProduct);
        productIds.add(id);
      }
    }
    finally {
      if (beanReader != null) {
        beanReader.close();
      }
    }

    ICsvBeanWriter beanWriter = null;
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    PrintWriter writer = new PrintWriter(new BufferedOutputStream(out, 1024));
    try {
      beanWriter = new CsvBeanWriter(writer, CsvPreference.STANDARD_PREFERENCE);
      beanWriter.writeHeader(ProductCsv.ACTUAL_HEADER);

      for (String productId : productIds) {
        Product product = productServiceWrapper.getCompleteProductDetailByProductIdAndMarkForDeleteFalse(storeId, productId);
        ProductItemCsv productItemCsv;
        for (ProductItem productItem : product.getProductItems()) {
          productItemCsv = new ProductItemCsv();
          BeanUtils.copyProperties(productItem, productItemCsv);
          productItemCsv.setProductId(product.getId());
          beanWriter.write(productItemCsv, ProductCsv.OUTPUT_HEADER, ProductCsv.OUTPUT_PROCESSORS);
        }
      }
    }
    finally {
      if (beanWriter != null) {
        beanWriter.close();
      }
    }
    out.close();
    byte[] result = out.toByteArray();
    response.setHeader("Content-Disposition",
        "attachment; filename=\"" + ProductController.PRODUCT_EXPORT_CSV_FILENAME + "\"");
    return result;
  }

  @RequestMapping(value = ProductApiPath.UPLOAD_PRODUCT_ITEM, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.MULTIPART_FORM_DATA_VALUE})
  @Operation(summary = "upload product item", description = "upload product item")
  public GdnBaseRestResponse uploadProductItem(HttpServletRequest request,
      HttpServletResponse response, @RequestParam String storeId, @RequestParam String channelId,
      @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestPart("productItemCsvFile") MultipartFile productItemCsvFile) throws Exception {
    ICsvBeanReader beanReader = null;
    try {
      beanReader =
          new CsvBeanReader(new InputStreamReader(productItemCsvFile.getInputStream(), "UTF-8"),
              CsvPreference.STANDARD_PREFERENCE);
      beanReader.getHeader(true);
      ProductItemCsv productItemCsv = null;
      while ((productItemCsv = beanReader.read(ProductItemCsv.class, ProductItemCsv.INPUT_HEADER,
          ProductItemCsv.INPUT_PROCESSORS)) != null) {
        ProductItem savedProductItem = this.productItemService
            .findByStoreIdAndId(productItemCsv.getStoreId(), productItemCsv.getId());
        GdnPreconditions.checkArgument(savedProductItem != null,
            "Cannot find product with id " + productItemCsv.getId());

        savedProductItem.setUpcCode(productItemCsv.getUpcCode());
        savedProductItem.setSkuCode(productItemCsv.getSkuCode());
        /*
         * this line of code delete because the system now allow duplicate UPC_CODE
         * GdnPreconditions.checkArgument(productItemService.isUpcAvailable(storeId,
         * savedProductItem.getUpcCode(), savedProductItem.isActivated()),
         * PRODUCT_ITEM_WITH_SAME_UPC_EXIST + savedProductItem.getUpcCode());
         */
        this.productItemService.update(savedProductItem);
      }
    }
    finally {
      if (beanReader != null) {
        beanReader.close();
      }
    }
    return new GdnBaseRestResponse(true);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_VIEWABLE, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product viewable", description = "update product viewable")
  public GdnBaseRestResponse updateProductViewable(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestParam boolean viewable) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode), ProductController.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    productServiceWrapper.updateProductViewable(storeId, productCode, viewable);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_REVIEW_PENDING, method = RequestMethod.PUT,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product viewable", description = "update product viewable")
  public GdnBaseRestResponse updateProductReviewPending(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode, @RequestParam boolean reviewPending)
      throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode), ProductController.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    this.productServiceWrapper.updateProductReviewPendingAndEvictCache(storeId, productCode,
      reviewPending);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_FLAGS_FOR_NEED_REVISION, method =
      RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product viewable", description = "update product viewable")
  public GdnBaseRestResponse updateFlagsOnNeedCorrection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode,
      @RequestBody NeedRevisionConfigRequest request) throws Exception {
    this.productServiceWrapper.updateFlagsOnNeedCorrectionAndEvictCache(storeId, productCode,
      request);
    return new GdnBaseRestResponse(requestId);
  }

  protected ActivateImageResponse convertRequestToActivateImageResponse(ActivateImage request) {
    ActivateImageResponse activateImageResponse = new ActivateImageResponse();

    BeanUtils.copyProperties(request, activateImageResponse);
    return activateImageResponse;
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_IMAGE_NAME, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product image name", description = "update product image name")
  public GdnRestSingleResponse<ActivateImageResponse> updateImageName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ActivateImageRequest request)
      throws Exception {
    LOG.info("Invoking updateImageName : {}", request);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getHashCode()),
        ErrorMessage.IMAGE_HASHCODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getFilenames()),
        ErrorMessage.IMAGE_LOCATION_PATH_MUST_NOT_BE_BLANK.getMessage());

    this.imageService.activateAndUpdateImageName(storeId, request.getProductCode(), request.getFilenames(),
            request.getHashCode());
    ActivateImage activateImage = this.imageService.isProductImagesActivated(storeId, request.getProductCode());

    ActivateImageResponse response = this.convertRequestToActivateImageResponse(activateImage);
    return new GdnRestSingleResponse<ActivateImageResponse>("", "", true, response, requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_IMAGES_NAME, method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update product image name", description = "update product image name")
  public GdnRestSingleResponse<ActivateImageResponse> updateImagesName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false) boolean skipReview,
      @RequestBody ProductActivateImageRequest request) throws Exception {
    LOG.info("Invoking updateImagesName: {}", request);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getImageRequests()),
        ErrorMessage.IMAGE_REQUEST_MUST_NOT_BE_BLANK.getMessage());
    ProductActivateImageDTO dto = new ProductActivateImageDTO();
    BeanUtils.copyProperties(request, dto, "imageRequests");
    Set<ActivateImageDTO> activateImageDTOs = new HashSet<>();
    for (ActivateImageRequest imageRequest : request.getImageRequests()) {
      ActivateImageDTO activateImageDTO = new ActivateImageDTO();
      BeanUtils.copyProperties(imageRequest, activateImageDTO);
      activateImageDTOs.add(activateImageDTO);
    }
    dto.setImageRequests(activateImageDTOs);
    try {
      this.imageServiceWrapper.activateAndUpdateImagesName(storeId, dto, skipReview);
      ActivateImage activateImage = this.imageService.isProductImagesActivated(storeId, request.getProductCode());
      ActivateImageResponse response = this.convertRequestToActivateImageResponse(activateImage);
      return new GdnRestSingleResponse<ActivateImageResponse>("", "", true, response, requestId);
    } catch (Exception e) {
      log.error("Exception while updating image name for productCode : {} ", request.getProductCode(), e);
      return new GdnRestSingleResponse<ActivateImageResponse>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.IS_PRODUCT_IMAGES_ACTIVATED, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "check product images activation status", description = "return true if "
      + "all product images is activated")
  public GdnRestSingleResponse<ActivateImageResponse> isProductImagesActivated(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productCode) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    ActivateImage result =
        this.imageService.isProductImagesActivated(storeId, productCode);
    ActivateImageResponse response = this.convertRequestToActivateImageResponse(result);
    return new GdnRestSingleResponse<ActivateImageResponse>("", "", true, response, requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_IMAGE_TO_ACTIVE, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product image to active", description = "update product image to "
      + "active")
  public GdnBaseRestResponse activateAndUpdateImageName(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ActivateImageRequest request)
      throws Exception {
    LOG.info("Invoking activateAndUpdateImageName: {}", request);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ErrorMessage.PRODUCT_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getHashCode()),
        ErrorMessage.IMAGE_HASHCODE_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getFilenames()),
        ErrorMessage.IMAGE_LOCATION_PATH_MUST_NOT_BE_BLANK.getMessage());
    this.imageService.activateAndUpdateImageName(storeId, request.getProductCode(),
        request.getFilenames(), request.getHashCode());
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_ACTIVATED, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product activated", description = "update product activated")
  public GdnBaseRestResponse updateProductActivated(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestParam boolean activated) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(productCode), ProductController.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    this.productServiceWrapper.updateProductActivated(storeId, productCode, activated);
    return new GdnBaseRestResponse(requestId);
  }


  @RequestMapping(value = ProductApiPath.FILTER_PRODUCT_CODE_EXACT_MATCH, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by product code exact match", description = "get product by "
  + "store id and product code and pageable")
  public GdnRestListResponse<ProductResponse> getProductByProductCodeExactMatch(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String productCode)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    LOG.info("Calling API to get product from product-code , requestId: {}, product_code: {}",
        requestId, productCode);
    return this.populateListResponse(requestId, pageable,
        this.service.findByProductCodeExactMatch(storeId, productCode, pageable));
  }

  @RequestMapping(value = ProductApiPath.VIEWABLE_COUNT, method = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product by viewable", description = "get product by store id and "
      + "viewable and pageable")
  public GdnRestSingleResponse<SingleObjectResponse<Long>> getProductCountByViewable(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam boolean viewable) throws Exception {
    LOG.info("Calling API to get product counts from viewable criteria for request {}, viewable " +
        "{}", requestId, viewable);
    Long productCount = this.service.getProductCountForViewable(storeId, viewable);
    return new GdnRestSingleResponse<SingleObjectResponse<Long>>(StringUtils.EMPTY, StringUtils
        .EMPTY, true, new SingleObjectResponse(productCount), requestId);
  }

  @RequestMapping(value = ProductApiPath.FILTER_UPC_CODE_EXACT_MATCH, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by upc code", description = "get product item by store "
      + "id and upc code and pageable")
  public GdnRestListResponse<ProductItemResponse> getProductItemByUpcCodeExactMatch(
      @RequestParam String storeId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size, @RequestParam String upcCode)
      throws Exception {
    LOG.info("Calling API to get product by upc code for upc code {}, page {}, size {}",
        upcCode, page, size);
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItemPage =
        this.productItemService.findByUpcCodeExactMatch(storeId, upcCode, pageable);
    List<ProductItemResponse> productItemResponseList=convertToProductItemResponse(productItemPage);
    return new GdnRestListResponse<>(null, null, true, productItemResponseList,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(),
            productItemPage.getTotalElements()),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.ITEM_FILTER_PRODUCT_CODES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by list of product codes", description = "get Product "
      + "Item by list of product codes")
  public GdnRestListResponse<ProductItemDetailResponse> getProductItemByListOfProductCode(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String clientId,
      @RequestParam String channelId, @RequestParam(required = false) String username,
      @RequestBody ProductCodesRequest request,
      @RequestParam(defaultValue = "true") boolean isOnlyExternal,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages,
      @RequestParam(required = false, defaultValue = "true") boolean active) throws Exception {
    try {
      List<ProductItem> productItemList =
          this.productItemService.findByListOfProductCode(request.getProductCodes(), isOnlyExternal, active);
      List<ProductItemDetailResponse> productItemResponseList =
          ConverterUtil.convertToProductItemDetailResponse(productItemList, originalImages);
      return new GdnRestListResponse<>(null, null, true, productItemResponseList,
          new PageMetaData(productItemList.size(), 0,
              productItemList.size()),
          requestId);
    } catch(Exception e) {
      log.error("Error when trying to get item details by productCodes request : {} ", request, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ITEM_FILTER_UPC_CODE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get Product Item by UPCCode and category IDs", description = "get Product"
      + " Item by UPCCode and category IDs")
  public GdnRestListResponse<ProductItemDetailResponse> getProductItemsByUPCCodeAndCategoryIds(
      @RequestParam String storeId, @RequestParam String requestId, @RequestParam String clientId,
      @RequestParam String channelId, @RequestParam(required = false) String username,
      @RequestBody UPCCodeSearchRequest request,
      @RequestParam(defaultValue = "true") boolean isOnlyExternal,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam(required = false, defaultValue = "false") boolean originalImages)
      throws Exception {
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getUpcCode()),
          ErrorMessage.UPC_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getCategoryCodes()),
          ErrorMessage.CATEGORY_IDS_MUST_NOT_BE_EMPTY.getMessage());
      Page<ProductItem> productItemList =
          productItemServiceWrapper.findByUPCCodeAndCategoryIds(storeId, request, isOnlyExternal, page, size);
      List<ProductItemDetailResponse> productItemResponseList =
          ConverterUtil.convertToProductItemDetailResponse(productItemList.getContent(), originalImages);
      return new GdnRestListResponse<>(null, null, true, productItemResponseList,
          new PageMetaData(size, page, productItemList.getTotalElements()), requestId);
    } catch (Exception e) {
      LOG.error("Error while fetching items by upc code: {}", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  private List<ProductItemResponse> convertToProductItemResponse(Page<ProductItem> productItemPage) {
    List<ProductItemResponse> productItemResponses = new ArrayList<>();
    for (ProductItem product : productItemPage.getContent()) {
      ProductItemResponse response = new ProductItemResponse();
      BeanUtils.copyProperties(product, response);
      productItemResponses.add(response);
    }
    return productItemResponses;
  }


  @RequestMapping(value = ProductApiPath.FILTER_ACTIVE_PRODUCT_CATEGORY, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get all active products code by category", description = "used by "
      + "ext-catalog")
  public GdnRestListResponse<SingleObjectResponse<List<String>>> getActiveProductIdsFromCategory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String categoryId,
      @RequestParam(defaultValue = "0", required = false) Integer page,
      @RequestParam(defaultValue = "10", required = false) Integer size,
      @RequestParam(required = false) @DateTimeFormat(pattern = "dd-MM-yyyy") Date updatedAfter) {
    try {
      SolrProductModel requestModel = new SolrProductModel();
      requestModel.setProductCategoryId(categoryId);
      requestModel.setUpdatedDate(updatedAfter);

      Page<SolrProductResponse> codeResponses =
        this.solrProductFilterService.getActiveProductIds(requestModel, page, size);

      if (Objects.isNull(codeResponses) || codeResponses.getTotalElements() == 0) {
        return new GdnRestListResponse<>(NO_DATA_ERROR, null, true, null);
      }

      List<String> solrResponses = codeResponses.getContent().stream()
        .map(SolrProductResponse::getProductCode)
        .collect(Collectors.toList());

      return new GdnRestListResponse<>(Arrays.asList(new SingleObjectResponse<>(solrResponses)),
        new PageMetaData(size, page, codeResponses.getTotalElements()), requestId);
    } catch (Exception e) {
      LOG.error("Exception: failed to fetch products for category ", e);
    }
    return new GdnRestListResponse<>(null, null, false, null);
  }

  private void fetchAndSetItemDetails(ProductRequest productRequest, Product product, boolean isComplete) {
    List<String> attributes = productRequest.getProductItems().stream()
        .flatMap(productItemRequest -> productItemRequest.getProductItemAttributeValues().stream())
        .map(productItemAttributeValueRequest -> productItemAttributeValueRequest.getAttribute().getId()).distinct()
        .collect(Collectors.toList());
    Map<String, Attribute> attributeMap =
        attributeService.findByAttributeIds(productRequest.getStoreId(), attributes).stream()
            .collect(Collectors.toMap(Attribute::getAttributeCode, Function.identity()));
    if (isComplete) {
      ConverterUtil.setProductItemDetails(productRequest, product, attributeMap);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_CONTENT, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product content", description = "update product content")
  public GdnBaseRestResponse updateProductContent(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "false") boolean ignoreSalesCategoryPublish,
      @RequestBody ProductRequest request) throws Exception {
    LOG.info("Invoking updateProductContent : {}", request);
    try {
      GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
          ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      Product product =
          this.convertRequestToProduct(storeId, false, request, new ArrayList<>(), false);
      fetchAndSetItemDetails(request, product, true);
      this.productServiceWrapper.updateProductContent(product, request.isPublishProductEvent(),
          ignoreSalesCategoryPublish, request.isUpdateFromVendor());
      return new GdnBaseRestResponse(requestId);
    } catch (Exception e) {
      log.error("Error when trying to update product content for productCode : {} ,error - ", request.getProductCode(),
          e);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_IMAGE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product image", description = "update product image")
  public GdnBaseRestResponse updateProductImage(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductRequest request)
      throws Exception {
    LOG.info("Invoking updateProductImage : {}", request);
    try {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(request.getProductCode()),
        ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      Product product =
          this.convertRequestToProduct(storeId, false, request, new ArrayList<>(), false);
    fetchAndSetItemDetails(request, product, true);
    productServiceWrapper.updateProductImage(product);
    return new GdnBaseRestResponse(requestId);
    } catch (Exception e) {
      log.error("Error when trying to update product image for productCode : {} , error - ", request.getProductCode(),
          e);
      return new GdnBaseRestResponse(false);
    }
  }

  @RequestMapping(value = ProductApiPath.COUNT_PRODUCT_BY_BRAND_NAME, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get total of product that is using specific brand", description = "get "
      + "total of product that is using specific brand")
  public GdnRestSingleResponse<SingleObjectResponse<Long>> getProductCountByBrandName(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("brandName") String brandName) throws Exception {
    LOG.info("Calling API to get product counts by brand name {}", brandName);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(brandName),
        BrandControllerErrorMessage.BRAND_NAME_MUST_NOT_BE_BLANK);
    Long productCount = this.service.getProductCountByBrandName(storeId, brandName);
    return new GdnRestSingleResponse<SingleObjectResponse<Long>>(StringUtils.EMPTY, StringUtils
        .EMPTY, true, new SingleObjectResponse(productCount), requestId);
  }

  private void sortProductItemImages(Product product) {
    if (null != product && ( !CollectionUtils.isEmpty(product.getProductItems()))) {
      List<ProductItem> productItemList = product.getProductItems();
      for (ProductItem productItem : productItemList)
        ConverterUtil.sortProductItemImagesBySequenceId(productItem);
    }
  }

  @RequestMapping(value = ProductApiPath.REPLACE_PRODUCT_IMAGES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "replace product images based on product code", description = "replace "
      + "product images based on product code")
  public GdnRestSingleResponse<GeneratedProductImagesPathResponse> replaceProductImages(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ReplaceProductImagesRequest productImageRequest) throws Exception {
    LOG.info("Invoked replaceProductImages : {}", productImageRequest);
    GeneratedProductImagesPathDto generatedProductImageDto =
        this.service.replaceProductImages(storeId, this.convertToReplaceProductImagesDto(productImageRequest));
    return new GdnRestSingleResponse<GeneratedProductImagesPathResponse>(
        this.convertToGeneratedProductImagesPathResponse(generatedProductImageDto), requestId);
  }

  private GeneratedProductImagesPathResponse convertToGeneratedProductImagesPathResponse(GeneratedProductImagesPathDto source){
    GeneratedProductImagesPathResponse result = new GeneratedProductImagesPathResponse();
    result.setProductCode(source.getProductCode());
    result.setProductItems(new ArrayList<GeneratedProductItemImagesPathResponse>());
    result.setImages(new ArrayList<ImagePathResponse>());
    if(! CollectionUtils.isEmpty(source.getImages())){
      for(ImagePathDTO productImage : source.getImages()){
        result.getImages().add(new ImagePathResponse(productImage.getImagePath(), productImage.isMainImage()));
      }
    }
    if(! CollectionUtils.isEmpty(source.getProductItems())){
      for(GeneratedProductItemImagesPathDto productItem : source.getProductItems()){
        GeneratedProductItemImagesPathResponse productItemResponse = new GeneratedProductItemImagesPathResponse();
        productItemResponse.setSkuCode(productItem.getSkuCode());
        productItemResponse.setImages(new ArrayList<ImagePathResponse>());
        if(! CollectionUtils.isEmpty(productItem.getImages())){
          for(ImagePathDTO itemImage : productItem.getImages()){
            productItemResponse.getImages().add(new ImagePathResponse(itemImage.getImagePath(), itemImage.isMainImage()));
          }
        }
        result.getProductItems().add(productItemResponse);
      }
    }

    return result;
  }

  private ReplaceProductImagesDTO convertToReplaceProductImagesDto(ReplaceProductImagesRequest source){
    ReplaceProductImagesDTO productImageReqDto = new ReplaceProductImagesDTO();
    productImageReqDto.setProductCode(source.getProductCode());
    productImageReqDto.setGeneratedImageCount(source.getGeneratedImageCount());
    productImageReqDto.setProductItem(new ArrayList<ReplaceProductItemImagesDTO>());
    if(! CollectionUtils.isEmpty(source.getProductItem())){
      for(ReplaceProductItemImagesRequest sourceItem : source.getProductItem()) {
        productImageReqDto.getProductItem().add(new ReplaceProductItemImagesDTO(sourceItem.getSkuCode(), sourceItem.getGeneratedImageCount()));
      }
    }

    return productImageReqDto;
  }

  @RequestMapping(value = ProductApiPath.VALIDATE_PROMO_SKU, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "validate Product PromoSku", description = "validate Product PromoSku")
  public GdnRestSingleResponse<SingleObjectResponse<Boolean>> validateProductPromoSku(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam String productId, @RequestParam(defaultValue = "false") boolean isPromoSku)
      throws Exception {
    LOG.info(
        "Calling API to validate product promo sku for request {}, productId {}, isPromoSku {}",
        requestId, isPromoSku, productId);
    Boolean isValid = this.service.validateProductPromoSku(productId, storeId, isPromoSku);
    return new GdnRestSingleResponse<SingleObjectResponse<Boolean>>(StringUtils.EMPTY,
        StringUtils.EMPTY, true, new SingleObjectResponse(isValid), requestId);
  }

  @RequestMapping(value = ProductApiPath.CREATE_PRODUCT, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "create product", description = "create product")
  public GdnRestSingleResponse<MapResponse<String, String>> createProduct(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "true") boolean computeCommonImage,
      @RequestBody ProductRequest request) throws Exception {
    LOG.info("Invoking createProduct : {}", request);
    try {
      GdnPreconditions.checkArgument(!(StringUtils.isEmpty(request.getCreatedBy()) || request.getCreatedDate() == null),
          ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_SAVE_MESSAGE.getMessage());
      GdnPreconditions.checkArgument(request.getProductCategories() != null,
          ErrorMessage.ENTITY_REQUIRED_PRODUCT_CATEGORY_FOR_SAVE_MESSAGE.getMessage());
      //create map of unique item request with it's images and upc code
      addVariantCreationFlagToRequest(storeId, request);
      Map<Map<String, String>, Map<String, Object>> dataMap =
          ConverterUtil.generateItemDataHashMap(request, sizeChartValueTypeDelimiter);
      Product product =
          this.convertRequestToProduct(storeId, false, request, new ArrayList<>(), true);
      product = this.service.sortAndGenerateProductItem(product);
      if (StringUtils.isEmpty(product.getStoreId())) {
        product.setStoreId(storeId);
      }
      Map<String, Attribute> itemAttributeMap = this.getItemAttrMap(storeId, request);
      ConverterUtil.setRequiredData(storeId, product, dataMap, itemAttributeMap, imageSourceDirectory,
          fullImageSourceDirectory, request, skipMissingVariantsWhileCreation, ranchIntegrationEnabled,
          distributionSellerList);
      if (StringUtils.isNotEmpty(request.getProductCode())) {
        product = this.service.saveProduct(product, request.getCommonImages(), computeCommonImage);
        this.service.publishVatUpdateEvent(product.getProductItems());
        return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, Boolean.TRUE,
            new MapResponse<>(ConverterUtil.createAttributeAndItemIdMap(product, valueTypeAdditionForDefiningAttributes, sizeChartValueTypeDelimiter)), requestId);
      }
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT,
          ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    } catch (ApplicationRuntimeException e) {
      LOG.error("failed to save Product for productCode : {}, requestId : {}, productCreationRequest : {}",
          request.getProductCode(), requestId, request, e);
      trackerService.trackProductCreationFailure(requestId, request, e.getMessage());
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    }
    catch (Exception e) {
      if (Optional.ofNullable(e).map(Exception::getMessage).orElse(StringUtils.EMPTY).contains(duplicateProductCodeConstraint)) {
        trackerService.trackProductCreationFailure(requestId, request, e.getMessage());
        return new GdnRestSingleResponse<>(ErrorMessage.DUPLICATE_PRODUCT_CODE.getMessage(),
            ErrorCategory.VALIDATION.getCode(), false, null, requestId);
      }
      LOG.error("failed to save Product for productCode : {}, requestId : {}, productCreationRequest : {}",
          request.getProductCode(), requestId, request, e);
      trackerService.trackProductCreationFailure(requestId, request, e.getMessage());
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.FILTER_PRODUCT_IMAGES, method = RequestMethod.POST,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "filter product's images by productIds", description = "filter product's "
  + "images by productIds")
  public GdnRestListResponse<ProductImageResponse> filterProductImagesByProductIds(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ListHolderRequest<String> request,
      @RequestParam(required = false) boolean mainImage) throws Exception {
    List<ProductImageResponse> result = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(request.getLists())) {
      result = ProductControllerUtil
          .convertProductImagesResponse(this.imageService.filterProductImagesByProductIds(request.getLists()),
              mainImage);
    }
    return new GdnRestListResponse<>(result, null, requestId);
  }

  private void addVariantCreationFlagToRequest(String storeId, ProductRequest productRequest) {
    for (ProductAttributeRequest productAttributeRequest : productRequest.getProductAttributes()) {
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(
          attributeService.getAttributeByStoreIdAndAttributeCodeAndMarkForDeleteFalse(
              storeId, productAttributeRequest.getAttribute().getAttributeCode()), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
    }
  }

  @RequestMapping(value = ProductApiPath.FILTER_PRODUCT_IMAGES_BY_PRODUCT_CODES, method =
      RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes =
      MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "filter product's images by productCodes", description = "filter product's"
      + " images by productCodes")
  public GdnRestListResponse<ProductImageResponse> filterProductImagesByProductIdsOrProductCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestBody ListHolderRequest<String> request) throws Exception {
    List<ProductImageResponse> result =
        ProductControllerUtil.convertImageDtoToResponse(this.imageService
            .filterProductImagesByProductCodes(storeId, request.getLists()));
    return new GdnRestListResponse<>(result, null, requestId);
  }

  private Map<String, Attribute> getItemAttrMap(String storeId, ProductRequest request) {
    Set<String> attributeIds = new HashSet<>();
    for(ProductItemRequest productItemRequest : request.getProductItems()){
      attributeIds.addAll(Optional.ofNullable(productItemRequest.getProductItemAttributeValues())
          .orElseGet(Collections::emptyList).stream()
          .map(itemAttrValue -> itemAttrValue.getAttribute().getId()).collect(Collectors.toSet()));
    }
    return Optional.ofNullable(attributeService
        .findByAttributeIds(storeId, attributeIds.stream().collect(Collectors.toList())))
      .orElseGet(Collections::emptyList).stream().collect(Collectors.toMap(attribute -> attribute.getId(),
        attribute -> attribute));
  }

  @Operation(summary = "Send product publish event using updated by" ,
      description = "Send product publish event using updated by")
  @RequestMapping(value = ProductApiPath.PUBLISH_PRODUCT_BY_UPDATED_BY, method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnBaseRestResponse sendProductPublishEventUsingUpdatedBy(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String updatedBy) {
    LOG.info("Publishing product publish event for updated by {}", updatedBy);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(updatedBy), "updatedBy should not be empty");
    service.publishProductByStoreIdAndUpdatedBy(storeId, updatedBy);
    return new GdnBaseRestResponse(null, null, true, requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_AND_ITEM_IMAGE_DETAILS_BY_PRODUCT_CODE,
                  method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update product and item images by product code", description =
      "update product and item images by " + "product code")
  public GdnBaseRestResponse updateProductAndItemImagesByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(required = false, defaultValue = "1") boolean setDgLevel,
      @RequestBody ProductAndItemImageRequest request) {
    LOG.info("Invoking updateProductAndItemImagesByProductCode : {}", request);
    return new GdnBaseRestResponse(null, null,
        productServiceWrapper.updateProductAndItemImagesByProductCode(request, storeId, setDgLevel), requestId);
  }

  @RequestMapping(value = ProductApiPath.DELETE_ORIGINAL_IMAGES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Delete original images by product code and make review pending as false",
   description =
      "Delete original images by " + "product code and update review pending as false")
  public GdnBaseRestResponse deleteOriginalImages(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode) {
    LOG.info("Invoking deleteOriginalImages productCode : {}", productCode);
    return new GdnBaseRestResponse(null, null,
        productServiceWrapper.deleteOriginalImagesByProductCode(storeId, productCode), requestId);
  }

  @RequestMapping(value = ProductApiPath.ITEM_LISTING_BY_PRODUCT_CODE, method = RequestMethod.GET
      , produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetches list of items mapped to corresponding L1. B2B API", description =
      "Fetches list of items mapped to corresponding L1. B2B API")
  public GdnRestListResponse<SimpleItemDetailResponse> getItemCodesList(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode, @RequestParam(defaultValue = "0") int page,
      @RequestParam(defaultValue = "10") int size) {
    LOG.info("Fetching items list for product code : {} ", productCode);
    try {
      Page<SimpleItemDetailResponse> simpleItemDetailResponsePage =
          this.productServiceWrapper.getItemListByProductCode(storeId, productCode, page, size);
      return new GdnRestListResponse<>(null, null, true, simpleItemDetailResponsePage.getContent(),
          new PageMetaData(size, page, simpleItemDetailResponsePage.getTotalElements()), requestId);
    } catch (ApplicationRuntimeException e) {
      LOG.error("Error while fetching items for given product code {}, Product not found with given product code",
          productCode, e);
      return new GdnRestListResponse<>(ErrorCategory.DATA_NOT_FOUND.getMessage(),
          ErrorCategory.DATA_NOT_FOUND.getCode(), false, null, null, requestId);
    } catch (Exception e) {
      LOG.error("Error while fetching items for product code : {} ", productCode, e);
      return new GdnRestListResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, null, requestId);
    }
  }


  @RequestMapping(value = ProductApiPath.GET_CATEGORY_HIERARCHY_BY_UPCCODE_PRODUCT_COUNT, method
      = RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get category hierarchy with product count by UPC Code", description =
 "get category hierarchy with product count by UPC Code")
  public GdnRestListResponse<CategoryHierarchyResponse> getCategoryHierarchyByUPCCodeWithProductCount(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String upcCode,
      @RequestParam(defaultValue = "true") boolean isOnlyExternal) throws Exception {
    try {
      List<CategoryHierarchyResponse> response =
          this.service.getCategoryHierarchyByUPCCode(storeId, upcCode, isOnlyExternal);
      return new GdnRestListResponse<>(null, null, true, response,
          new PageMetaData(response.size(), 0, response.size()), requestId);
    } catch (Exception e) {
      LOG.error("error getting category hierarchy with product count by product name or product code.", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @Operation(summary = "Update product category by productCode", description = "Update product "
      + "category by productCode")
  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_CATEGORY, method = RequestMethod.PUT,
   produces = MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<CategorySummaryResponse> updateProductCategory(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode, @RequestParam String categoryCode,
      @RequestParam(defaultValue = "true") boolean updateSalesCategory,
      @RequestParam(defaultValue = "true") boolean b2bSeller) {
    LOG.info("Updating product category for product {} and category {} ", productCode, categoryCode);
    CategorySummaryResponse categorySummaryResponse = null;
    try {
      categorySummaryResponse =
          productServiceWrapper.updateProductCategoryByStoreIdAndProductCode(storeId, productCode, categoryCode,
              updateSalesCategory,b2bSeller);
    } catch (ApplicationRuntimeException e) {
      LOG.info("Error when updating product category for product {} and category {} ", productCode, categoryCode, e);
      return new GdnRestSingleResponse<>(e.getErrorMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      LOG.info("Error when updating product category for product {} and category {} ", productCode, categoryCode, e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
    return new GdnRestSingleResponse<>(null, null, true, categorySummaryResponse, requestId);
  }

  @RequestMapping(value = ProductApiPath.ITEM_DETAIL_BY_ITEM_CODE, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetches item detail along with complete product detail. B2B Api",
   description = "Fetches item detail along with complete product detail. B2B Api")
  public GdnRestSingleResponse<ProductItemCompleteResponse> getProductItemDetailByItemCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("itemCode") String itemCode) {
    try {
      ProductItemCompleteResponse response = this.productServiceWrapper.getItemResponseByItemCode(storeId, itemCode);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (Exception e) {
      LOG.error("Error while fetching details of item by item code : {} ", itemCode, e);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DETAIL_BY_PRODUCT_CODE_WITHOUT_ITEM, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "To retrieve product details without items", description = "To retrieve "
      + "product details without items. B2B Api")
  public GdnRestSingleResponse<ProductDetailResponse> getProductDetailsWithoutItems(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode) {
    LOG.info("Fetching product details without items for product code : {} ", productCode);
    try{
      Product product =
          productServiceWrapper.getProductDetailsWithoutItemsByProductCodeAndMarkForDeleteFalse(storeId, productCode);
      ProductDetailResponse productDetailResponse =
          ConverterUtil.convertProductToProductDetailResponse(product, false, false, false);
      if (CollectionUtils.isNotEmpty(product.getProductCategories())) {
        List<Category> categoryList = this.categoryService.findCategoryHierarchyByCategoryCode(storeId,
            product.getProductCategories().get(0).getCategory().getCategoryCode());
        productDetailResponse =
            ConverterUtil.setCategoryNamesProductDetailResponse(categoryList, productDetailResponse);
      }
      return new GdnRestSingleResponse<>(null, null, true, productDetailResponse, requestId);
    } catch (Exception e) {
      LOG.error("Failed while fetching product details for productCode : {}", productCode);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.DELETE_IMAGES_FOR_DELETED_PRODUCT, method =
      RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete images for deleted products", description = "delete images for "
      + "deleted products")
  public GdnBaseRestResponse deleteImagesForDeletedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam int days, @RequestParam int daySpan,
      @RequestParam int batchSize) throws Exception {
    GdnPreconditions
        .checkArgument(days > 0 && batchSize > 0 && daySpan >= 0, ErrorMessage.NOT_VALID_PARAMETERS.getMessage());
    LOG.info("Deleting images for deleted products older than {} days, daySpan : {}, batchSize : {} ", days, daySpan,
        batchSize);
    this.service.deleteImagesForDeletedProduct(storeId, days, daySpan, batchSize);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.DELETE_IMAGES_FOR_UPDATED_PRODUCT, method =
      RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "delete images for updated products", description = "delete images for "
      + "updated products")
  public GdnBaseRestResponse deleteImagesForUpdatedProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam int days, @RequestParam int batchSize)
      throws Exception {
    GdnPreconditions.checkArgument(days > 0 && batchSize > 0, ErrorMessage.NOT_VALID_PARAMETERS.getMessage());
    LOG.info("Deleting images for updated products older than {} days with batchSize", days, batchSize);
    this.service.deleteImagesForUpdatedProduct(days, batchSize);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_SALES_CATEGORY_BY_PRODUCT_CODE, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch old and new sales category mappings for product")
  public GdnRestSingleResponse<ProductSalesCategoryMappingResponse> getSalesCategoryForProduct(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @PathVariable("productCode") String productCode,
      @RequestParam(defaultValue = "false") boolean ignoreHalalCategories) {
    log.info("Fetching sales category mappings for product code : {}", productCode);
    try {
      ProductSalesCategoryMappingResponse productSalesCategoryMappingResponse =
          this.service.getProductSalesCategoryMapping(storeId, productCode, ignoreHalalCategories);
      return new GdnRestSingleResponse<>(null, null, true, productSalesCategoryMappingResponse, requestId);
    } catch (Exception e) {
      log.error("Error while fetching sales category mappings for : {}", productCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_SCORE, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Scheduler to update product score", description = "update product score")
  public GdnBaseRestResponse updateProductScore(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username,
      @RequestParam(defaultValue = "0") Integer page,
      @RequestParam(defaultValue = "10") Integer size,
      @RequestParam boolean productScoreUpdateSwitch, @RequestParam int productScoreBatchSize)
      throws Exception {
    Pageable pageable = PageRequest.of(page, size);
    LOG.info("Updating product score for {} products with batch size : {}", productScoreBatchSize, size);
    productServiceWrapper.updateProductScore(storeId, pageable, productScoreUpdateSwitch, productScoreBatchSize);
    return new GdnBaseRestResponse(requestId);
  }

  @RequestMapping(value = ProductApiPath.MIGRATE_PRODUCT, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "copy product", description = "copy product")
  public GdnRestSimpleResponse<String> migrateProduct(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestParam(required = false) String oldProductCode,
      @RequestParam String newProductCode, @RequestParam String createdMerchant,
      @RequestBody(required = false) ProductRequest request) throws Exception {
    LOG.info(
        "Invoking copy product old product code : {}, new product code : {}, created merchant : "
            + "{} , product request : {}",
        oldProductCode, newProductCode, createdMerchant, request);
    try {
      Product product = null;
      if (Objects.nonNull(request)) {
        product = this.convertRequestToProduct(request, newProductCode);
      }
      String productId = this.service.copyProduct(oldProductCode, newProductCode, storeId, product, createdMerchant);
      return new GdnRestSimpleResponse(null, null, true, requestId, productId);
    } catch (Exception e) {
      log.error("Error while copy product old product code : {}, new product code : {} ", oldProductCode,
          newProductCode, e);
      return new GdnRestSimpleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, requestId,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ITEM_FILTER_BY_SKU_CODES, method = RequestMethod.POST,
                  consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product item by skuCodes", description = "get product item by skuCodes")
  public GdnRestListResponse<ProductItemResponse> getProductItemBySkuCodes(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SkuCodesRequest request) throws Exception {
    LOG.info("Get product item by skuCodes : {} ", request);
    List<ProductItemResponse> productItemResponses = new ArrayList<>();
    List<ProductItem> productItems =
        this.productItemService.findBySkuCodes(storeId, request.getSkuCodes(), request.isFetchArchived());
    ConverterUtil.convertToProductItemResponse(productItemResponses, productItems);
    return new GdnRestListResponse<>(null, null, true, productItemResponses, null, requestId);
  }

  @RequestMapping(value = ProductApiPath.ITEM_UPDATE_UPCCODE, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Update upc code for product item", description = "Update upc code for "
      + "product item")
  public GdnBaseRestResponse editItemUpcCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String productCode,
      @RequestBody List<ProductItemUpcCodeUpdateRequest> request) throws Exception {
    LOG.info("Invoking Update upc code for product item for : {}", request);
    try {
      GdnPreconditions
          .checkArgument(StringUtils.isNotEmpty(productCode), ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      this.productItemServiceWrapper.updateProductItemUpcCodeAndEvictCache(storeId, request,
        productCode);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error while Update upc code for product item request : {},  product code : {} ", request, productCode,
          e);
      return new GdnRestSimpleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, requestId,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_ITEM_IMAGES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update productItem images by product code", description =
      "update productItem images by " + "product code")
  public GdnRestListResponse<LocationPathAndCommonImage> updateProductItemImagesByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductItemImageUpdateRequest request) {
    LOG.info("Invoking updateProductItemImagesByProductCode : {}", request);
    return productServiceWrapper.updateProductItemImagesByProductCode(request, storeId, requestId);
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_NAME_BY_UPC_CODE_PRODUCT_CODE, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch item name by upc code and product code for product")
  public GdnRestListResponse<SingleObjectResponse<List<String>>> getItemNameByUpcCodeAndProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username, @RequestParam String upcCode,
      @RequestParam String productCode, @RequestParam String skuCode) {
    log.info("Fetching item name by upc code : {}  and product code : {} and skuCode : {} for product", upcCode,
        productCode, skuCode);
    try {
      List<String> itemNameList =
          this.productItemService.getItemNameByUPCCodeAndProductCode(upcCode, productCode, skuCode);
      return new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(itemNameList)), null,
          requestId);
    } catch (Exception e) {
      LOG.error("Exception: item name by upc code and product code for product ", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_NAME_BY_PRODUCT_ITEM_ID, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Fetch item name by product item id")
  public SingleBaseResponse<String> getItemNameByProductItemId(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("itemId") String itemId) {
    log.info("Fetching item name by product item id : {} ", itemId);
    try {
      ProductItem productItem =
          this.productItemService.findByStoreIdAndId(storeId, itemId);
      return new SingleBaseResponse<>(null, null, true, requestId, productItem.getGeneratedItemName());
    } catch (Exception e) {
      LOG.error("Exception: item name by product item id ", e);
      return new SingleBaseResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, requestId, null);
    }
  }

  protected Product convertRequestToProduct(ProductRequest request, String newProductCode)
      throws Exception {
    String storeId = Constants.DEFAULT_STORE_ID;
    Product product = new Product();
    BeanUtils.copyProperties(request, product, "productAttributes", "productItems", "productCategories", "images");
    product.setStoreId(storeId);

    ProductAttribute brandAttribute = null;
    Map<String, ProductAttribute> productAttributeMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(request.getProductAttributes())) {
      for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
        ProductAttribute productAttribute =
            this.convertRequestToProductAttributeForMigration(storeId, product, productAttributeRequest,
                newProductCode);
        if (Objects.isNull(productAttribute) || CollectionUtils.isEmpty(productAttribute.getProductAttributeValues())) {
          continue;
        }
        productAttributeMap.put(productAttribute.getAttribute().getAttributeCode(), productAttribute);
        if (Constants.BRAND.equalsIgnoreCase(productAttribute.getProductAttributeName())) {
          brandAttribute = productAttribute;
        }
      }
    }

    if (Objects.isNull(brandAttribute)) {
      log.info(
          "Creating brand attribute using brand value : {}  as brand not present in product attributes for product code {}",
          request.getBrand(), newProductCode);
      brandAttribute = createBrandAttribute(storeId, request.getBrand());
      productAttributeMap.put(brandAttribute.getAttribute().getAttributeCode(), brandAttribute);
    }

    for (Map.Entry<String, ProductAttribute> productAttributeEntry : productAttributeMap.entrySet()) {
      product.getProductAttributes().add(productAttributeEntry.getValue());
    }

    for (ProductCategoryRequest productCategoryRequest : request.getProductCategories()) {
      product.getProductCategories()
          .add(this.convertRequestToProductCategoryForMigration(storeId, product, productCategoryRequest));
    }

    for (Image productImageRequest : request.getImages()) {
      product.getProductImages()
          .add(convertRequestToProductImageForMigration(storeId, product, productImageRequest));
    }

    Map<String, Attribute> attributeMap = getProductItemAttribute(storeId, request.getProductItems());
    for (ProductItemRequest productItemRequest : request.getProductItems()) {
      product.getProductItems().add(
          convertRequestToProductItemsForMigration(storeId, productItemRequest, brandAttribute, newProductCode,
              attributeMap));
    }
    return product;
  }

  private Map<String, Attribute> getProductItemAttribute(String storeId, List<ProductItemRequest> productItemRequests) {
    Map<String, Attribute> attributeMap = new HashMap<>();
    List<String> distinctAttributeCodes = productItemRequests.stream()
        .flatMap(productItemRequest -> productItemRequest.getProductItemAttributeValues().stream())
        .filter(Objects::nonNull)
        .filter(attribute -> Objects.nonNull(attribute.getAttribute()))
        .map(attribute -> attribute.getAttribute().getAttributeCode())
        .distinct()
        .collect(Collectors.toList());
    List<Attribute> attributes =
        attributeService.findDetailByStoreIdAndAttributeCodeList(storeId, distinctAttributeCodes);
    attributes.forEach(attribute -> attributeMap.put(attribute.getAttributeCode(), attribute));
    return attributeMap;
  }

  protected ProductCategory convertRequestToProductCategoryForMigration(String storeId, Product product,
      ProductCategoryRequest productCategoryRequest) {
    ProductCategory productCategory = new ProductCategory(product, this.categoryService
        .getCategoryByStoreIdAndCategoryCodeAndMarkForDeleteFalse(storeId,
            productCategoryRequest.getCategory().getCategoryCode()), storeId);
    BeanUtils.copyProperties(productCategoryRequest, productCategory, "category");
    productCategory.setStoreId(storeId);
    return productCategory;
  }

  protected ProductAttribute convertRequestToProductAttributeForMigration(String storeId, Product product,
      ProductAttributeRequest request, String newProductCode) throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    if (CollectionUtils.isEmpty(request.getProductAttributeValues())) {
      return productAttribute;
    }
    BeanUtils.copyProperties(request, productAttribute, "attribute", "productAttributeValues");
    productAttribute.setProduct(product);

    if (migrateBrandAttribute(storeId, request, productAttribute)) {
      return productAttribute;
    }

    for (ProductAttributeValueRequest productAttributeValueRequest : request.getProductAttributeValues()) {
      ProductAttributeValue productAttributeValue = new ProductAttributeValue();
      BeanUtils.copyProperties(productAttributeValueRequest, productAttributeValue, "allowedAttributeValue",
          "predefinedAllowedAttributeValue", "descriptiveAttributeValueType");
      productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType
          .valueOf(productAttributeValueRequest.getDescriptiveAttributeValueType().toString()));
      productAttributeValue.setStoreId(storeId);

      if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equals(request.getAttribute().getAttributeType().name())) {
        Attribute attribute = attributeService
            .findAttributeByCodeOrNameAndValue(storeId, request.getAttribute().getAttributeCode(),
                request.getProductAttributeName(), productAttributeValue.getDescriptiveAttributeValue(),
                String.valueOf(request.getAttribute().getAttributeType()));
        if (Objects.isNull(attribute)) {
          log.warn("Skipping descriptive attribute {} for product : {} ", request.getAttribute().getAttributeCode(),
              newProductCode);
          return null;
        }
        log.info("Found descriptive attribute for attribute request : {} , attribute code : {}",
            request.getAttribute().getAttributeCode(), attribute.getAttributeCode());
        productAttribute.setAttribute(attribute);
        productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
        if (StringUtils.isEmpty(productAttributeValue.getDescriptiveAttributeValue())) {
          if (attribute.isMandatory()) {
            log.warn("Value for mandatory descriptive attribute is not filled for attribute : {} for product : {}",
                attribute.getAttributeCode(), newProductCode);
            return null;
          } else {
            productAttributeValue.setDescriptiveAttributeValue(Constants.HYPHEN);
          }
        }
        if (productAttributeValue.getDescriptiveAttributeValue().length() > 255) {
          productAttributeValue
              .setDescriptiveAttributeValue(productAttributeValue.getDescriptiveAttributeValue().substring(0, 255));
        }
      }

      if (AttributeType.DEFINING_ATTRIBUTE.name().equals(request.getAttribute().getAttributeType().name())) {
        if (Objects.nonNull(productAttributeValueRequest.getAllowedAttributeValue())) {
          AllowedAttributeValueRequest allowedAttributeValueRequest =
              productAttributeValueRequest.getAllowedAttributeValue();
          Attribute attribute = attributeService
              .findAttributeByCodeOrNameAndValue(storeId, request.getAttribute().getAttributeCode(),
                  request.getProductAttributeName(), allowedAttributeValueRequest.getValue(),
                  String.valueOf(request.getAttribute().getAttributeType()));
          if (Objects.isNull(attribute)) {
            log.warn("Skipping defining attribute : {} for product : {}", request.getAttribute().getAttributeCode(), newProductCode);
            return null;
          }
          AllowedAttributeValue allowedAttributeValue = null;
          log.info("Found defining attribute for attribute request : {} , attribute code : {}",
              request.getAttribute().getAttributeCode(), attribute.getAttributeCode());
          productAttribute.setAttribute(attribute);
          productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
          if (StringUtils.isNotBlank(allowedAttributeValueRequest.getAllowedAttributeCode())) {
            log.info("fetching allowedAttributeValue with allowed attribute code : {}",
                allowedAttributeValueRequest.getAllowedAttributeCode());
             allowedAttributeValue = allowedAttributeValueService
                .findByStoreIdAndAllowedAttributeCode(Constants.DEFAULT_STORE_ID,
                    allowedAttributeValueRequest.getAllowedAttributeCode());
            productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
          } else {
            log.info("fetching allowedAttributeValue by value : {} and attribute id : {}",
                allowedAttributeValueRequest.getValue(), attribute.getId());
            List<AllowedAttributeValue> allowedAttributeValues = allowedAttributeValueService
                .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute,
                    allowedAttributeValueRequest.getValue());
            if (CollectionUtils.isEmpty(allowedAttributeValues)) {
              log.warn("Skipping defining attribute as value not found for attribute: {} for product : {}",
                  request.getAttribute().getAttributeCode(), newProductCode);
              return null;
            }
            allowedAttributeValue = allowedAttributeValues.get(0);
            log.info("Setting allowedAttributeValues {}", allowedAttributeValue.getAllowedAttributeCode());
            productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
          }
          if (Objects.isNull(allowedAttributeValue)) {
            log.warn("Skipping defining attribute as allowed attribute not found for attribute: {} for product : {}",
                request.getAttribute().getAttributeCode(), newProductCode);
            return null;
          }
        } else {
          log.warn("Skipping defining attribute as allowed attribute is null for  attribute: {} for product : {}",
              request.getAttribute().getAttributeCode(), newProductCode);
          return null;
        }
      }

      if (AttributeType.PREDEFINED_ATTRIBUTE.name().equals(request.getAttribute().getAttributeType().name())) {
        productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
        if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
          PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
              productAttributeValueRequest.getPredefinedAllowedAttributeValue();
          Attribute attribute = attributeService
              .findAttributeByCodeOrNameAndValue(storeId, request.getAttribute().getAttributeCode(),
                  request.getProductAttributeName(), predefinedAllowedAttributeValueRequest.getValue(),
                  String.valueOf(request.getAttribute().getAttributeType()));
          if (Objects.isNull(attribute)) {
            log.warn("Skipping predefined attribute as allowed attribute not found for attribute: {} for product : {}",
                request.getAttribute().getAttributeCode(), newProductCode);
            return null;
          }
          log.info("Found predefined attribute for attribute request : {} , attribute code : {}",
              request.getAttribute().getAttributeCode(), attribute.getAttributeCode());
          productAttribute.setAttribute(attribute);
          PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = null;
          if (StringUtils.isNotBlank(predefinedAllowedAttributeValueRequest.getPredefinedAllowedAttributeCode())) {
            log.info("fetching predefinedAllowedAttributeValue with predefined attribute code : {}",
                predefinedAllowedAttributeValueRequest.getPredefinedAllowedAttributeCode());
             predefinedAllowedAttributeValue = predefinedAllowedAttributeValueService
                .findByStoreIdAndPredefinedAllowedAttributeCode(Constants.DEFAULT_STORE_ID,
                    predefinedAllowedAttributeValueRequest.getPredefinedAllowedAttributeCode());
            productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
          } else {
            String value = StringUtils.isNotBlank(predefinedAllowedAttributeValueRequest.getValue()) ?
                predefinedAllowedAttributeValueRequest.getValue() :
                Constants.HYPHEN;
            log.info("fetching predefinedAllowedAttributeValue with value: {}", value);
            List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
                predefinedAllowedAttributeValueService
                    .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute,
                        value);
            if (CollectionUtils.isEmpty(predefinedAllowedAttributeValues)) {
              log.warn("Skipping predefined attribute as value not found for attribute: {} for product : {}",
                  request.getAttribute().getAttributeCode(), newProductCode);
              return null;
            }
            predefinedAllowedAttributeValue = predefinedAllowedAttributeValues.get(0);
            log.info("Setting predefined allowed attribute value  {}",
                predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode());
            productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
          }
          if (Objects.isNull(predefinedAllowedAttributeValue)) {
            log.warn("Skipping predefined attribute as predefined allowed attribute not found for attribute: {} for product : {}",
                request.getAttribute().getAttributeCode(), newProductCode);
            return null;
          }
        } else {
          Attribute attribute = attributeService
              .findAttributeByCodeOrNameAndValue(storeId, request.getAttribute().getAttributeCode(),
                  request.getProductAttributeName(), StringUtils.EMPTY,
                  String.valueOf(request.getAttribute().getAttributeType()));
          if (Objects.isNull(attribute)) {
            log.warn("Skipping attribute : {} ", request.getAttribute().getAttributeCode());
            return null;
          }
          if (attribute.isMandatory()) {
            log.warn(
                "Skipping predefined attribute as attribut is mandatory and no value is filled for for attribute: {} for product : {}",
                request.getAttribute().getAttributeCode(), newProductCode);
            return null;
          }
          productAttribute.setAttribute(attribute);
          List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
              predefinedAllowedAttributeValueService
                  .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute,
                      Constants.HYPHEN);
          productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValues.get(0));
        }
      }
      productAttributeValue.setProductAttribute(productAttribute);
      productAttribute.getProductAttributeValues().add(productAttributeValue);
    }

    productAttribute.setStoreId(storeId);
    return productAttribute;
  }

  private boolean migrateBrandAttribute(String storeId, ProductAttributeRequest request,
      ProductAttribute productAttribute) throws Exception {
    if (Constants.BRAND.equalsIgnoreCase(request.getProductAttributeName())) {
      Attribute attribute = attributeService.findDetailByStoreIdAndAttributeCode(storeId, Constants.BRAND_CODE);
      productAttribute.setAttribute(attribute);
      for (ProductAttributeValueRequest productAttributeValueRequest : request.getProductAttributeValues()) {
        ProductAttributeValue productAttributeValue = new ProductAttributeValue();
        BeanUtils.copyProperties(productAttributeValueRequest, productAttributeValue, "allowedAttributeValue",
            "predefinedAllowedAttributeValue", "descriptiveAttributeValueType");
        productAttributeValue.setStoreId(storeId);
        if (StringUtils.isNotBlank(productAttributeValue.getDescriptiveAttributeValue())) {
          List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
              predefinedAllowedAttributeValueService
                  .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute,
                      productAttributeValue.getDescriptiveAttributeValue());
          if (CollectionUtils.isNotEmpty(predefinedAllowedAttributeValues)) {
            log.info("Setting predefined allowed attribute value  {}",
                predefinedAllowedAttributeValues.get(0).getPredefinedAllowedAttributeCode());
            productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValues.get(0));
          }
        }

        if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
          PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
              productAttributeValueRequest.getPredefinedAllowedAttributeValue();
          if (StringUtils.isNotBlank(predefinedAllowedAttributeValueRequest.getPredefinedAllowedAttributeCode())) {
            log.info("fetching predefinedAllowedAttributeValue for brand with predefined attribute code : {}",
                predefinedAllowedAttributeValueRequest.getPredefinedAllowedAttributeCode());
            PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = predefinedAllowedAttributeValueService
                .findByStoreIdAndPredefinedAllowedAttributeCode(Constants.DEFAULT_STORE_ID,
                    predefinedAllowedAttributeValueRequest.getPredefinedAllowedAttributeCode());
            if (Objects.nonNull(predefinedAllowedAttributeValue)) {
              productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
            }
          } else {
            log.info("fetching predefinedAllowedAttributeValue for brand with value: {}",
                predefinedAllowedAttributeValueRequest.getValue());
            List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues =
                predefinedAllowedAttributeValueService
                    .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute,
                        predefinedAllowedAttributeValueRequest.getValue());
            if (CollectionUtils.isNotEmpty(predefinedAllowedAttributeValues)) {
              log.info("Setting predefined allowed attribute value  {}",
                  predefinedAllowedAttributeValues.get(0).getPredefinedAllowedAttributeCode());
              productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValues.get(0));
            }
          }
        }
        productAttributeValue.setDescriptiveAttributeValue(null);
        productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
        productAttributeValue.setProductAttribute(productAttribute);
        if (Objects.nonNull(productAttributeValue.getPredefinedAllowedAttributeValue())) {
          productAttribute.getProductAttributeValues().add(productAttributeValue);
        }
      }
      productAttribute.setStoreId(storeId);
      return true;
    }
    return false;
  }

  private ProductAttribute createBrandAttribute(String storeId, String value) throws Exception {
    ProductAttribute productAttribute = new ProductAttribute();
    Attribute attribute = attributeService.findDetailByStoreIdAndAttributeCode(storeId, Constants.BRAND_CODE);
    productAttribute.setAttribute(attribute);
    productAttribute.setStoreId(storeId);
    productAttribute.setProductAttributeName(Constants.BRAND);
    ProductAttributeValue productAttributeValue = new ProductAttributeValue();
    productAttributeValue.setStoreId(storeId);
    List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues = new ArrayList<>();
    predefinedAllowedAttributeValues = predefinedAllowedAttributeValueService
        .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute, value);
    if (CollectionUtils.isEmpty(predefinedAllowedAttributeValues)) {
      predefinedAllowedAttributeValues = predefinedAllowedAttributeValueService
          .findByStoreIdAndAttributeAndValueOrderByMarkForDelete(Constants.DEFAULT_STORE_ID, attribute,
              Constants.NO_BRAND);
    }
    productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValues.get(0));
    productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
    productAttributeValue.setProductAttribute(productAttribute);
    productAttribute.getProductAttributeValues().add(productAttributeValue);
    return productAttribute;
  }

  public ProductItem convertRequestToProductItemsForMigration(String storeId, ProductItemRequest productItemRequest,
      ProductAttribute brandAttribute, String productCode, Map<String, Attribute> attributeMap) throws Exception {
    ProductItem productItem = new ProductItem();
    BeanUtils.copyProperties(productItemRequest, productItem, "product", "productItemAttributeValues", "images");

    List<ProductItemImage> productItemImages = new ArrayList<>();
    for (Image image : productItemRequest.getImages()) {
      ProductItemImage productItemImage = new ProductItemImage();
      BeanUtils.copyProperties(image, productItemImage);
      productItemImage.setStoreId(storeId);
      productItemImages.add(productItemImage);
    }
    productItem.setProductItemImages(productItemImages);

    List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
    StringBuffer attributeValues = new StringBuffer(productCode);
    if (CollectionUtils.isNotEmpty(productItemRequest.getProductItemAttributeValues())) {
      for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemRequest
          .getProductItemAttributeValues()) {
        AttributeRequest attributeRequest = productItemAttributeValueRequest.getAttribute();
        if (Constants.BRAND_CODE.equals(attributeRequest.getAttributeCode()) || Constants.OLD_BRAND_CODE
            .equals(attributeRequest.getAttributeCode())) {
          continue;
        }
        ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
        BeanUtils.copyProperties(productItemAttributeValueRequest, productItemAttributeValue);
        Attribute attribute = null;
        try {
          attribute = attributeMap.get(productItemAttributeValueRequest.getAttribute().getAttributeCode());
          if (Objects.isNull(attribute)) {
            log.warn("Item attribute not found for attribute code : {}",
                productItemAttributeValueRequest.getAttribute().getAttributeCode());
            continue;
          }
        } catch (Exception e) {
          log.warn("Item attribute not found for attribute code : {}",
              productItemAttributeValueRequest.getAttribute().getAttributeCode());
          continue;
        }
        productItemAttributeValue.setStoreId(storeId);
        productItemAttributeValue.setAttribute(attribute);
        productItemAttributeValues.add(productItemAttributeValue);
        attributeValues
            .append(Constants.SPACE + attribute.getId() + Constants.SPACE + productItemAttributeValue.getValue());
      }
      productItem.setProductItemAttributeValues(productItemAttributeValues);
    }

    String brandValue =
        brandAttribute.getProductAttributeValues().get(0).getPredefinedAllowedAttributeValue().getValue();
    ProductItemAttributeValue brandAttributeValue = new ProductItemAttributeValue();
    brandAttributeValue.setStoreId(storeId);
    brandAttributeValue.setAttribute(brandAttribute.getAttribute());
    brandAttributeValue.setValue(brandValue);
    productItem.getProductItemAttributeValues().add(brandAttributeValue);
    attributeValues
        .append(Constants.SPACE + brandAttribute.getAttribute().getId() + Constants.SPACE + brandValue);
    productItem.setStoreId(storeId);
    productItem.setHash(GdnDigestUtil.getDigestFromString("SHA-256", "UTF-8", attributeValues.toString()));
    return productItem;
  }

  public ProductImage convertRequestToProductImageForMigration(String storeId, Product product,
      Image productImageRequest) {
    ProductImage productImage =
        new ProductImage(product, productImageRequest.isMainImages(), productImageRequest.getLocationPath(),
            productImageRequest.getSequence(), storeId);
    BeanUtils.copyProperties(productImageRequest, productImage, "product");
    productImage.setStoreId(storeId);
    return productImage;
  }

  @RequestMapping(value = ProductApiPath.FETCH_IMAGES_FOR_SCALING, method = RequestMethod.GET,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "get images for scaling of product by product code", description = "get "
      + "images for scaling of product by product code")
  public GdnRestSingleResponse<ProductDetailResponse> getImagesForScalingByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode) throws Exception {
    GdnPreconditions
        .checkArgument(StringUtils.isNotBlank(productCode), ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    try {
      Product product = service.getImagesByProductCode(storeId, productCode);
      ProductDetailResponse productDetailResponse =
          ConverterUtil.convertProductToProductDetailResponseForImages(product);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, productDetailResponse, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while fetching the scaling images for product code  : {}", productCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error while fetching the scaling images for product code  : {}", productCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_PRODUCT_ITEM_IDS_BY_SKU_CODES, method =
      RequestMethod.POST, consumes = MediaType.APPLICATION_JSON_VALUE, produces =
 MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product item ids by skuCodes", description = "get product item ids by"
  + " skuCodes")
  public GdnRestSingleResponse<SingleObjectResponse<Map<String, String>>> getProductItemIdsBySkuCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SkuCodesRequest request) throws Exception {
    LOG.info("Get product item ids by skuCodes : {} ", request);
    Map<String, String> skuCodesAndProductItemIdsMap =
        this.productItemService.getProductItemIdsBySkuCode(storeId, request.getSkuCodes());
    return new GdnRestSingleResponse<>(null, null, true,
        new SingleObjectResponse<>(skuCodesAndProductItemIdsMap), requestId);
  }

  @RequestMapping(value = ProductApiPath.UPDATE_PRODUCT_NEED_REVISION, method = RequestMethod.PUT
      , produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Refresh product detail and mark as need revision", description = "Refresh"
      + " product detail and mark as need revision")
  public GdnBaseRestResponse updateAndMarkProductForNeedCorrection(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @PathVariable("productCode") String productCode,
      @RequestBody ProductRequest productRequest) throws Exception {
    log.info("Update product details and flags for need revision with request : {}", productRequest);
    try {
      Product product =
          this.convertRequestToProduct(storeId, false, productRequest, new ArrayList<>(), false);
      fetchAndSetItemDetails(productRequest, product, true);
      this.productServiceWrapper.updateAndMarkForNeedRevision(product);
      return new GdnBaseRestResponse(null, null, true, requestId);
    } catch (Exception e) {
      log.error("Error updating to need revision for product code : {}, error - ", productRequest.getProductCode(), e);
      return new GdnBaseRestResponse(ErrorCategory.UNSPECIFIED.getMessage(), ErrorCategory.UNSPECIFIED.getCode(),
          false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.BATCH_VAT_UPDATE_BY_SKU_CODE, method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update vat by sku codes", description = "update vat by sku codes")
  public GdnRestListResponse<BatchVatUpdateResponse> updateVatFlagBySkuCodes(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam String username, @RequestBody BatchVatUpdateRequest batchVatUpdateRequest) {
    log.info("Updating vat flag for seller : {}", batchVatUpdateRequest.getBusinessPartnerCode());
    try {
      return new GdnRestListResponse<>(null, null, true,
          this.productServiceWrapper.updateVatFlagBySkuCodes(storeId, requestId, username, batchVatUpdateRequest),
          new PageMetaData(), requestId);
    } catch (Exception e) {
      log.error("Error updating vat flag for request : {}, error - ", batchVatUpdateRequest.getItemCodeToVatMap(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.MASTER_DETAIL_BY_ITEM_CODE, method = RequestMethod.GET,
                  produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get master product detail by item code", description = "Get master "
      + "product detail by item code")
  public GdnRestSingleResponse<ProductMasterDataResponse> getMasterProductDetailsByItemCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("itemCode") String itemCode) throws Exception {
    LOG.info("Fetch master detail for transaction. itemCode : {} ", itemCode);
    try {
      ProductMasterDataResponse productMasterDataResponse =
          productServiceWrapper.getMasterProductDetailsByItemCode(storeId, itemCode);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, productMasterDataResponse,
          requestId);
    } catch (Exception e) {
      log.error("Error in fetching master detail for transaction. itemCode : {}", itemCode, e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_IMAGES, method = RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update productItem images by product code", description =
      "update productItem images by " + "product code")
  public GdnRestSingleResponse<SingleObjectResponse<Map<String, String>>> updateImages(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductImageEditRequest request) {
    LOG.info("updating images for request : {} ", request);
    try {
      Map<String, String> errorMap = productServiceWrapper.updateImages(storeId, request);
      return new GdnRestSingleResponse<>(null, null, true, new SingleObjectResponse<>(errorMap), requestId);
    } catch (Exception e) {
      log.error("Error while updating images for request  : {} ", request, e);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_COMMON_IMAGES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update productItem images by product code", description =
      "update common images by " + "product code")
  public GdnRestSingleResponse<SingleObjectResponse<Map<String, Map<String, String>>>> updateCommonImages(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody List<ProductImageEditRequest> request) {
    LOG.info("updating images for request : {} ", request);
    try {
      Map<String, Map<String, String>> errorMap = productServiceWrapper.updateCommonImages(storeId, request);
      return new GdnRestSingleResponse<>(null, null, true, new SingleObjectResponse<>(errorMap), requestId);
    } catch (Exception e) {
      log.error("Error while updating images for request  : {} ", request, e);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_IMAGES_BY_PRODUCT_CODE, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product images by product code", description = "get product images by"
      + " product code")
  public GdnRestListResponse<ImageResponse> getProductImagesByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "true") boolean removeOriginalImages,
      @PathVariable("productCode") String productCode) throws Exception {
    try {
      log.info("fetching product images for productCode : {} ", productCode);
      List<ImageResponse> response = this.productServiceWrapper.findProductImagesByProductCode(storeId, productCode, removeOriginalImages);
      return new GdnRestListResponse<>(null, null, true, response,
          new PageMetaData(response.size(), 0, response.size()), requestId);
    } catch (Exception e) {
      LOG.error("Error while fetching product images by product code : {} ", productCode, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.ITEM_IMAGES_BY_ITEM_CODES, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get product item images by item codes", description = "get product item "
      + "images by item codes")
  public GdnRestListResponse<ItemImageResponse> getProductItemImagesByItemCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam(required = false, defaultValue = "true") boolean removeOriginalImages,
      @RequestBody SkuCodesRequest itemCodes) throws Exception {
    try {
      log.info("fetching product item images for itemCodes : {} ", itemCodes.getSkuCodes());
      List<ItemImageResponse> response =
        this.productItemServiceWrapper.findProductItemImagesByItemCodes(storeId,
          itemCodes.getSkuCodes(), removeOriginalImages, itemCodes.getFetchImageResponse());
      return new GdnRestListResponse<>(null, null, true, response,
          new PageMetaData(response.size(), 0, response.size()), requestId);
    } catch (Exception e) {
      LOG.error("Error while fetching product item images by item code : {} ", itemCodes, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_AND_ATTRIBUTE_DETAIL_BY_PRODUCT_CODE, method =
      RequestMethod.GET, produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "get details of product and attribute by product code", description = "get"
      + " details of product and attribute by product code")
  public GdnRestSingleResponse<ProductAndAttributeDetailResponse> getProductAndAttributeDetailsByProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode,
      @RequestParam(required = false, defaultValue = "false") boolean inAllProducts)
      throws Exception {
    try {
      LOG.info("Fetching product and attribute details by product-code {} ", productCode);
      Product product = this.productServiceWrapper.getProductAndAttributeDetailsByProductCode(storeId, productCode,
          inAllProducts);
      ProductAndAttributeDetailResponse productAndAttributeDetailResponse = ConverterUtil
          .convertProductToProductAndAttributeDetailResponse(product);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, productAndAttributeDetailResponse, requestId);
    } catch (Exception e) {
      log.error("Error while fetching product and attribute details by product-code {} ", productCode, e);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_ATTRIBUTE_DETAIL_BY_PRODUCT_ID, method =
      RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "Get details of product attribute by productId", description = "Get "
      + "details of product attribute by productId")
  public GdnRestSingleResponse<ProductAndAttributeDetailResponse> getProductAttributeDetailsByProductId(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productId") String productId) throws Exception {
    try {
      LOG.info("Fetching product and attribute details by productId {} ", productId);
      List<ProductAttribute> productAttributes =
          this.productServiceWrapper.getProductAttributeDetailsByProductId(storeId, productId);
      List<ProductAttributeResponse> productAndAttributeDetailResponse =
          ConverterUtil.getProductAttributeResponses(productAttributes);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true,
          new ProductAndAttributeDetailResponse(productAndAttributeDetailResponse), requestId);
    } catch (Exception e) {
      log.error("Error while fetching product attribute details by productId {} ", productId, e);
      return new GdnRestSingleResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.AUTO_FILL_PRODUCT_ATTRIBUTES, method = RequestMethod.GET
      , produces = {
      MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "extract and autofill product attributes", description = "extract and "
  + "autofill product attributes")
  public GdnRestListResponse<AttributeHistoryResponse> autoFillProductAttribute(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode)
      throws Exception {
    try {
      LOG.info("extract and autofill product attributes for product-code {} ", productCode);
      List<AttributeHistoryResponse> attributeHistoryResponseList =
          productServiceWrapper.autoFillAttributes(storeId, productCode);
      return new GdnRestListResponse<>(StringUtils.EMPTY, StringUtils.EMPTY, true, attributeHistoryResponseList,
          new PageMetaData(0, attributeHistoryResponseList.size(), attributeHistoryResponseList.size()), requestId);
    } catch (Exception e) {
      log.error("Error while extract and autofill product attributes for product-code {} ", productCode, e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, null,
          requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_CODE_BY_UPC_CODE_PRODUCT_CODE, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "Fetch item code by upc codes and product code for product")
  public GdnRestListResponse<SingleObjectResponse<List<String>>> getItemCodeByUpcCodeAndProductCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam String username,
      @RequestParam String productCode, @RequestBody ProductItemUpcCodesSkuCodesRequest request) {
    log.info("Fetching item code by upc codes : {}  and product code : {} and skuCodes : {} for product",
        request.getUpcCodes(), productCode, request.getSkuCodes());
    try {
      List<String> itemCodeList =
          this.productItemService.getItemCodeByUPCCodeAndProductCode(request.getUpcCodes(), productCode,
              request.getSkuCodes());
      return new GdnRestListResponse<>(null, null, true, Arrays.asList(new SingleObjectResponse<>(itemCodeList)), null,
          requestId);
    } catch (Exception e) {
      LOG.error("Exception: item code by upc code and product code for product ", e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_MASTER_DATA_IMAGES_UPC_CODE, method =
      RequestMethod.POST, produces = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE}, consumes = {
      MediaType.APPLICATION_JSON_VALUE, MediaType.APPLICATION_XML_VALUE})
  @Operation(summary = "update product master data and images and upc code", description =
 "update product master data and images and upc code")
  public GdnRestSingleResponse<EditProductItemAndImageResponse> updateProductMasterDataAndImagesAndUpcCode(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("productCode") String productCode,
      @RequestParam(required = false, defaultValue = "false") boolean ignoreSalesCategoryPublish,
      @RequestBody EditProductDetailRequest editProductDetailRequest) throws Exception {
    log.info(
      "Invoking updateProductMasterDataAndImagesAndUpcCode for product : {} with request : {} ", productCode, editProductDetailRequest);
    GdnPreconditions.checkArgument(Objects.nonNull(editProductDetailRequest.getProductRequest()),
      ErrorMessage.PRODUCT_REQUEST_CANNOT_BE_NULL.getMessage());
    GdnPreconditions.checkArgument(
      (StringUtils.isNotEmpty(editProductDetailRequest.getProductRequest().getUpdatedBy()) || Objects.nonNull(editProductDetailRequest.getProductRequest().getUpdatedDate())),
      ErrorMessage.ENTITY_REQUIRED_VALIDATION_FOR_UPDATE_MESSAGE.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
      ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    List<NewlySavedItemResponse> newlySavedItemResponseList = new ArrayList<>();
    try {
      Product product =
          this.convertRequestToProduct(storeId, true, editProductDetailRequest.getProductRequest(),
              newlySavedItemResponseList, false);
      EditProductItemAndImageResponse editProductItemAndImageResponse =
        productServiceWrapper.updateProductMasterDataAndImagesAndUpcCode(product,
        editProductDetailRequest, productCode, storeId, newlySavedItemResponseList);
      return new GdnRestSingleResponse<>(null, null, true, editProductItemAndImageResponse,
        requestId);
    } catch (Exception e) {
      log.error("Error while performing Content and Image update for : product {} ", productCode,
        e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
        null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.UPDATE_BRAND_DATA, method = RequestMethod.POST,
                  produces = {
      MediaType.APPLICATION_JSON_VALUE}, consumes = {MediaType.APPLICATION_JSON_VALUE})
  @Operation(summary = "update brand data of a product", description = "update brand data of a "
      + "product using product code")
  public GdnRestSingleResponse<ProductBrandUpdateResponse> updateProductBrandData(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductBrandUpdateRequest request) {
    try {
      LOG.info("Invoking updateProductBrandData : {}", request);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getProductCode()),
          ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getOldBrandCode()),
          ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(request.getNewBrandCode()),
          ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK.getMessage());
      GdnPreconditions.checkArgument(!StringUtils.equals(request.getNewBrandCode(), request.getOldBrandCode()),
          ErrorMessage.NEW_BRAND_CODE_CANNOT_BE_SAME_AS_OLD_BRAND_CODE.getMessage());

      ProductBrandUpdateDTO productBrandUpdateDTO = ConverterUtil.getProductBrandUpdateDTO(request);
      ProductBrandUpdateResponseDTO responseDTO =
          productServiceWrapper.updateProductBrandData(storeId, productBrandUpdateDTO);
      ProductBrandUpdateResponse response = ConverterUtil.getProductBrandUpdateResponse(responseDTO);
      return new GdnRestSingleResponse<>(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException e) {
      log.error("Error while updating product brand by product-code {} ", request.getProductCode(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), e.getErrorCodes().getCode(), false, null, requestId);
    } catch (Exception e) {
      log.error("Error while updating product brand by product-code {} ", request.getProductCode(), e);
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }

  @RequestMapping(value = ProductApiPath.GET_ITEM_CODES_BY_IDS, method = RequestMethod.POST,
                  consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product item ids by skuCodes", description = "get product item ids by"
      + " skuCodes")
  public GdnRestSingleResponse<SimpleStringMapResponse> getSkuCodesByProductItemIds(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody SimpleStringListRequest simpleStringListRequest) throws Exception {
    LOG.info("Get product item ids by skuCodes : {} ", simpleStringListRequest);
    Map<String, String> skuCodesAndProductItemIdsMap =
        this.productItemService.getBySkuCodeByProductItemIds(storeId, simpleStringListRequest.getRequest());
    return new GdnRestSingleResponse<>(null, null, true, new SimpleStringMapResponse(skuCodesAndProductItemIdsMap),
        requestId);
  }

  @RequestMapping(value = ProductApiPath.PRODUCT_SUITABILITY_ATTRIBUTE_MAPPING, method = RequestMethod.POST,
      consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "map ds attributes to product", description = "map ds attributes to product")
  public GdnRestSingleResponse<SimpleStringMapResponse> mapDsAttributesToProduct(
      @RequestBody ProductSuitabilityEventModel productSuitabilityEventModel) throws Exception {
    this.productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    return new GdnRestSingleResponse<>(null, null, true, new SimpleStringMapResponse(null), null);
  }

  @PostMapping(value = ProductApiPath.GET_PRODUCT_MASTER_DATA_BY_PRODUCT_CODE, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "get product master data info by product code", description = "get product master data info by product code")
  public GdnRestListResponse<ProductMasterDataItemResponse> getProductMasterDataByProductCode(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestBody ProductCodesRequest request) {
    String errorMessage = StringUtils.EMPTY;
    List<ProductMasterDataItemResponse> productMasterDataItemResponse = new ArrayList<>();
    try {
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(request.getProductCodes()),
          ErrorMessage.PRODUCT_CODE_LIST_MUST_NOT_BE_EMPTY.getMessage());
      log.info("#getProductMasterDataByProductCode with productCodes = {}, ", request.getProductCodes());
      productMasterDataItemResponse = productServiceWrapper.getProductMasterDataByProductCode(storeId, request);
      return new GdnRestListResponse<ProductMasterDataItemResponse>(null, null, true, productMasterDataItemResponse, null, requestId);
    } catch (Exception e) {
      errorMessage = e.getMessage();
      log.error("error invoking getProductMasterDataByProductCode: {} ", request.getProductCodes(), e);
      return new GdnRestListResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getMessage(), false, requestId);
    }
  }

  @PostMapping(value = ProductApiPath.UPDATED_BRAND, produces = MediaType.APPLICATION_JSON_VALUE,
               consumes = MediaType.APPLICATION_JSON_VALUE)
  @Operation(summary = "update brand data for a product", description = "update brand data for a "
      + "product")
  public GdnRestSingleResponse<ProductResponse> updateBrandDataForProduct(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @RequestBody ProductBrandDataUpdateRequest productBrandDataUpdateRequest) {
    try {
      log.info("Update brand value for the product:{} with the request: {}",
          productBrandDataUpdateRequest.getProductCode(), productBrandDataUpdateRequest);
      ProductBrandUpdateResponse response =
          productServiceWrapper.updateProductBrandDataWithBrandInfo(storeId,
              productBrandDataUpdateRequest);
      return new GdnRestSingleResponse(null, null, true, response, requestId);
    } catch (ApplicationRuntimeException ex) {
      log.error("Error updating brand data of the product:{}, e - ",
          productBrandDataUpdateRequest.getProductCode(), ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ex.getErrorCodes().getCode(), false, null,
          requestId);
    } catch (Exception ex) {
      log.error("Error updating brand data of the product:{}, e - ",
          productBrandDataUpdateRequest.getProductCode(), ex);
      return new GdnRestSingleResponse<>(ErrorCategory.UNSPECIFIED.getMessage(),
          ErrorCategory.UNSPECIFIED.getCode(), false, null, requestId);
    }
  }
}