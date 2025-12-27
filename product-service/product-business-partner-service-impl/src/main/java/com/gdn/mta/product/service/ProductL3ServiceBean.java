package com.gdn.mta.product.service;

import static com.gdn.common.base.GdnPreconditions.checkState;
import static com.gdn.mta.product.util.CommonUtils.merchantTypesForBopisCategoryValidation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.response.DistributionInfo;
import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gda.mta.product.dto.ItemSkuPickupPointRequest;
import com.gda.mta.product.dto.ProductCodeAndSkuRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.response.DeleteInProgressL5Response;
import com.gda.mta.product.dto.response.ProductSkuDetailResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.mta.product.commons.constant.UpdateProductActivity;
import com.gdn.mta.product.entity.WorkflowStates;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductType;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.mta.product.service.config.PreOrderConfig;
import com.gdn.mta.product.util.ConverterUtil;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.util.ValueTypeUtil;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.outbound.inventory.InventoryOutbound;
import com.gdn.partners.pbp.outbound.productPricing.ProductPricingOutbound;
import com.gdn.partners.pbp.outbound.xbp.feign.XbpFeign;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.ListRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.WebInventoryUpdatePickupPointRequestDTO;
import com.gdn.x.inventory.v2.rest.web.model.transaction.response.WebInventoryUpdatePickupPointResponseDTO;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.rest.web.model.dto.AuditTrailDto;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductDTO;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.AuditTrailListResponse;
import com.gdn.x.product.rest.web.model.response.DeleteItemPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;

import com.gdn.x.product.rest.web.model.response.ProductCenterDetailResponse;

import com.gdn.x.productcategorybase.dto.VideoDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.CategoryDetailDto;
import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductItemLevel3LogisticResponse;
import com.gda.mta.product.dto.ProductL3CommonImageResponse;
import com.gda.mta.product.dto.ProductLevel3AttributeResponse;
import com.gda.mta.product.dto.ProductScoreResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gda.mta.product.dto.response.PreOrderResponse;
import com.gda.mta.product.dto.response.ProductL3DetailsResponse;
import com.gda.mta.product.dto.ProductExistenceAndPreOrderDTO;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductCollection;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.repository.BusinessPartnerRepository;
import com.gdn.mta.product.repository.CategoryRepository;
import com.gdn.mta.product.repository.ProductBusinessPartnerRepository;
import com.gdn.mta.product.repository.ProductCollectionRepository;
import com.gdn.mta.product.repository.ProductRepository;
import com.gdn.mta.product.service.util.MapperUtil;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dto.productlevel3.ProductItemWholesalePriceResponse;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.helper.ResponseHelper;
import com.gdn.partners.pbp.model.productlevel3.ProductLevel3Inventory;
import com.gdn.partners.pbp.outbound.campaign.CampaignOutbound;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.partners.pbp.service.productlevel3.ProductItemWholesalePriceService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3InventoryService;
import com.gdn.partners.pbp.service.productlevel3.ProductLevel3LogisticsService;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.inventory.v2.rest.web.model.transaction.request.InventoryDetailInfoRequestDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.MasterDataProductImageDTO;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDTO;
import com.gdn.x.product.rest.web.model.dto.ProductAttributeDetailDTO;
import com.gdn.x.product.rest.web.model.dto.ProductSpecialAttributeDTO;
import com.gdn.x.product.rest.web.model.response.BusinessPartnerPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ProductBasicResponse;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeRequest;
import com.gdn.x.productcategorybase.dto.request.SkuCodesRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.DistributionInfoResponse;
import com.gdn.x.productcategorybase.dto.response.ImageResponse;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.google.common.collect.Lists;
import org.springframework.transaction.annotation.Transactional;

@Service
public class ProductL3ServiceBean implements ProductL3Service {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductL3ServiceBean.class);
  private static final String RESIZE = "resize/";
  private static final int ORDINAL_INDEX = 2;

  @Autowired
  private ProductCollectionRepository productCollectionRepository;

  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private ProductBusinessPartnerRepository productBusinessPartnerRepository;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private XbpFeign xbpFeign;

  @Autowired
  private CategoryRepository categoryRepository;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  @Autowired
  private ProductLevel3LogisticsService productLevel3LogisticsService;

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductItemWholesalePriceService productItemWholesalePriceService;

  @Autowired
  private ProductLevel3InventoryService productLevel3InventoryService;

  @Autowired
  private CampaignOutbound campaignOutbound;

  @Autowired
  private ProductLevel3Service productLevel3Service;

  @Autowired
  private ProductItemBusinessPartnerService productItemBusinessPartnerService;

  @Autowired
  private MapperUtil mapperUtil;

  @Autowired
  private ProductSystemParameterService productSystemParameterService;

  @Autowired
  private ProductPricingOutbound productPricingOutbound;

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private InventoryOutbound inventoryOutbound;

  @Autowired
  private ProductLevel3ServiceBean productLevel3ServiceBean;

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Autowired
  private PreOrderConfig preOrderConfig;

  @Value("${product.suitability.feature.enabled}")
  private boolean productSuitabilityFeatureEnabled;

  private static final String DELETE_PICKUP_POINT_NOT_ALLOWED =
    "Delete not allowed, Item will be left with no active L5 ";

  private static final String REASON = "No Default PickupPoint";

  @Value("${mpp.allowed.sellers}")
  private String mppAllowedSellers;

  @Value("${back.fill.wrong.product.item.id}")
  private boolean backFillWrongProductItemIds;

  @Value("${fbb.sorting.sellers}")
  private String fbbSortingSellers;

  @Value("${auto.heal.main.image.url.enabled}")
  private boolean autoHealMainImageUrlEnabled;

  @Value("${mpp.for.wh.enabled}")
  private boolean mppForWhEnabled;

  @Value("${item.code.fetch.size}")
  private int itemCodeFetchSize;

  @Value("${fetch.item.code.from.pcb}")
  private boolean fetchItemCodeFromPcb;

  @Value("${set.default.product.type}")
  private boolean setDefaultProductType;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${override.wholesale.price.activated.for.empty.wholesale.price}")
  private boolean overrideWholesalePriceActivatedSwitch;

  @Value("${fetch.L4.without.mfd.update.pp}")
  private boolean fetchL4WithOutMFDFilterForUpdatePP;

  @Value("${delete.pickup.point.new.flow}")
  private boolean deletePickupPointNewFlow;

  @Value("${inventory.batch.update.delete.pp.code}")
  private boolean inventoryBatchUpdateDeletePpCode;

  @Value("${inventory.batch.update.insert.batch.size}")
  private int inventoryBatchUpdateInsertBatchSize;

  @Value("${inventory.batch.update.delete.batch.size}")
  private int inventoryBatchUpdateDeleteBatchSize;

  @Value("${inventory.batch.update.size}")
  private int inventoryBatchUpdateSize;

  @Value("${fetch.l4.based.on.mfd.in.need.revision}")
  private boolean fetchL4BasedOnMfdInNrFlow;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${populate.null.for.wholesale.price.activated}")
  private boolean populateNullForWholesalePriceActivated;

  @Value("${value.type.addition.for.defining.attributes}")
  private boolean valueTypeAdditionForDefiningAttributes;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${bopis.category.validation.for.merchant.types}")
  private String bopisCategoryValidationMerchantTypes;

  @Value("${bopis.category.restriction.feature.switch}")
  private boolean bopisCategoryRestrictionFeatureEnabled;

  @Value("${faas.feature.switch}")
  private boolean faasFeatureSwitch;


  @Override
  public ProductL3DetailsResponse getL3ProductDetailsByProductSku(String storeId, String productSku,
      boolean isNeedCorrection, boolean fullFetch, boolean concatenateValueWithValueType) throws Exception {
    LOGGER.info("Get L3 Product Details by ProductSku : " + "{}, isNeedCorrection : {}", productSku,
      isNeedCorrection);
    String categoryCode, merchantCode, itemSku, productCode;
    ProductL3Response productL3Response = new ProductL3Response();
    ProductDetailResponse productDetailResponse = new ProductDetailResponse();
    ProductBusinessPartner productBusinessPartner = new ProductBusinessPartner();
    ProductCollection productCollection = new ProductCollection();

    if (isNeedCorrection) {
      productCode = productCollectionRepository.getProductCodeByGdnSku(productSku);
      productDetailResponse =
        productOutbound.getProductDetailByProductCode(productCode, true, false);
      productBusinessPartner = productBusinessPartnerRepository.findFirstByGdnProductSku(productSku);
      categoryCode = productBusinessPartner.getCategoryCode();
      merchantCode = productBusinessPartner.getBusinessPartnerId();
      itemSku = Optional.ofNullable(productBusinessPartner.getProductItemBusinessPartners())
        .orElse(new ArrayList<>()).stream().findFirst().get().getGdnProductItemSku();
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
    }
    List<CategoryResponse> categoriesData = null;
    List<ProductLevel3Logistics> productLevel3Logistics = null;
    ProfileResponse profileResponse = null;
    categoriesData = this.categoryRepository.findHierarchyByCategoryCode(categoryCode);
    productLevel3Logistics = new ArrayList<>();
    profileResponse = this.businessPartnerRepository.filterDetailByBusinessPartnerCode(merchantCode);
    if (StringUtils.isNotBlank(profileResponse.getCompany().getMerchantDeliveryType())) {
      productLevel3Logistics = productLevel3LogisticsService
        .findLogisticsByItemSku(itemSku, merchantCode,
          profileResponse.getCompany().getMerchantDeliveryType());
    }

    if (isNeedCorrection) {
      return generateProductLevel3DetailByProductDetailResponse(productDetailResponse,
        categoriesData, productLevel3Logistics, profileResponse, productBusinessPartner, productSku,
        productCode, productCollection, concatenateValueWithValueType);
    } else {
      return generateProductLevel3Detail(productL3Response, categoriesData, productLevel3Logistics,
        profileResponse, productCollection, concatenateValueWithValueType);
    }
  }

  @Override
  public Page<ItemPickupPointListingL3Response> getItemPickupPointL3Listing(String storeId, String username,
      String requestId, int page, int size, ItemPickupPointListingL3Request itemPickupPointListingL3Request,
      boolean onlyDefaultViewConfig, boolean concatenateValueWithValueType, boolean needInventoryData) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemPickupPointListingL3Request.getBusinessPartnerCode()),
        ErrorMessages.BUSINESS_PARTNER_CODE_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemPickupPointListingL3Request.getProductSku()),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(itemPickupPointListingL3Request.getProductSku()
            .startsWith(itemPickupPointListingL3Request.getBusinessPartnerCode()),
        String.format(ErrorMessages.PRODUCT_SHOULD_BELONG_TO_THE_MERCHANT,
            itemPickupPointListingL3Request.getProductSku(), itemPickupPointListingL3Request.getBusinessPartnerCode()));

    itemPickupPointListingL3Request.setFbbSortRequired(isFbbSortRequired(itemPickupPointListingL3Request.getBusinessPartnerCode()));

    Page<ItemPickupPointListingResponse> itemPickupPointListingResponsePage;
    if (!itemPickupPointListingL3Request.isNeedCorrection()) {
      itemPickupPointListingResponsePage =
          getItemPickupPointListForActiveProducts(storeId, username, requestId, page, size,
              itemPickupPointListingL3Request);
    } else {
      itemPickupPointListingResponsePage =
          getItemPickupPointListForNeedCorrectionProducts(storeId, page, size, itemPickupPointListingL3Request);
    }

    if (CollectionUtils.isEmpty(itemPickupPointListingResponsePage.getContent())) {
      return new PageImpl<>(new ArrayList<>(), PageRequest.of(page, size), 0);
    }
    List<String> skuCodes =
        itemPickupPointListingResponsePage.getContent().stream().map(ItemPickupPointListingResponse::getSkuCode)
            .distinct().collect(Collectors.toList());
    Map<String, ItemImageResponse> productItemResponseMap =
        getProductItemResponseBySkuCode(itemPickupPointListingResponsePage.getContent(), skuCodes);
    if (!itemPickupPointListingL3Request.isNeedCorrection() && autoHealMainImageUrlEnabled) {
      autoHealMainImageUrl(itemPickupPointListingResponsePage.getContent(), skuCodes, productItemResponseMap);
    }
    Map<String, ProductCollection> rejectFlagMap =
        getProductCollectionByProductCodes(storeId, itemPickupPointListingResponsePage.getContent());
    Map<String, List<ProductItemWholesalePriceResponse>> productItemWholesalePriceMap =
        getProductItemWholeSalePriceByItemSkuAndPickupPointCode(storeId, rejectFlagMap.values().iterator().next(),
            itemPickupPointListingResponsePage.getContent());
    Map<String, ProductLevel3Inventory> productLevel3InventoryMap = new HashMap<>();
    if (needInventoryData) {
      productLevel3InventoryMap =
          getProductLevel3InventoryByItemSkuAndPickupPointCode(storeId, rejectFlagMap.values().iterator().next(),
              itemPickupPointListingResponsePage.getContent());
    }
    Map<String, String> valueAndValueTypeMap = new HashMap<>();
    if (concatenateValueWithValueType && valueTypeAdditionForDefiningAttributes) {
      ProductAndAttributeDetailResponse productAttributesByProductId =
          productOutbound.getProductAttributesByProductId(rejectFlagMap.values().iterator().next().getProductId());
      ValueTypeUtil.getValueAndValueTypeMap(productAttributesByProductId, valueAndValueTypeMap);
    }
    Map<String, CampaignPriceSkuResponse> campaignPriceSkuResponseMap =
        !itemPickupPointListingL3Request.isNeedCorrection() ?
            getCampaignPriceByItemSkuAndPickupPointCode(itemPickupPointListingResponsePage.getContent()) :
            new HashMap<>();
    return new PageImpl<>(
        ResponseHelper.toItemPickupPointListingL3Response(itemPickupPointListingResponsePage.getContent(),
            productItemResponseMap, rejectFlagMap, productItemWholesalePriceMap, productLevel3InventoryMap,
            campaignPriceSkuResponseMap, onlyDefaultViewConfig, setDefaultProductType,
            overrideWholesalePriceActivatedSwitch, cncForWarehouseFeatureSwitch, valueAndValueTypeMap,
            sizeChartValueTypeDelimiter, populateNullForWholesalePriceActivated), PageRequest.of(page, size),
        itemPickupPointListingResponsePage.getTotalElements());
  }

  public void autoHealMainImageUrl(List<ItemPickupPointListingResponse> itemPickupPointListingResponseList,
      List<String> skuCodes, Map<String, ItemImageResponse> productItemResponseMap) {
    String productCode = StringUtils.EMPTY;
    Map<String, String> itemCodeAndMainImageUrlMap = itemPickupPointListingResponseList.stream().collect(
        Collectors.toMap(ItemPickupPointListingResponse::getSkuCode,
            response -> Objects.requireNonNullElse(response.getMainImageUrl(), StringUtils.EMPTY),
            (oldValue, newValue) -> oldValue));
    Map<String, String> itemCodeAndMainImageUrlMapFromPCB = productItemResponseMap.values().stream()
        .collect(Collectors.toMap(ItemImageResponse::getItemCode, this::getMainImageUrlFromItemImageResponse));
    for (String skuCode : skuCodes) {
      String mainImageXProduct = itemCodeAndMainImageUrlMap.getOrDefault(skuCode, StringUtils.EMPTY);
      String mainImagePcb = itemCodeAndMainImageUrlMapFromPCB.get(skuCode);
      if (StringUtils.isBlank(mainImageXProduct) || !mainImageXProduct.equals(mainImagePcb)) {
        productCode = skuCode.substring(0, StringUtils.ordinalIndexOf(skuCode, Constants.HYPHEN, ORDINAL_INDEX));
        break;
      }
    }
    if (StringUtils.isNotBlank(productCode)) {
      xProductOutbound.generateProductScoreByProductSkuOrProductCode(null, productCode, false);
    }
  }

  public String getMainImageUrlFromItemImageResponse(ItemImageResponse itemImageResponse) {
    return Optional.ofNullable(itemImageResponse.getImageResponses()).orElse(Collections.emptyList()).stream().filter(
        imageResponse -> imageResponse.isActive() && imageResponse.isMainImage() && !imageResponse.getLocationPath()
            .contains(RESIZE)).map(ImageResponse::getLocationPath).findFirst().orElse(StringUtils.EMPTY);
  }

  @Override
  public Page<ProductLevel3SummaryResponse> getItemPickupPointByProductSkus(int page, int size,
    List<String> productSkuList, String businessPartnerCode, boolean onlineOrCnc,
      List<String> accessiblePickupPoints)
    throws Exception {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(productSkuList),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    ItemPickupPointSummaryRequest itemPickupPointSummaryRequest =
      RequestHelper.toItemPickupPointSummaryRequest(businessPartnerCode, productSkuList,
          onlineOrCnc, accessiblePickupPoints);
    Page<ItemResponseV2> itemPickupPointSummary =
      xProductOutbound.getItemPickupPointSummary(page, size, itemPickupPointSummaryRequest);
    if (CollectionUtils.isEmpty(itemPickupPointSummary.getContent())) {
      return new PageImpl<>(Collections.emptyList(), PageRequest.of(page, size), 0);
    }
    List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOS = itemPickupPointSummary.getContent().stream()
      .map(RequestHelper::toInventoryDetailInfoRequestDTO).collect(Collectors.toList());
    Map<String, ProductLevel3Inventory> productLevel3InventoryMap = Optional.ofNullable(
        productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
          inventoryDetailInfoRequestDTOS)).orElse(new ArrayList<>()).stream().filter(Objects::nonNull)
      .collect(Collectors.toMap(productLevel3Inventory -> CommonUtils.getItemSkuAndPickupPointKey(
          productLevel3Inventory.getWebItemSku(), productLevel3Inventory.getWebPickupPointCode()),
        Function.identity()));
    return new PageImpl<>(itemPickupPointSummary.getContent().stream().map(
      itemResponseV2 -> ConverterUtil.toProductLevel3SummaryResponse(itemResponseV2,
        productLevel3InventoryMap.getOrDefault(
          CommonUtils.getItemSkuAndPickupPointKey(itemResponseV2.getItemSku(),
            itemResponseV2.getPickUpPointCode()), null))).collect(Collectors.toList()),
      PageRequest.of(page, size), itemPickupPointSummary.getTotalElements());
  }

  @Transactional(readOnly = false, rollbackFor = Exception.class)
  @Override
  public List<DeleteInProgressL5Response> deleteInProgressL5ForDeletePickupPoint(String storeId,
      ItemSkuPickupPointRequest itemSkuPickupPointRequest) throws Exception {
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(itemSkuPickupPointRequest.getProductSku()),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(!StringUtils.isEmpty(itemSkuPickupPointRequest.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_NULL);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkuPickupPointRequest.getItemSkuList()),
        ErrorMessages.ITEM_SKU_LIST_MUST_NOT_BE_EMPTY);
    List<DeleteItemPickupPointResponse> deleteActiveL5PPCodeResponse;
    List<AuditTrailDto> auditTrailDtoList = new ArrayList<>();
    List<String> toDeleteL4 = new ArrayList<>();
    List<String> notToDeleteL4 = new ArrayList<>();

    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(itemSkuPickupPointRequest.getProductSku());
    ProductCollection productCollection = null;
    if (Objects.nonNull(productBusinessPartner)) {
      productCollection = productCollectionRepository.findByStoreIdAndProductIdAndMarkForDeleteFalse(storeId,
          productBusinessPartner.getProductId());
    }

    // get default status as active if it is null
    String productCollectionState =
        Optional.ofNullable(productCollection).map(ProductCollection::getState).orElse(Constants.ACTIVE);
    String productPresentState =
        Optional.ofNullable(productBusinessPartner).map(ProductBusinessPartner::getState).orElse(Constants.ACTIVE);

    //call x-product with product sku in request to check if product exists
    ProductExistenceAndPreOrderDTO productData =
        getProductExistenceAndPreOrder(itemSkuPickupPointRequest.getProductSku());

    if (productPresentState.equalsIgnoreCase(Constants.NEED_CORRECTION) || productPresentState.equalsIgnoreCase(
        Constants.IN_PROGRESS_STATE) || productPresentState.equalsIgnoreCase(Constants.ACTIVE)) {
      validateL4ToBeDeleted(storeId, toDeleteL4, notToDeleteL4, itemSkuPickupPointRequest.getItemSkuList());
      if (!toDeleteL4.isEmpty()) {
        productItemBusinessPartnerService.deleteItemsPickupPointDelete(storeId,
            itemSkuPickupPointRequest.getPickupPointCode(), toDeleteL4);
        if (!Constants.ACTIVE.equals(productCollectionState)) {
          for (String itemSku : toDeleteL4) {
            auditTrailDtoList.add(new AuditTrailDto(itemSkuPickupPointRequest.getBusinessPartnerCode(), itemSku,
                UpdateProductActivity.PICKUP_POINT_DELETED.name(), itemSkuPickupPointRequest.getPickupPointCode(),
                StringUtils.EMPTY, null, itemSkuPickupPointRequest.getProductSku(),
                productBusinessPartner.getProductName(), Constants.HYPHEN, false));
          }
        }
      }
    }

    PickupPointFilterRequest pickupPointFilterRequest = new PickupPointFilterRequest();
    pickupPointFilterRequest.setBusinessPartnerCode(itemSkuPickupPointRequest.getBusinessPartnerCode());
    pickupPointFilterRequest.setFbbActivated(false);
    pickupPointFilterRequest.setWaitingDeletion(false);
    pickupPointFilterRequest.setSortedBy(Constants.DEFAULT_ADDRESS);
    pickupPointFilterRequest.setSortDirection(SolrConstants.DESC);
    List<PickupPointResponse> pickupPointResponseList = new ArrayList<>();
    boolean completeFail = false;
    String defaultPickupPoint = null;
    boolean isFbbPickupPoint = false;
    String reason = StringUtils.EMPTY;
    ProfileResponse profileResponse = null;
    try {
      profileResponse =
          businessPartnerRepository.filterDetailByBusinessPartnerCode(itemSkuPickupPointRequest.getBusinessPartnerCode());
      if (com.gdn.mta.product.util.CommonUtils.isInternalSeller(profileResponse, Constants.INTERNAL_SELLERS)) {
        pickupPointFilterRequest.setFbbActivated(true);
      }
      pickupPointResponseList =
          businessPartnerRepository.filterPickupPointsByPickupPointRequestForOne(pickupPointFilterRequest);
      defaultPickupPoint = pickupPointResponseList.get(0).getCode();
      isFbbPickupPoint = pickupPointResponseList.get(0).isFbbActivated();
    } catch (Exception e) {
      completeFail = true;
      reason = REASON;
    }

    GdnPreconditions.checkArgument(
        !StringUtils.equals(defaultPickupPoint, itemSkuPickupPointRequest.getPickupPointCode()),
        ErrorMessages.DEFAULT_PICKUPPOINT_SAME_AS_PICKUP_POINT);

    Map<String, String> itemSkusMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(notToDeleteL4) && !completeFail) {
      updatePickupPointForL4(storeId, defaultPickupPoint, isFbbPickupPoint, notToDeleteL4,
          itemSkuPickupPointRequest.getPickupPointCode(), pickupPointResponseList.get(0),
          itemSkuPickupPointRequest.getProductSku(), productBusinessPartner,
          itemSkuPickupPointRequest.getBusinessPartnerCode(), profileResponse, productData);
      if (!Constants.ACTIVE.equals(productCollectionState)) {
        for (String itemSku : notToDeleteL4) {
          auditTrailDtoList.add(new AuditTrailDto(itemSkuPickupPointRequest.getBusinessPartnerCode(), itemSku,
              UpdateProductActivity.PICK_POINT_CODE.name(), itemSkuPickupPointRequest.getPickupPointCode(),
              defaultPickupPoint, null, productBusinessPartner.getGdnProductSku(),
              productBusinessPartner.getProductName(), itemSkuPickupPointRequest.getPickupPointCode(), false));
        }
      }
    }

    if (Constants.ACTIVE.equals(productCollectionState) && productData.isProductExistInXproduct()) {
      LOGGER.info("Deleting active products in xprod for ppcode : {} ", itemSkuPickupPointRequest.getPickupPointCode());
      DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
      deleteItemPickupPointRequest.setDefaultPickupPointCode(defaultPickupPoint);
      BeanUtils.copyProperties(itemSkuPickupPointRequest, deleteItemPickupPointRequest);
      deleteItemPickupPointRequest.setItemSkus(itemSkuPickupPointRequest.getItemSkuList());
      deleteItemPickupPointRequest.setDefaultPickupPointFbbActive(isFbbPickupPoint);
      deleteActiveL5PPCodeResponse =
          xProductOutbound.deleteActiveItemPickupPointByPickupPointCode(deleteItemPickupPointRequest);
      populateFailedActiveL5Delete(notToDeleteL4, deleteActiveL5PPCodeResponse, itemSkusMap);
    } else if (productPresentState.equalsIgnoreCase(Constants.DELETED_STATE)) {
      LOGGER.info("Product is already deleted {} ", itemSkuPickupPointRequest.getProductSku());
    }
    LOGGER.info("Delete pp code successful for product sku : {}, ppCode :{} ",
        itemSkuPickupPointRequest.getProductSku(), itemSkuPickupPointRequest.getPickupPointCode());
    if (CollectionUtils.isNotEmpty(auditTrailDtoList)) {
      AuditTrailListResponse auditTrailListResponse = new AuditTrailListResponse();
      auditTrailListResponse.setAuditTrailResponseList(auditTrailDtoList);
      auditTrailListResponse.setAccessChannel(Constants.PBP);
      auditTrailListResponse.setChangedBy(Constants.DEFAULT_USERNAME);
      auditTrailListResponse.setClientId(Constants.DEFAULT_CLIENT_ID);
      auditTrailListResponse.setRequestId(Constants.DEFAULT_REQUEST_ID);
      auditTrailListResponse.setUpdateDirectly(true);
      kafkaProducer.send(ProductDomainEventName.PRODUCT_SKU_UPDATE_HISTORY, auditTrailListResponse);
    }
    return populateFailedL5ResponseList(notToDeleteL4, itemSkuPickupPointRequest.getPickupPointCode(), itemSkusMap,
        reason, defaultPickupPoint);
  }

  private ProductExistenceAndPreOrderDTO getProductExistenceAndPreOrder(String productSku) {
    List<ProductBasicResponse> productBasicResponseList =
        xProductOutbound.getProductBasicDetails(Collections.singletonList(productSku));
    ProductBasicResponse productBasicResponse = new ProductBasicResponse();
    Map<String, ProductBasicResponse> productBasicResponseMap =
        Optional.ofNullable(productBasicResponseList).orElse(new ArrayList<>()).stream()
            .collect(Collectors.toMap(ProductBasicResponse::getProductSku, Function.identity(), (v1, v2) -> v2));
    if (productBasicResponseMap.containsKey(productSku)) {
      productBasicResponse = productBasicResponseMap.get(productSku);
    }
    
    return ProductExistenceAndPreOrderDTO.builder()
        .productExistInXproduct(productBasicResponse.isProductExists())
        .preOrder(productBasicResponse.getPreOrder())
        .build();
  }

  private void populateFailedActiveL5Delete(List<String> toDeleteL4,
      List<DeleteItemPickupPointResponse> deleteActiveL5PPCodeResponse, Map<String, String> itemSkusMap) {
    for (DeleteItemPickupPointResponse response : deleteActiveL5PPCodeResponse) {
      itemSkusMap.put(response.getItemSku(), response.getNewPickupPointCode());
      toDeleteL4.add(response.getItemSku());
    }
  }

  private void updatePickupPointForL4(String storeId, String defaultPickupPointCode,
      boolean isFbbPickupPoint, List<String> updatePickupPointsL4, String pickupPointCode,
      PickupPointResponse pickupPointResponse, String productSku,
      ProductBusinessPartner productBusinessPartner, String businessPartnerCode,
      ProfileResponse profileResponse, ProductExistenceAndPreOrderDTO productData)
      throws Exception {
    LOGGER.info("updated pickupPoints for l4 with default pickupPoint : {} for ppCode : {} ", defaultPickupPointCode,
        pickupPointCode);
    Map<String, String> itemXNewPickUpPointMap = new HashMap<>();
    Map<String, String> itemXOldPickUpPointMap = new HashMap<>();
    List<ProductItemBusinessPartner> productItemBusinessPartnerList = new ArrayList<>();
    int cncActiveGettingDeleted = 0;
    int fbbActiveGettingDeleted = 0;
    Map<String, ProductItemBusinessPartner> L5CodeAndItemBusinessPartnerMap = new HashMap<>();
    Map<String, String> productItemIdAndItemSkuMap = new HashMap<>();
    Map<String, String> itemSkuAndItemCodeMap = new HashMap<>();
    List<ItemBasicDetailV2Response> itemBasicDetailsByItemSkus = new ArrayList<>();
    for (String itemSku : updatePickupPointsL4) {
      ProductItemBusinessPartner productItemBusinessPartner = fetchL4WithOutMFDFilterForUpdatePP ?
        this.productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointId(storeId, itemSku, pickupPointCode) :
        this.productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointIdAndMarkForDeleteFalse(
          storeId, itemSku, pickupPointCode);
      if (Objects.nonNull(productItemBusinessPartner)) {
        L5CodeAndItemBusinessPartnerMap.put(itemSku + pickupPointCode, productItemBusinessPartner);
        productItemIdAndItemSkuMap.putIfAbsent(productItemBusinessPartner.getProductItemId(),
            productItemBusinessPartner.getGdnProductItemSku());
      }
    }
    itemSkuAndItemCodeMap =
        getItemSkuAndItemCodeMap(updatePickupPointsL4, profileResponse, itemBasicDetailsByItemSkus,
            itemSkuAndItemCodeMap, pickupPointCode, productItemIdAndItemSkuMap,
            productData.isProductExistInXproduct());
    for (String itemSku : updatePickupPointsL4) {
      itemXOldPickUpPointMap.put(itemSku, pickupPointCode);
      ProductItemBusinessPartner productItemBusinessPartner =
          L5CodeAndItemBusinessPartnerMap.getOrDefault(itemSku + pickupPointCode, null);
      if (Objects.nonNull(productItemBusinessPartner)) {
        if (productItemBusinessPartner.isCncActivated()) {
          cncActiveGettingDeleted = cncActiveGettingDeleted + 1;
        }
        if (productItemBusinessPartner.isFbbActive() && !isFbbPickupPoint) {
          fbbActiveGettingDeleted = fbbActiveGettingDeleted + 1;
        }
        if (!deletePickupPointNewFlow) {
          productItemBusinessPartner.setPickupPointId(defaultPickupPointCode);
        }
        productItemBusinessPartner.setFbbActive(isFbbPickupPoint);
        deletePickupPointNewFlow(storeId, defaultPickupPointCode, isFbbPickupPoint, pickupPointCode, itemSku,
            productItemBusinessPartner, productItemBusinessPartnerList);
        productItemBusinessPartner.setBuyable(false);
        productItemBusinessPartner.setCncActivated(false);
        productItemBusinessPartner.setDisplay(false);

        productItemBusinessPartnerList.add(productItemBusinessPartner);
      }
      itemXNewPickUpPointMap.put(itemSku, defaultPickupPointCode);
      if (productData.isProductExistInXproduct() && !inventoryBatchUpdateDeletePpCode) {
        if (ConverterUtil.isMPPEnabled(profileResponse, mppAllowedSellers)) {
          insertAndDeleteInventoryForMppSeller(defaultPickupPointCode, pickupPointCode,
              pickupPointResponse, productSku, profileResponse, itemSku, itemSkuAndItemCodeMap,
              productData.getPreOrder());
          } else {
            LOGGER.info("Call to x-inventory to update inventory for ppCode : {} is done", pickupPointCode);
            WebInventoryUpdatePickupPointResponseDTO response =
                inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(),
                    ConverterUtil.toWebInventoryUpdatePickupPointRequestDTO(businessPartnerCode,
                        itemXNewPickUpPointMap, itemXOldPickUpPointMap));
          }
        }
      productLevel3ServiceBean.updateWholesaleEntriesOnPickupPointChange(productBusinessPartner.getStoreId(), itemXOldPickUpPointMap,
          itemXNewPickUpPointMap, productBusinessPartner.getState());
    }
    if (productData.isProductExistInXproduct() && inventoryBatchUpdateDeletePpCode) {
      batchedInventoryOperation(defaultPickupPointCode, updatePickupPointsL4, pickupPointCode,
          pickupPointResponse, productSku, businessPartnerCode, profileResponse,
          itemSkuAndItemCodeMap, itemXNewPickUpPointMap, itemXOldPickUpPointMap,
          productData.getPreOrder());
    }
    setCncAndFbbAtL3Level(productBusinessPartner, cncActiveGettingDeleted, fbbActiveGettingDeleted);
    if (CollectionUtils.isNotEmpty(productItemBusinessPartnerList)) {
      this.productItemBusinessPartnerRepository.saveAll(productItemBusinessPartnerList);
    }
  }

  private void deletePickupPointNewFlow(String storeId, String defaultPickupPointCode, boolean isFbbPickupPoint,
      String pickupPointCode, String itemSku, ProductItemBusinessPartner productItemBusinessPartner,
      List<ProductItemBusinessPartner> productItemBusinessPartnerList) {
    if (deletePickupPointNewFlow) {
      ProductItemBusinessPartner productItemBusinessPartnerNew = Optional.ofNullable(
          this.productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndPickupPointId(storeId, itemSku,
              defaultPickupPointCode)).orElse(new ProductItemBusinessPartner());
      BeanUtils.copyProperties(productItemBusinessPartner, productItemBusinessPartnerNew, "id", "version",
          "pickupPointId", "createdBy", "createdDate");
      productItemBusinessPartner.setPickupPointId(pickupPointCode);
      productItemBusinessPartner.setMarkForDelete(true);
      productItemBusinessPartner.setFbbActive(false);
      productItemBusinessPartnerNew.setMarkForDelete(false);
      productItemBusinessPartnerNew.setFbbActive(isFbbPickupPoint);
      productItemBusinessPartnerNew.setPickupPointId(defaultPickupPointCode);
      productItemBusinessPartnerList.add(productItemBusinessPartnerNew);
    }
  }

  private void batchedInventoryOperation(String defaultPickupPointCode,
      List<String> updatePickupPointsL4, String pickupPointCode,
      PickupPointResponse pickupPointResponse, String productSku, String businessPartnerCode,
      ProfileResponse profileResponse, Map<String, String> itemSkuAndItemCodeMap,
      Map<String, String> itemXNewPickUpPointMap, Map<String, String> itemXOldPickUpPointMap,
      PreOrderDTO preOrderDTO) throws Exception {
    if (ConverterUtil.isMPPEnabled(profileResponse, mppAllowedSellers)) {
      addAndDeleteInventory(defaultPickupPointCode, updatePickupPointsL4, pickupPointCode,
          pickupPointResponse, productSku, profileResponse, itemSkuAndItemCodeMap, preOrderDTO);
    } else {
      updatePickupPointCodeInInventory(businessPartnerCode, itemXNewPickUpPointMap, itemXOldPickUpPointMap);
    }
  }

  private void updatePickupPointCodeInInventory(String businessPartnerCode, Map<String, String> itemXNewPickUpPointMap,
      Map<String, String> itemXOldPickUpPointMap) throws Exception {
    ListRequestDTO<WebInventoryUpdatePickupPointRequestDTO> webInventoryUpdatePickupPointRequestDTO =
        ConverterUtil.toWebInventoryUpdatePickupPointRequestDTO(businessPartnerCode, itemXNewPickUpPointMap,
            itemXOldPickUpPointMap);
    List<List<WebInventoryUpdatePickupPointRequestDTO>> updateInventoryList =
        Lists.partition(webInventoryUpdatePickupPointRequestDTO.getList(), inventoryBatchUpdateSize);
    for (List<WebInventoryUpdatePickupPointRequestDTO> updateInventoryBatch : updateInventoryList) {
      LOGGER.info("Calling x-inventory to update inventory for data : {} ", updateInventoryBatch);
      inventoryOutbound.updatePickupPoint(generateMandatoryRequestParam(),
          new ListRequestDTO<>(updateInventoryBatch));
    }
  }

  private void addAndDeleteInventory(String defaultPickupPointCode,
      List<String> updatePickupPointsL4, String pickupPointCode,
      PickupPointResponse pickupPointResponse, String productSku, ProfileResponse profileResponse,
      Map<String, String> itemSkuAndItemCodeMap, PreOrderDTO preOrderDTO) throws Exception {
    List<ProductLevel3Inventory> productLevel3InventoryList = new ArrayList<>();
    List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList =
        new ArrayList<>();
    getInsertAndDeleteData(defaultPickupPointCode, updatePickupPointsL4, pickupPointCode,
        pickupPointResponse, productSku, profileResponse, itemSkuAndItemCodeMap,
        productLevel3InventoryList, webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList,
        preOrderDTO);
    insertInventoryInBatch(defaultPickupPointCode, pickupPointCode, productLevel3InventoryList);
    deleteInventoryInBatch(defaultPickupPointCode, pickupPointCode, webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList);
  }

  private void deleteInventoryInBatch(String defaultPickupPointCode, String pickupPointCode,
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList)
      throws Exception {
    List<List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO>> deleteInventoryBatch =
        Lists.partition(webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList, inventoryBatchUpdateDeleteBatchSize);
    for (List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> deleteInventoryList : deleteInventoryBatch) {
      LOGGER.info("Calling x-inventory to delete inventory for new ppCode : {} for ppCode : {} and itemSkus: {} ",
          defaultPickupPointCode, pickupPointCode,
          deleteInventoryList.stream().map(WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO::getWebItemSku)
              .collect(Collectors.toList()));
      productLevel3InventoryService.deleteByItemSkuAndPickupPointCode(deleteInventoryList);
    }
  }

  private void insertInventoryInBatch(String defaultPickupPointCode, String pickupPointCode,
      List<ProductLevel3Inventory> productLevel3InventoryList) throws Exception {
    List<List<ProductLevel3Inventory>> insertInventoryBatch =
        Lists.partition(productLevel3InventoryList, inventoryBatchUpdateInsertBatchSize);
    for (List<ProductLevel3Inventory> inventoryList : insertInventoryBatch) {
      LOGGER.info("Calling x-inventory to insert inventory for new ppCode : {} for ppCode : {} and itemSkus: {} ",
          defaultPickupPointCode, pickupPointCode,
          inventoryList.stream().map(ProductLevel3Inventory::getWebItemSku).collect(Collectors.toList()));
      productLevel3InventoryService.insertInventory(inventoryList);
    }
  }

  private void getInsertAndDeleteData(String defaultPickupPointCode,
      List<String> updatePickupPointsL4, String pickupPointCode,
      PickupPointResponse pickupPointResponse, String productSku, ProfileResponse profileResponse,
      Map<String, String> itemSkuAndItemCodeMap,
      List<ProductLevel3Inventory> productLevel3InventoryList,
      List<WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO> webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList,
      PreOrderDTO preOrderDTO) {
    for (String itemSku : updatePickupPointsL4) {
      productLevel3InventoryList.add(
          getProductLevel3Inventory(defaultPickupPointCode, pickupPointResponse, productSku,
              profileResponse, itemSku, itemSkuAndItemCodeMap, preOrderDTO));
      webInventoryDeleteByWebItemSkuAndPickupPointCodeDTOList.add(
          getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO(pickupPointCode, profileResponse, itemSku));
    }
  }

  private Map<String, String> getItemSkuAndItemCodeMap(List<String> updatePickupPointsL4, ProfileResponse profileResponse,
      List<ItemBasicDetailV2Response> itemBasicDetailsByItemSkus, Map<String, String> itemSkuAndItemCodeMap,
      String pickupPointCode, Map<String, String> productItemIdAndItemSkuMap, boolean isProductExistsInXproduct) throws Exception {
    if (isProductExistsInXproduct) {
      if (ConverterUtil.isMPPEnabled(profileResponse, mppAllowedSellers)) {
        itemSkuAndItemCodeMap = getItemSkuAndItemCodeMap(updatePickupPointsL4, itemBasicDetailsByItemSkus, pickupPointCode);
        if (fetchItemCodeFromPcb) {
          Set<String> itemSkusInResponse =
              itemBasicDetailsByItemSkus.stream().map(ItemBasicDetailV2Response::getItemSku)
                  .collect(Collectors.toSet());
          List<String> itemsSkusInRequest = new ArrayList<>(updatePickupPointsL4);
          itemsSkusInRequest.removeAll(itemSkusInResponse);
          if (CollectionUtils.isNotEmpty(itemsSkusInRequest)) {
            List<String> itemIdsToBeFetchedFromPCB = productItemIdAndItemSkuMap.entrySet().stream()
                .filter(entry -> itemsSkusInRequest.contains(entry.getValue())).map(Map.Entry::getKey)
                .collect(Collectors.toList());
            Map<String, String> productItemIdAndSkuCodeMap =
                getProductSkusCodesFromPCB(pickupPointCode, itemIdsToBeFetchedFromPCB);
            if (MapUtils.isNotEmpty(productItemIdAndSkuCodeMap)) {
              itemSkuAndItemCodeMap.putAll(productItemIdAndSkuCodeMap.entrySet().stream().collect(
                  Collectors.toMap(entry -> productItemIdAndItemSkuMap.get(entry.getKey()), Map.Entry::getValue)));
            }
          }
        }
      }
    }
    return itemSkuAndItemCodeMap;
  }

  private Map<String, String> getItemSkuAndItemCodeMap(List<String> updatePickupPointsL4,
      List<ItemBasicDetailV2Response> itemBasicDetailsByItemSkus, String pickupPointCode) {
    Map<String, String> itemSkuAndItemCodeMap;
    LOGGER.info("Fetching itemCode from x-product delete pickup point code : {} ", pickupPointCode);
    List<List<String>> itemSkuList = Lists.partition(updatePickupPointsL4, itemCodeFetchSize);
    for (List<String> itemSkus : itemSkuList) {
      itemBasicDetailsByItemSkus.addAll(
          xProductOutbound.getItemBasicDetailsByItemSkus(true, new SimpleListStringRequest(itemSkus)));
    }
    itemSkuAndItemCodeMap = itemBasicDetailsByItemSkus.stream().collect(
        Collectors.toMap(ItemBasicDetailV2Response::getItemSku, ItemBasicDetailV2Response::getItemCode,
            (a, b) -> b));
    return itemSkuAndItemCodeMap;
  }

  private Map<String, String> getProductSkusCodesFromPCB(String pickupPointCode, List<String> productItemIds)
      throws Exception {
    Map<String, String> productItemIdAndSkuCodeMap = new HashMap<>();
    if (CollectionUtils.isNotEmpty(productItemIds)) {
      List<List<String>> itemIdsList = Lists.partition(productItemIds, itemCodeFetchSize);
      for (List<String> itemIds : itemIdsList) {
        LOGGER.info("Call PCB to get itemCode for ppCode : {} and items : {} ", pickupPointCode, itemIds);
        SimpleStringMapResponse skuCodesByProductItemIds = productRepository.getSkuCodesByProductItemIds(itemIds);
        if (Objects.nonNull(skuCodesByProductItemIds) && MapUtils.isNotEmpty(
            skuCodesByProductItemIds.getMapResponse())) {
          productItemIdAndSkuCodeMap.putAll(skuCodesByProductItemIds.getMapResponse());
        }
      }
    }
    return productItemIdAndSkuCodeMap;
  }

  private void insertAndDeleteInventoryForMppSeller(String defaultPickupPointCode,
      String pickupPointCode, PickupPointResponse pickupPointResponse, String productSku,
      ProfileResponse profileResponse, String itemSku, Map<String, String> itemSkuAndItemCodeMap,
      PreOrderDTO preOrderDTO) throws Exception {
    LOGGER.info("Calling x-inventory to insert inventory for new ppCode : {} for ppCode : {} and itemSku : {} ",
        defaultPickupPointCode, pickupPointCode, itemSku);
    insertInventory(defaultPickupPointCode, pickupPointResponse, productSku, profileResponse,
        itemSku, itemSkuAndItemCodeMap, preOrderDTO);
    LOGGER.info("Calling x-inventory to delete inventory for ppCode : {} and itemSku : {} ", pickupPointCode, itemSku);
    deleteInventory(pickupPointCode, profileResponse, itemSku);
    LOGGER.info("Call to x-inventory to delete inventory for ppCode : {} and itemSku : {} is done", pickupPointCode,
        itemSku);
  }

  private void deleteInventory(String pickupPointCode, ProfileResponse profileResponse, String itemSku)
      throws Exception {
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO =
        getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO(pickupPointCode, profileResponse, itemSku);
    productLevel3InventoryService.deleteByItemSkuAndPickupPointCode(
        Collections.singletonList(webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO));
  }

  private void insertInventory(String defaultPickupPointCode,
      PickupPointResponse pickupPointResponse, String productSku, ProfileResponse profileResponse,
      String itemSku, Map<String, String> itemSkuAndItemCodeMap, PreOrderDTO preOrderDTO)
      throws Exception {
    ProductLevel3Inventory productLevel3Inventory =
        getProductLevel3Inventory(defaultPickupPointCode, pickupPointResponse, productSku,
            profileResponse, itemSku, itemSkuAndItemCodeMap, preOrderDTO);
    productLevel3InventoryService.insertInventory(Collections.singletonList(productLevel3Inventory));
  }

  private static WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO getWebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO(
      String pickupPointCode, ProfileResponse profileResponse, String itemSku) {
    WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO =
        new WebInventoryDeleteByWebItemSkuAndPickupPointCodeDTO();
    webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO.setWebMerchantCode(profileResponse.getBusinessPartnerCode());
    webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO.setPickupPointCode(pickupPointCode);
    webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO.setWebItemSku(itemSku);
    return webInventoryDeleteByWebItemSkuAndPickupPointCodeDTO;
  }

  private ProductLevel3Inventory getProductLevel3Inventory(String defaultPickupPointCode,
      PickupPointResponse pickupPointResponse, String productSku, ProfileResponse profileResponse,
      String itemSku, Map<String, String> itemSkuAndItemCodeMap, PreOrderDTO preOrderDTO) {
    ProductLevel3Inventory productLevel3Inventory = new ProductLevel3Inventory();
    productLevel3Inventory.setWebItemSku(itemSku);
    productLevel3Inventory.setWebPickupPointCode(defaultPickupPointCode);
    productLevel3Inventory.setWebSyncStock(
      com.gdn.mta.product.util.CommonUtils.getSyncStockValueForFASSMerchants(faasFeatureSwitch,
        pickupPointResponse.isFbbActivated(), profileResponse));
    productLevel3Inventory.setWebAvailable(0);
    productLevel3Inventory.setWebMerchantCode(profileResponse.getBusinessPartnerCode());
    productLevel3Inventory.setWarehouseItemSku(itemSkuAndItemCodeMap.get(itemSku));
    productLevel3Inventory.setProductSku(productSku);
    productLevel3Inventory.setFbbPP(pickupPointResponse.isFbbActivated() && mppForWhEnabled);
    productLevel3Inventory.setDistributionPickupPoint(Boolean.TRUE.equals(
        Optional.ofNullable(pickupPointResponse.getFlags()).orElse(new HashMap<>())
            .getOrDefault(Constants.DISTRIBUTION_FLAG_KEY, false)));
    if (Objects.nonNull(preOrderDTO)) {
      com.gdn.mta.product.util.CommonUtils.setPreOrderFields(
          preOrderConfig.isPoQuotaFeatureSwitch(), profileResponse, preOrderDTO.getPreOrderDate(),
          productLevel3Inventory, Constants.ZERO);
    }
    if (ConverterUtil.isPurchaseOrderPurchaseTerm(profileResponse)) {
      productLevel3Inventory.setWarehouseMerchantCode(GdnBaseLookup.DEFAULT_BUSINESS_PARTNER_CODE);
    } else {
      productLevel3Inventory.setWarehouseMerchantCode(profileResponse.getBusinessPartnerCode());
    }
    return productLevel3Inventory;
  }

  public static MandatoryRequestParam generateMandatoryRequestParam() throws Exception {
    return MandatoryRequestParam.generateMandatoryRequestParam(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        GdnMandatoryRequestParameterUtil.getAuthenticator());
  }

  private void setCncAndFbbAtL3Level(ProductBusinessPartner productBusinessPartner, int cncActiveGettingDeleted,
      int fbbActiveGettingDeleted) {
    if (Objects.nonNull(productBusinessPartner)) {
      ProductItemBusinessPartner productItemBusinessPartnerForFbbActive = null;
      ProductItemBusinessPartner productItemBusinessPartnerForCncActive = null;
      if (fbbActiveGettingDeleted > 0) {
        productItemBusinessPartnerForFbbActive =
            productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndFbbActiveAndMarkForDeleteFalse(
                productBusinessPartner.getId(), true);
      }
      if (cncActiveGettingDeleted > 0) {
        productItemBusinessPartnerForCncActive =
            productItemBusinessPartnerRepository.findFirstByProductBusinessPartnerIdAndCncActiveAndMarkForDeleteFalse(
                productBusinessPartner.getId(), true);
      }

      boolean fbbChanged = false;
      boolean cncChanged = false;
      if (Objects.isNull(productItemBusinessPartnerForCncActive)) {
        cncChanged = true;
        productBusinessPartner.setCncActivated(false);
      }
      if (Objects.isNull(productItemBusinessPartnerForFbbActive)) {
        fbbChanged = true;
        productBusinessPartner.setFbbActivated(false);
      }
      if (fbbChanged || cncChanged) {
        productBusinessPartnerRepository.save(productBusinessPartner);
      }
    }
  }

  private void validateL4ToBeDeleted(String storeId, List<String> toDeleteL4, List<String> notToDeleteL4,
      List<String> itemSkuList) {
    for (String itemSku : itemSkuList) {
      if (getL5Count(storeId, itemSku) == 1) {
        notToDeleteL4.add(itemSku);
      } else {
        toDeleteL4.add(itemSku);
      }
    }
  }

  //get l5 count from x-product if in PBP its 0
  private long getL5Count(String storeId, String itemSku) {
    int totalL5CountForItem = productItemBusinessPartnerService.getProductItemCountByItemSku(storeId, itemSku);
    if (totalL5CountForItem == 0) {
      return xProductOutbound.getL5CountByItemSku(itemSku).getValue();
    } else {
      return totalL5CountForItem;
    }
  }

  private List<DeleteInProgressL5Response> populateFailedL5ResponseList(List<String> failedItemSku,
    String pickupPointCode,Map<String,String> itemSkuMap, String reason,String defaultPickupPointCode) {
    List<DeleteInProgressL5Response> responseList = new ArrayList<>();
    for (String itemSku : failedItemSku) {
      responseList.add(DeleteInProgressL5Response.builder().itemSku(itemSku)
          .pickupPointCode(pickupPointCode).reason(reason)
          .newPickupPointCode(itemSkuMap.getOrDefault(itemSku,defaultPickupPointCode)).build());
    }
    return responseList;
  }

  private Map<String, CampaignPriceSkuResponse> getCampaignPriceByItemSkuAndPickupPointCode(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList) throws Exception {
    CampaignPriceResponse campaignPriceResponse = campaignOutbound.getCampaignPriceInfoV2(
        RequestHelper.toCampaignPriceRequestV2(itemPickupPointListingResponseList));
    return Optional.ofNullable(campaignPriceResponse.getItemInfoToPriceResponse()).orElse(new ArrayList<>()).stream()
        .collect(Collectors.toMap(
            campaignPriceSkuResponse -> CommonUtils.getItemSkuAndPickupPointKey(campaignPriceSkuResponse.getItemSku(),
                campaignPriceSkuResponse.getPickUpPointCode()), Function.identity(), (v1, v2) -> v2));
  }

  private Map<String, ProductLevel3Inventory> getProductLevel3InventoryByItemSkuAndPickupPointCode(String storeId,
      ProductCollection productCollection, List<ItemPickupPointListingResponse> itemPickupPointListingResponseList)
      throws Exception {
    if (com.gdn.mta.product.util.CommonUtils.isProductNotActivatedBefore(productCollection)) {
      List<ProductItemBusinessPartner> productItemBusinessPartnerList =
          productItemBusinessPartnerService.getProductItemBusinessPartnerByItemSkuList(storeId,
              itemPickupPointListingResponseList.stream().map(ItemPickupPointListingResponse::getItemSku).distinct()
                  .collect(Collectors.toList()));
      return productItemBusinessPartnerList.stream().map(productItemBusinessPartner -> ProductLevel3Inventory.builder()
          .webItemSku(productItemBusinessPartner.getGdnProductItemSku())
          .webPickupPointCode(productItemBusinessPartner.getPickupPointId())
          .webAvailable(productItemBusinessPartner.getStock()).webMinAlert(productItemBusinessPartner.getMinimumStock())
          .build()).collect(Collectors.toMap(
          productLevel3Inventory -> CommonUtils.getItemSkuAndPickupPointKey(productLevel3Inventory.getWebItemSku(),
              productLevel3Inventory.getWebPickupPointCode()), Function.identity(), (v1, v2) -> v2));
    } else {
      List<InventoryDetailInfoRequestDTO> inventoryDetailInfoRequestDTOList =
          itemPickupPointListingResponseList.stream().map(RequestHelper::inventoryDetailInfoRequestDTO)
              .collect(Collectors.toList());
      List<ProductLevel3Inventory> productLevel3InventoryList =
          productLevel3InventoryService.findInventoryByBusinessPartnerCodeAndItemSkuAndPickupPointCode(
              inventoryDetailInfoRequestDTOList);
      return productLevel3InventoryList.stream().collect(Collectors.toMap(
          productLevel3Inventory -> CommonUtils.getItemSkuAndPickupPointKey(productLevel3Inventory.getWebItemSku(),
              productLevel3Inventory.getWebPickupPointCode()), Function.identity(), (v1, v2) -> v2));
    }
  }

  private Map<String, List<ProductItemWholesalePriceResponse>> getProductItemWholeSalePriceByItemSkuAndPickupPointCode(
      String storeId, ProductCollection productCollection,
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList)
      throws Exception {
    Map<String, List<ProductItemWholesalePriceResponse>> productItemWholesalePriceMap = new HashMap<>();
    if (WorkflowStates.NEED_CORRECTION.getValue().equals(productCollection.getState())) {
      List<ItemPickupPointDto> itemPickupPointDtoList = itemPickupPointListingResponseList.stream()
        .map(itemPickupPointListingResponse -> ItemPickupPointDto.builder()
          .itemSku(itemPickupPointListingResponse.getItemSku())
          .pickupPointCode(itemPickupPointListingResponse.getPickUpPointCode()).build())
        .collect(Collectors.toList());
      List<ProductItemWholesalePrice> productItemWholesalePriceList =
          productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(storeId, itemPickupPointDtoList);
      for (ProductItemWholesalePrice productItemWholesalePrice : productItemWholesalePriceList) {
        productItemWholesalePriceMap.put(CommonUtils.getItemSkuAndPickupPointKey(productItemWholesalePrice.getItemSku(),
                    productItemWholesalePrice.getPickupPointCode()),
            mapperUtil.mapStringToResponse(productItemWholesalePrice.getWholesaleRules()));
      }
    } else {
      List<WholesalePriceSkuResponse> wholesalePriceSkuResponseList = productPricingOutbound.getWholesalePriceListV2(
          RequestHelper.toWholesalePriceSkuDetailListRequest(itemPickupPointListingResponseList));
      for (WholesalePriceSkuResponse wholesalePriceSkuResponse : wholesalePriceSkuResponseList) {
        productItemWholesalePriceMap.put(CommonUtils.getItemSkuAndPickupPointKey(wholesalePriceSkuResponse.getItemSku(),
                wholesalePriceSkuResponse.getPickUpPointCode()), wholesalePriceSkuResponse.getWholesaleRules().entrySet().stream()
            .map(entry -> new ProductItemWholesalePriceResponse(entry.getKey(), entry.getValue()))
            .collect(Collectors.toList()));
      }
    }
    return productItemWholesalePriceMap;
  }

  private Map<String, ProductCollection> getProductCollectionByProductCodes(String storeId,
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList) throws Exception {
    List<String> productCodes =
        itemPickupPointListingResponseList.stream().map(ItemPickupPointListingResponse::getProductCode).distinct()
            .collect(Collectors.toList());
    List<ProductCollection> productCollectionList =
        productCollectionRepository.findByStoreIdAndProductCodeIn(storeId, productCodes);
    return productCollectionList.stream().collect(Collectors.toMap(ProductCollection::getProductCode, Function.identity()));
  }

  private Map<String, ItemImageResponse> getProductItemResponseBySkuCode(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList, List<String> skuCodes) throws Exception {
    List<ItemImageResponse> itemImageResponseList =
        productOutbound.getProductItemImagesByItemCode(new SkuCodesRequest(skuCodes), false);
    return itemImageResponseList.stream()
        .collect(Collectors.toMap(ItemImageResponse::getItemCode, Function.identity()));
  }

  private Map<String, CategoryDetailDto> getCategoryHierarchyByCategoryCodes(
      List<ItemPickupPointListingResponse> itemPickupPointListingResponseList) throws Exception {
    Map<String, CategoryDetailDto> categoryHierarchyMap = new HashMap<>();
    List<String> categoryCodes =
        itemPickupPointListingResponseList.stream().map(ItemPickupPointListingResponse::getCategoryCode).distinct()
            .collect(Collectors.toList());
    List<CategoryHierarchyResponse> categoryHierarchyResponseList =
        categoryRepository.findHierarchyByCategoryCodes(new CategoryCodeRequest(categoryCodes));
    for (CategoryHierarchyResponse categoryHierarchyResponse : categoryHierarchyResponseList) {
      List<String> categoryNames =
          categoryHierarchyResponse.getCategoryHierarchy().stream().map(CategoryResponse::getName)
              .collect(Collectors.toList());
      categoryHierarchyMap.put(categoryHierarchyResponse.getCategoryCode(),
          new CategoryDetailDto(categoryHierarchyResponse.getCategoryHierarchy().get(0).getId(),
              categoryHierarchyResponse.getCategoryHierarchy().get(0).getName(),
              StringUtils.join(Lists.reverse(categoryNames), Constants.HIERARCHY_DELIMITER)));
    }
    return categoryHierarchyMap;
  }

  private Page<ItemPickupPointListingResponse> getItemPickupPointListForActiveProducts(String storeId, String username,
      String requestId, int page, int size, ItemPickupPointListingL3Request itemPickupPointListingL3Request) {
    Page<ItemPickupPointListingResponse> itemPickupPointListingResponsePage =
        xProductOutbound.getItemPickupPointList(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            requestId, username, page, size,
            RequestHelper.toItemPickupPointListingRequest(itemPickupPointListingL3Request));
    return itemPickupPointListingResponsePage;
  }

  private Page<ItemPickupPointListingResponse> getItemPickupPointListForNeedCorrectionProducts(String storeId, int page,
      int size, ItemPickupPointListingL3Request itemPickupPointListingL3Request) throws Exception {
    List<ItemPickupPointListingResponse> itemPickupPointListingResponseList = new ArrayList<>();
    ProductBusinessPartner productBusinessPartner =
        productBusinessPartnerRepository.findFirstByGdnProductSku(itemPickupPointListingL3Request.getProductSku());
    Page<ProductItemBusinessPartner> productItemBusinessPartnerPage =
        productItemBusinessPartnerService.getProductItemBusinessPartnerForL5Listing(storeId,
            productBusinessPartner.getId(), page, size, itemPickupPointListingL3Request);
    if (CollectionUtils.isNotEmpty(productItemBusinessPartnerPage.getContent())) {
      List<ProductCollection> productCollections = productCollectionRepository.findByStoreIdAndProductIds(storeId,
          Arrays.asList(productBusinessPartner.getProductId()));
      if (CollectionUtils.isNotEmpty(productCollections)) {
        ProductDetailResponse productDetailResponse =
            productOutbound.getProductDetailByProductCode(productCollections.get(0).getProductCode(), false, true);
        List<BusinessPartnerPickupPointResponse> businessPartnerPickupPointResponseList =
            xProductOutbound.getPickupPointDetailsByListOfPickupPointCodes(
                productItemBusinessPartnerPage.getContent().stream().map(ProductItemBusinessPartner::getPickupPointId)
                    .distinct().collect(Collectors.toList()));
        List<ProductItemWholesalePrice> productItemWholesalePriceList =
            productItemWholesalePriceService.findByStoreIdAndItemSkuAndPickupPointCode(storeId,
              productItemBusinessPartnerPage.getContent().stream().map(
                  productItemBusinessPartner -> ItemPickupPointDto.builder()
                    .itemSku(productItemBusinessPartner.getGdnProductItemSku())
                    .pickupPointCode(productItemBusinessPartner.getPickupPointId()).build())
                .collect(Collectors.toList()));
        ProfileResponse profileResponse = this.businessPartnerRepository.filterDetailByBusinessPartnerCode(
            itemPickupPointListingL3Request.getBusinessPartnerCode());
        Pair<Boolean, List<ItemPickupPointListingResponse>> itemPickupPointListingResponseListPair =
            ResponseHelper.toItemPickupPointListingResponse(productItemBusinessPartnerPage.getContent(),
                productBusinessPartner, productCollections.get(0), productDetailResponse,
                businessPartnerPickupPointResponseList, productItemWholesalePriceList, profileResponse,
                backFillWrongProductItemIds, cncForWarehouseFeatureSwitch);
        itemPickupPointListingResponseList = itemPickupPointListingResponseListPair.getRight();
        if (Boolean.TRUE.equals(itemPickupPointListingResponseListPair.getLeft())) {
          productItemBusinessPartnerService.saveAll(productItemBusinessPartnerPage.getContent());
        }
      }
    }
    return new PageImpl<>(itemPickupPointListingResponseList, PageRequest.of(page, size),
        productItemBusinessPartnerPage.getTotalElements());
  }

  private ProductL3DetailsResponse generateProductLevel3DetailByProductDetailResponse(
    ProductDetailResponse productDetailResponse, List<CategoryResponse> categories,
    List<ProductLevel3Logistics> productLevel3Logistics, ProfileResponse profileResponse,
    ProductBusinessPartner productBusinessPartner, String productSku, String productCode,
    ProductCollection productCollection, boolean concatenateValueWithValueType) throws Exception {
    ProductL3DetailsResponse product = new ProductL3DetailsResponse();
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
    product.setOff2OnChannelActive(productBusinessPartner.isOff2OnChannelActive());
    product.setOnline(productBusinessPartner.isOnline());
    product.setCncActivated(productBusinessPartner.isCncActivated());
    product.setCategoryCode(productBusinessPartner.getCategoryCode());
    product.setB2bActivated(productBusinessPartner.isB2bActivated());
    product.setB2cActivated(productBusinessPartner.isB2cActivated());
    product.setBundleProduct(productBusinessPartner.isBundleProduct());
    product.setSizeChartCode(productBusinessPartner.getSizeChartCode());

    if (CollectionUtils.isNotEmpty(categories)) {
      CategoryDetailDto categoryDetailDto =
        ResponseHelper.generateCategoryNameIdAndHierarchy(categories);
      setDimensionsMissing(categories, profileResponse, productBusinessPartner, product);
      product.setCategoryName(categoryDetailDto.getCategoryName());
      product.setCategoryHierarchy(categoryDetailDto.getCategoryHierarchy());
      product.setCategoryId(categoryDetailDto.getCategoryId());
      product.setCategoryNameEnglish(categoryDetailDto.getCategoryNameEnglish());
      product.setCategoryHierarchyEnglish(categoryDetailDto.getCategoryHierarchyEnglish());
      product.setWholesalePriceConfigEnabled(categories.get(0).isWholesalePriceConfigEnabled());
    }

    product.setProductName(productDetailResponse.getName());
    product.setBrand(productDetailResponse.getBrand());
    product.setBrandCode(
        Optional.ofNullable(productDetailResponse.getProductAttributeResponses()).orElse(new ArrayList<>()).stream()
            .filter(
                productAttributeResponse -> Constants.BRAND.equals(productAttributeResponse.getProductAttributeName()))
            .findFirst().flatMap(attr -> attr.getProductAttributeValues().stream().findFirst()
                .map(val -> val.getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode()))
            .orElse(productCollection.getBrandCode()));
    product.setDescription(new String(productDetailResponse.getDescription()));
    product.setSpecificationDetail(productDetailResponse.getSpecificationDetail());
    product.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    product.setProductStory(productDetailResponse.getProductStory());
    product.setUrl(productDetailResponse.getUrl());

    product.setInstallationRequired(false);
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());

    if (ranchIntegrationEnabled && Objects.nonNull(productDetailResponse.getDistributionInfoResponse())) {
      DistributionInfo distributionInfo = new DistributionInfo();
      distributionInfo.setProductName(productDetailResponse.getDistributionInfoResponse().getProductName());
      distributionInfo.setCategoryName(productDetailResponse.getDistributionInfoResponse().getCategoryName());
      product.setDistributionInfoResponse(distributionInfo);
    }

    if (CollectionUtils.isNotEmpty(productDetailResponse.getProductAttributeResponses())) {
      for (ProductAttributeResponse productAttributeResponse : productDetailResponse
        .getProductAttributeResponses()) {
        for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse
          .getProductAttributeValues()) {
          // skip sending attribute in response if attribute is hide for seller
          if (checkHideForSellerAttribute(productAttributeResponse))
            continue;
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
          productLevel3Attribute.setExtractedValue(productAttributeResponse.getAttribute().isExtractedValue());
          productLevel3Attribute.setSizeAttribute(productAttributeResponse.getAttribute().isSizeAttribute());
          productLevel3Attribute.setDsExtraction(productAttributeResponse.getAttribute().isDsExtraction());
          if (Objects.nonNull(productAttributeValueResponse.getAllowedAttributeValue())) {
            if (valueTypeAdditionForDefiningAttributes) {
              StringBuilder concatenatedValueAndValueType = new StringBuilder();
              String valueType = productAttributeValueResponse.getAllowedAttributeValue().getValueType();
              if (Objects.nonNull(valueType) && concatenateValueWithValueType) {
                concatenatedValueAndValueType.append(valueType).append(sizeChartValueTypeDelimiter)
                    .append(productAttributeValueResponse.getAllowedAttributeValue().getValue());
                productLevel3Attribute.getValues().add(concatenatedValueAndValueType.toString());
              } else {
                productLevel3Attribute.getValues()
                    .add(productAttributeValueResponse.getAllowedAttributeValue().getValue());
              }
              productLevel3Attribute.setValueType(
                  productAttributeValueResponse.getAllowedAttributeValue().getValueType());
            } else {
              productLevel3Attribute.getValues()
                  .add(productAttributeValueResponse.getAllowedAttributeValue().getValue());
            }
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
        image.setMarkForDelete(imageData.isMarkForDelete());
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
    if (ConverterUtil.isMPPEnabled(profileResponse, mppAllowedSellers)) {
      product.setFbbPickupPointCodes(Optional.ofNullable(
        productBusinessPartner.getProductItemBusinessPartners().stream().filter(
          productItemBusinessPartner -> !productItemBusinessPartner.isMarkForDelete()
            && productItemBusinessPartner.isFbbActive())
          .map(ProductItemBusinessPartner::getPickupPointId).distinct()
          .collect(Collectors.toList())).orElse(new ArrayList<>()));
      product.getPickupPointCodes().removeAll(product.getFbbPickupPointCodes());
    }
    product.setFbbActivated(productBusinessPartner.isFbbActivated());
    if (fetchL4BasedOnMfdInNrFlow) {
      product.setItemSkus(new ArrayList<>(productBusinessPartner.getProductItemBusinessPartners().stream()
          .filter(productItemBusinessPartner -> !productItemBusinessPartner.isMarkForDelete())
          .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toSet())));
    } else {
      product.setItemSkus(new ArrayList<>(productBusinessPartner.getProductItemBusinessPartners().stream()
          .map(ProductItemBusinessPartner::getGdnProductItemSku).collect(Collectors.toSet())));
    }
    product.setItemCount(product.getItemSkus().size());
    product.setResubmitCount(productCollection.getResubmitCount());
    product.setFreeSample(productBusinessPartner.isFreeSample());
    product.setBrandApprovalStatus(productCollection.getBrandApprovalStatus().name());
    product.setVideoUrl(Optional.ofNullable(productDetailResponse.getVideoDTO())
      .map(video -> StringUtils.defaultIfEmpty(video.getFinalUrl(), video.getSourceUrl()))
      .orElse(null));
    product.setCoverImagePath(
      Optional.ofNullable(productDetailResponse.getVideoDTO()).map(VideoDTO::getCoverImagePath)
        .orElse(null));
    if (Objects.nonNull(productDetailResponse.getDistributionInfoResponse())) {
      DistributionInfo distributionInfo = new DistributionInfo();
      distributionInfo.setProductName(productDetailResponse.getDistributionInfoResponse().getProductName());
      distributionInfo.setCategoryName(productDetailResponse.getDistributionInfoResponse().getCategoryName());
      product.setDistributionInfoResponse(distributionInfo);
    }
    product.setAiGeneratedFieldsResponse(productDetailResponse.getAiGeneratedFieldsResponse());
    return product;
  }

  private boolean checkHideForSellerAttribute(ProductAttributeResponse productAttributeResponse) {
    return productSuitabilityFeatureEnabled && Optional.ofNullable(productAttributeResponse.getAttribute())
        .map(AttributeResponse::isHideForSeller).orElse(false);
  }

  private void setDimensionsMissing(List<CategoryResponse> categories, ProfileResponse profileResponse,
      ProductBusinessPartner productBusinessPartner, ProductL3DetailsResponse product) {
    if (merchantTypesForBopisCategoryValidation(profileResponse, bopisCategoryValidationMerchantTypes)
        && bopisCategoryRestrictionFeatureEnabled) {
      boolean bopisInEligible = Boolean.FALSE.equals(
        categories.stream().filter(Objects::nonNull).reduce((first, second) -> second)
          .map(CategoryResponse::isBopisEligible).orElse(null));
      boolean bopisProduct =
          Optional.ofNullable(productBusinessPartner.getProductItemBusinessPartners()).orElse(new ArrayList<>())
              .stream().allMatch(productItemBusinessPartner -> productItemBusinessPartner.getProductType()
                  .equals(ProductType.BOPIS.getProductType()));
      product.setDimensionsMissing(bopisInEligible && bopisProduct);
    }
  }

  private ProductL3DetailsResponse generateProductLevel3Detail(ProductL3Response productData,
    List<CategoryResponse> categories, List<ProductLevel3Logistics> productLevel3Logistics,
    ProfileResponse profileResponse, ProductCollection productCollection, boolean concatenateValueWithValueType) throws Exception {
    ProductL3DetailsResponse product = new ProductL3DetailsResponse();
    BeanUtils.copyProperties(productData, product);
    product.setProductL3Response(productData);
    product.setOnline(productData.isOnline());
    product.setCncActivated(productData.isCncActivated());
    product.setProfileResponse(profileResponse);
    product.setBusinessPartnerCode(productData.getMerchantCode());
    product.setSynchronize(productData.isSynchronized());
    if (Objects.nonNull(productData.getProductType())) {
      product.setProductType(productData.getProductType().getCode());
    } else if (setDefaultProductType) {
      product.setProductType(1);
    }
    product.setSuspended(productData.isSuspended());
    product.setArchived(productData.isArchived());
    product.setProductEditable(productData.isProductEditable());
    product.setMerchantPromoDiscount(productData.isMerchantPromoDiscount());
    product.setMerchantPromoDiscountActive(productData.isMerchantPromoDiscountActive());
    product.setDisableUnSync(productData.isDisableUnSync());
    product.setFbbActivated(productData.isFbbActivated());
    product.setPickupPointCodes(productData.getPickupPointCodes());
    product.setFbbPickupPointCodes(productData.getFbbPickupPointCodes());
    product.setB2bActivated(productData.isB2bActivated());
    product.setB2cActivated(productData.isB2cActivated());
    if (ranchIntegrationEnabled && Objects.nonNull(productData.getDistributionInfoDTO())) {
      DistributionInfo distributionInfo = new DistributionInfo();
      distributionInfo.setProductName(productData.getDistributionInfoDTO().getProductName());
      distributionInfo.setCategoryName(productData.getDistributionInfoDTO().getCategoryName());
      product.setDistributionInfoResponse(distributionInfo);
    }
    if (!ConverterUtil.isMPPEnabled(profileResponse, mppAllowedSellers)) {
      if (CollectionUtils.isNotEmpty(productData.getFbbPickupPointCodes())) {
        product.getPickupPointCodes().addAll(productData.getFbbPickupPointCodes());
      }
      product.setFbbPickupPointCodes(new ArrayList<>());
    }
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
      product.setBrandCode(Optional.ofNullable(productData.getMasterDataProduct())
          .map(MasterDataProductDTO::getMasterDataProductAttributes).map(attrs -> attrs.stream()
              .filter(attr -> Constants.BRAND.equals(attr.getMasterDataAttribute().getAttributeName())).findFirst()
              .flatMap(attr -> attr.getMasterDataProductAttributeValues().stream().findFirst()
                  .map(val -> val.getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode()))
              .orElse(productCollection.getBrandCode())).orElse(productCollection.getBrandCode()));
      product.setDescription(new String(productData.getMasterDataProduct().getDescription()));
      product.setSpecificationDetail(productData.getMasterDataProduct().getSpecificationDetail());
      product.setUniqueSellingPoint(productData.getMasterDataProduct().getUniqueSellingPoint());
      product.setProductStory(productData.getMasterDataProduct().getProductStory());
      product.setUrl(productData.getMasterDataProduct().getUrl());
    }
    product.setInstallationRequired(productData.isInstallationRequired());
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());

    Map<String, MasterDataAttributeDTO>  attributeDatas =
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

    MasterDataAttributeDTO  masterDataAttributeDTO = null;
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
            productLevel3Attribute.setExtractedValue(masterDataAttributeDTO.isExtractedValue());
            productLevel3Attribute.setSizeAttribute(masterDataAttributeDTO.isSizeAttribute());
            productLevel3Attribute.setDsExtraction(masterDataAttributeDTO.isDsExtraction());
          }
          productLevel3Attribute.getValues().add(descriptiveAttributeData.getAttributeValue());
          product.getAttributes().add(productLevel3Attribute);
        }
      }
    }
    if (CollectionUtils.isNotEmpty(productData.getDefiningAttributes())) {
      List<String> definingAttributeCodeAndValue = new ArrayList<>();
      for (ProductAttributeDTO definingAttributeData : productData.getDefiningAttributes()) {
        String itemSku = definingAttributeData.getItemSku();
        for (ProductAttributeDetailDTO definingDetailAttributeData : definingAttributeData
          .getProductAttributeDetails()) {
          if (!definingAttributeCodeAndValue.contains(
              definingDetailAttributeData.getAttributeCode() + definingDetailAttributeData.getAttributeValue())) {
          definingAttributeCodeAndValue.add(
              definingDetailAttributeData.getAttributeCode() + definingDetailAttributeData.getAttributeValue());
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
            productLevel3Attribute.setSizeAttribute(masterDataAttributeDTO.isSizeAttribute());
            productLevel3Attribute.setDsExtraction(masterDataAttributeDTO.isDsExtraction());
          }
          productLevel3Attribute.getValues().add(definingDetailAttributeData.getAttributeValue());
          productLevel3Attribute.setValueType(StringUtils.EMPTY);
          productLevel3Attribute.setItemSku(itemSku);
          product.getAttributes().add(productLevel3Attribute);
          }
        }
      }
    }
    if (Objects.nonNull(productData.getMasterDataProduct())) {
      for (MasterDataProductImageDTO imageData : productData.getMasterDataProduct()
        .getMasterDataProductImages()) {
        if (imageData.isCommonImage()) {
          ProductL3CommonImageResponse image = new ProductL3CommonImageResponse();
          image.setMainImage(imageData.isMainImage());
          image.setSequence(imageData.getSequence());
          image.setLocationPath(imageData.getLocationPath());
          image.setMarkForDelete(Boolean.FALSE);
          image.setActiveLocation(Boolean.TRUE);
          product.getCommonImages().add(image);
        }
      }
    }
    ValueTypeUtil.concatenateValueWithValueType(productData, product, concatenateValueWithValueType,
        sizeChartValueTypeDelimiter, valueTypeAdditionForDefiningAttributes);

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

    product.setItemSkus(productData.getItemSkus());
    product.setFreeSample(productData.isFreeSample());
    product.setBrandApprovalStatus(productCollection.getBrandApprovalStatus().name());
    if (merchantTypesForBopisCategoryValidation(profileResponse, bopisCategoryValidationMerchantTypes) && bopisCategoryRestrictionFeatureEnabled) {
      product.setDimensionsMissing(productData.getDimensionsMissing());
    }
    ResponseHelper.filterHideFromSellerAttributes(product, productData, productSuitabilityFeatureEnabled);
    product.setAiGeneratedFieldsResponse(ResponseHelper.getAiGeneratedFields(productData.getAiGeneratedFieldsResponse()));
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
      productLevel3Attribute.setDsExtraction(attributeResponse.isDsExtraction());
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
      productLevel3Attribute.setDsExtraction(masterDataAttributeDetail.isDsExtraction());
    }
  }

  private boolean isFbbSortRequired(String businessPartnerCode) throws Exception {
    ProfileResponse profileResponse =
      businessPartnerRepository.filterDetailByBusinessPartnerCode(businessPartnerCode);
    if(Objects.isNull(profileResponse)){
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INVALID_BUSINESS_PARTNER_CODE.getDesc());
    }
    return fbbSortingSellers.contains(profileResponse.getCompany().getMerchantType());
  }

  @Override
  public ProductSkuDetailResponse getProductSkuDetailResponse(String storeId,
    ProductCodeAndSkuRequest productCodeAndSkuRequest) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId),
      ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCodeAndSkuRequest.getProductSku()),
      ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    ProductCollection productCollection;
    if (StringUtils.isBlank(productCodeAndSkuRequest.getProductCode())) {
      productCollection =
        productCollectionRepository.findProductByGdnSku(productCodeAndSkuRequest.getProductSku());
    } else {
      productCollection = productCollectionRepository.findByStoreIdAndProductCode(storeId,
        productCodeAndSkuRequest.getProductCode());
    }
    GdnPreconditions.checkArgument(Objects.nonNull(productCollection),
      ErrorMessages.PRODUCT_SKU_NOT_FOUND);
    ProductCenterDetailResponse productCenterDetailResponse =new ProductCenterDetailResponse();
    if (!productCollection.isMarkForDelete() && WorkflowStates.ACTIVE.name()
      .equals(productCollection.getState())) {
      productCenterDetailResponse =
        xProductOutbound.getProductSkuDetailResponse(productCodeAndSkuRequest.getProductSku());
    }
    return ConverterUtil.getProductSkuDetailResponse(productCodeAndSkuRequest.getProductSku(),
      productCollection, productCenterDetailResponse);
  }
}
