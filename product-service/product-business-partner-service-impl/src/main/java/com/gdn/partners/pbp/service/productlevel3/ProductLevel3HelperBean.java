package com.gdn.partners.pbp.service.productlevel3;

import com.gda.mta.product.dto.DeletedProductItems;
import com.gda.mta.product.dto.ProductBundleRecipeRequest;
import com.gda.mta.product.dto.ProductL3UpdateRequest;
import com.gda.mta.product.dto.ProductLevel3AttributeRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryDetailsImageRequest;
import com.gda.mta.product.dto.ProductVariantPriceStockAndImagesRequest;
import com.gda.mta.product.dto.QuickEditRequest;
import com.gda.mta.product.dto.QuickEditV2Request;
import com.gda.mta.product.dto.RestrictedKeywordsByField;
import com.gda.mta.product.dto.RestrictedKeywordsByFieldAndActionType;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.entity.KeywordRequestDTO;
import com.gdn.mta.product.entity.PreOrderDTO;
import com.gdn.mta.product.entity.ProductBundleRecipe;
import com.gdn.mta.product.entity.ProductBusinessPartner;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.entity.ProductLevel3Image;
import com.gdn.mta.product.entity.ProductLevel3Logistics;
import com.gdn.mta.product.enums.ApiErrorCode;
import com.gdn.mta.product.enums.ProductLevel3Status;
import com.gdn.mta.product.enums.RestrictedKeywordActionType;
import com.gdn.mta.product.util.ProductContentUtil;
import com.gdn.mta.product.util.validator.PhoneNumberValidator;
import com.gdn.mta.product.util.validator.RestrictedKeywordValidator;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.warehouse.WareHouseOutBound;
import com.gdn.partners.pbp.outbound.xProduct.XProductOutbound;
import com.gdn.warehouse.itemmaster.command.model.biilofmaterial.CreateUpdateBillOfMaterialRecipeCommandRequest;
import com.gdn.x.product.model.vo.BundleRecipeVo;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.dto.ItemViewConfigDTO;
import com.gdn.x.product.rest.web.model.request.ItemActivationRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemActivationRequest;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemSummaryListResponse;
import com.gdn.x.product.rest.web.model.response.SharedProductBundleRecipeResponse;
import com.gdn.x.product.rest.web.model.response.SkuCodeBundleRecipeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.RestrictedKeywordsMappedToCategoryResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Service
@Slf4j
public class ProductLevel3HelperBean implements ProductLevel3Helper {

  public static final int SKIP_ALL_ACTIONS = -1;

  @Value("${phone.number.detection.regex}")
  private String phoneNumberDetectionRegex;

  @Value("${family.color.attribute.code}")
  private String familyColorAttributeCode;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${intelligent.restricted.keywords.switch}")
  private boolean intelligentRestrictedKeywordsSwitch;

  @Value("${update.skip.all.actions.based.on.action}")
  private boolean updateSkipAllActionsBasedOnAction;

  @Value("${auto.fill.family.colour.attribute}")
  private boolean autoFillFamilyColourAttribute;

  @Value("${shared.product.bundle.recipe.edit.enabled}")
  private boolean sharedProductBundleRecipeEditEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;


  @Autowired
  private ProductOutbound productOutbound;

  @Autowired
  private XProductOutbound xProductOutbound;

  @Autowired
  private WareHouseOutBound wareHouseOutBound;

  @Override
  public List<RestrictedKeywordsByField> getRestrictedKeywordsInProductDetails(
      ProductDetailResponse productDetailResponse, String categoryCode) {
    List<String> restrictedKeywordMappedToCategory = productOutbound.getRestrictedKeywordMappedToCategory(categoryCode);
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
    if (!CollectionUtils.isEmpty(restrictedKeywordMappedToCategory)) {
      restrictedKeywordsByFieldList = RestrictedKeywordValidator
          .getRestrictedKeywordPresentInProductDetail(restrictedKeywordMappedToCategory, productDetailResponse);
    }
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, phoneNumberDetectionRegex,
        restrictedKeywordsByFieldList);
    return restrictedKeywordsByFieldList;
  }

  @Override
  public RestrictedKeywordsByFieldAndActionType getRestrictedKeywordsWithActionTypeInProductDetails(
      ProductDetailResponse productDetailResponse, String categoryCode) {
    RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType =
        new RestrictedKeywordsByFieldAndActionType();
    List<RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordMappedToCategory =
        productOutbound.getRestrictedKeywordMappedToCategoryWithAction(categoryCode);
    Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordsMappedToCategoryResponseMap =
        restrictedKeywordMappedToCategory.stream()
            .filter(restrictedKeywordsResponse -> StringUtils.isNotBlank(restrictedKeywordsResponse.getKeyword()))
            .collect(Collectors.toMap(response -> response.getKeyword().toLowerCase(), Function.identity()));
    List<RestrictedKeywordsByField> restrictedKeywordsByFieldList = new ArrayList<>();
    if (!CollectionUtils.isEmpty(restrictedKeywordMappedToCategory)) {
      restrictedKeywordsByFieldList = RestrictedKeywordValidator.getRestrictedKeywordPresentInProductDetail(
          restrictedKeywordMappedToCategory.stream().map(RestrictedKeywordsMappedToCategoryResponse::getKeyword)
              .filter(StringUtils::isNotBlank).collect(Collectors.toList()), productDetailResponse);
      if (CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList)) {
        getResultantActionType(restrictedKeywordsMappedToCategoryResponseMap, restrictedKeywordsByFieldList,
            restrictedKeywordsByFieldAndActionType);
      }
      else {
        setSkipAllActionsToTrue(restrictedKeywordsByFieldAndActionType);
      }
    }
    else {
      setSkipAllActionsToTrue(restrictedKeywordsByFieldAndActionType);
    }
    boolean noKeywordsDetected = CollectionUtils.isEmpty(restrictedKeywordsByFieldList);
    PhoneNumberValidator.isPhoneNumberOrEmailPresentInProductDetails(productDetailResponse, phoneNumberDetectionRegex,
        restrictedKeywordsByFieldList);
    if (noKeywordsDetected && CollectionUtils.isNotEmpty(restrictedKeywordsByFieldList)) {
      log.info("No keywords detected but number/ email detected for productCode : {} ",
          productDetailResponse.getProductCode());
      restrictedKeywordsByFieldAndActionType.setAction(
          RestrictedKeywordActionType.MANUAL_REVIEW_DEFAULT.getRestrictedKeywordActionType());
      restrictedKeywordsByFieldAndActionType.setSkipAllActions(false);
    }
    restrictedKeywordsByFieldAndActionType.setRestrictedKeywordsByFieldList(restrictedKeywordsByFieldList);
    log.info("Product action : {} and top restrictedKeywordID : {} for productCode : {} skipAllActions : {}",
        restrictedKeywordsByFieldAndActionType.getAction(),
        restrictedKeywordsByFieldAndActionType.getCategoryRestrictedKeywordId(),
        productDetailResponse.getProductCode(), restrictedKeywordsByFieldAndActionType.isSkipAllActions());
    if (updateSkipAllActionsBasedOnAction && SKIP_ALL_ACTIONS == restrictedKeywordsByFieldAndActionType.getAction()) {
      restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    }
    return restrictedKeywordsByFieldAndActionType;
  }

  private void setSkipAllActionsToTrue(RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(true);
    restrictedKeywordsByFieldAndActionType.setAction(-1);
  }

  @Override
  public RestrictedKeywordsByFieldAndActionType getResultantActionType(
      Map<String, RestrictedKeywordsMappedToCategoryResponse> restrictedKeywordToActionTypeMap,
      List<RestrictedKeywordsByField> restrictedKeywordsByFieldList,
      RestrictedKeywordsByFieldAndActionType restrictedKeywordsByFieldAndActionType) {
    Map<String, KeywordRequestDTO> keywordRequestDTOHashMap = new HashMap<>();
    int resultantAction = -1;
    int prevAction = resultantAction;
    boolean skipAllActions = false;
    String resultantKeyword = StringUtils.EMPTY;
    for (RestrictedKeywordsByField restrictedKeywordsByField : restrictedKeywordsByFieldList) {
      for (String keyword : restrictedKeywordsByField.getKeywords()) {
        if (restrictedKeywordToActionTypeMap.containsKey(keyword.toLowerCase())) {
          keywordRequestDTOHashMap.putIfAbsent(keyword.toLowerCase(),
            getKeywordRequestDTO(restrictedKeywordToActionTypeMap.get(keyword.toLowerCase())));
        }
        RestrictedKeywordsMappedToCategoryResponse restrictedKeywordsMappedToCategoryResponse =
            Optional.ofNullable(restrictedKeywordToActionTypeMap.get(keyword.toLowerCase()))
                .orElse(RestrictedKeywordsMappedToCategoryResponse.builder().action(1).build());
        if (intelligentRestrictedKeywordsSwitch && restrictedKeywordsMappedToCategoryResponse.isValidateByDs()) {
          log.info("Skipping all actions as keyword : {} is validate by ds ", keyword);
          skipAllActions = true;
        }
        resultantAction = Integer.max(resultantAction,
            restrictedKeywordsMappedToCategoryResponse.getAction());
        if (prevAction != resultantAction) {
          resultantKeyword = keyword;
        }
        prevAction = resultantAction;
      }
    }
    restrictedKeywordsByFieldAndActionType.setAction(resultantAction);
    restrictedKeywordsByFieldAndActionType.setKeywordToKeywordRequestDTOMap(
      keywordRequestDTOHashMap);
    restrictedKeywordsByFieldAndActionType.setCategoryRestrictedKeywordId(
        Optional.ofNullable(restrictedKeywordToActionTypeMap.get(resultantKeyword.toLowerCase()))
            .orElse(new RestrictedKeywordsMappedToCategoryResponse()).getCategoryRestrictedKeywordId());
    restrictedKeywordsByFieldAndActionType.setSkipAllActions(skipAllActions);
    if (skipAllActions) {
      // If skipping all actions then setting action to invalid type
      restrictedKeywordsByFieldAndActionType.setAction(SKIP_ALL_ACTIONS);
    }
    log.debug("RestrictedKeywordsByFieldAndActionType : {} ", restrictedKeywordsByFieldAndActionType);
    return restrictedKeywordsByFieldAndActionType;
  }

  private KeywordRequestDTO getKeywordRequestDTO(
      RestrictedKeywordsMappedToCategoryResponse keywordsMappedToCategoryResponse) {
    return KeywordRequestDTO.builder().keyword(keywordsMappedToCategoryResponse.getKeyword())
        .keywordId(keywordsMappedToCategoryResponse.getKeywordId())
        .keywordAction(RestrictedKeywordActionType.getActionFromValue(keywordsMappedToCategoryResponse.getAction()))
        .keywordType(keywordsMappedToCategoryResponse.getKeywordType())
        .validateByDs(keywordsMappedToCategoryResponse.isValidateByDs()).build();
  }

  @Override
  public boolean isProductItemDetailChangedForListingUpdate(QuickEditRequest quickEditRequest,
      ItemSummaryResponse itemSummaryResponse, Boolean wholesalePriceActivated) {
    if (!(itemSummaryResponse.getPrice().stream().anyMatch(
        priceDTO -> priceDTO.getListPrice() == quickEditRequest.getPrice().getPrice()
            && priceDTO.getOfferPrice() == quickEditRequest.getPrice().getSalePrice()))) {
      return true;
    }
    if (quickEditRequest.getStatus() != getProductLevel3Status(itemSummaryResponse.isBuyable(),
      itemSummaryResponse.isDiscoverable())) {
      return true;
    }
    if (!StringUtils.equals(String.valueOf(itemSummaryResponse.isOff2OnChannelActive()),
        String.valueOf(quickEditRequest.getOff2OnActiveFlag()))) {
      return true;
    }
    if (StringUtils.isNotEmpty(quickEditRequest.getSellerSku()) && !StringUtils
        .equals(quickEditRequest.getSellerSku(), itemSummaryResponse.getMerchantSku())) {
      return true;
    }
    if (StringUtils.isNotEmpty(quickEditRequest.getPickupPointCode())) {
      return true;
    }
    if (!StringUtils.equals(String.valueOf(wholesalePriceActivated),
        String.valueOf(itemSummaryResponse.getWholesalePriceActivated()))) {
      return true;
    }
    return false;
  }

  @Override
  public void setProductLevel3DetailsFromSummaryResponse(ProductBusinessPartner productBusinessPartner,
      ProductLevel3 productLevel3) {
    productBusinessPartner.setProductName(productLevel3.getProductName());
    productBusinessPartner.setBrand(productLevel3.getBrand());
    productBusinessPartner.setCategoryName(productLevel3.getCategoryName());
  }

  @Override
  public boolean isProductItemDetailChangedForL5ListingUpdate(QuickEditV2Request quickEditV2Request,
    ItemSummaryListResponse itemSummaryListResponse, Boolean wholesaleActivated) {
    if ((itemSummaryListResponse.getPrice().stream().noneMatch(
      priceDTO -> priceDTO.getListPrice() == quickEditV2Request.getPrice().getPrice()
        && priceDTO.getOfferPrice() == quickEditV2Request.getPrice().getSalePrice()))) {
      return true;
    }
    ItemViewConfigDTO defaultItemViewConfigDTO = itemSummaryListResponse.getItemViewConfigs().stream()
        .filter(itemViewConfigDTO1 -> Constants.DEFAULT.equals(itemViewConfigDTO1.getChannel())).findFirst()
        .orElse(new ItemViewConfigDTO());
    ItemViewConfigDTO b2bItemViewConfigDTO = itemSummaryListResponse.getItemViewConfigs().stream()
        .filter(itemViewConfigDTO -> Constants.B2B_CHANNEL.equals(itemViewConfigDTO.getChannel())).findFirst()
        .orElse(null);
    ItemViewConfigDTO cncItemViewConfigDTO = itemSummaryListResponse.getItemViewConfigs().stream()
        .filter(itemViewConfigDTO1 -> Constants.CNC_CHANNEL.equals(itemViewConfigDTO1.getChannel())).findFirst()
        .orElse(null);
    if (quickEditV2Request.getStatus() != getProductLevel3Status(defaultItemViewConfigDTO.isBuyableOriginal(),
      defaultItemViewConfigDTO.isDiscoverableOriginal())) {
      return true;
    }
    if (cncForWarehouseFeatureSwitch && (Objects.isNull(cncItemViewConfigDTO)
        || quickEditV2Request.getCncStatus() != getProductLevel3Status(cncItemViewConfigDTO.isBuyableOriginal(),
        cncItemViewConfigDTO.isDiscoverableOriginal()))) {
      return true;
    }
    if (Objects.nonNull(quickEditV2Request.getB2bFieldsRequest())) {
      if (Objects.isNull(b2bItemViewConfigDTO)) {
        return true;
      }
      if (quickEditV2Request.getB2bFieldsRequest().getStatus() != getProductLevel3Status(
          b2bItemViewConfigDTO.isBuyable(), b2bItemViewConfigDTO.isDiscoverable())) {
        return true;
      }
      if (quickEditV2Request.getB2bFieldsRequest().isManaged() != itemSummaryListResponse.getB2bFields().isManaged()) {
        return true;
      }
      if (!Objects.equals(quickEditV2Request.getB2bFieldsRequest().getBasePrice(),
          itemSummaryListResponse.getB2bFields().getBasePrice())) {
        return true;
      }
    }
    if (!Boolean.valueOf(itemSummaryListResponse.isOff2OnChannelActive())
      .equals(quickEditV2Request.getOff2OnActiveFlag())) {
      return true;
    }
    if (StringUtils.isNotEmpty(quickEditV2Request.getSellerSku()) && !StringUtils
      .equals(quickEditV2Request.getSellerSku(), itemSummaryListResponse.getMerchantSku())) {
      return true;
    }
    if (!Objects.equals(wholesaleActivated, itemSummaryListResponse.getWholesalePriceActivated())) {
      return true;
    }
    if (quickEditV2Request.isScheduleUpdate()) {
      return true;
    }
    if (!cncForWarehouseFeatureSwitch && !Boolean.valueOf(itemSummaryListResponse.isCncActive())
        .equals(quickEditV2Request.getCncActive())) {
      return true;
    }
    return false;
  }

  public static ProductLevel3Status getProductLevel3Status(boolean buyable, boolean discoverable) {
    if (buyable && discoverable) {
      return ProductLevel3Status.ONLINE;
    } else if (!buyable && !discoverable) {
      return ProductLevel3Status.OFFLINE;
    } else if (!buyable) {
      return ProductLevel3Status.TEASER;
    } else {
      return ProductLevel3Status.B2B;
    }
  }

  @Override
  public ProductLevel3 generateProductLevel3(ProductL3UpdateRequest request) {
    ProductLevel3 product = new ProductLevel3();
    Map<String, ProductLevel3AttributeRequest> attributeCodeToAttributeMap = new HashMap<>();
    for (ProductLevel3AttributeRequest productLevel3AttributeRequest : request.getAttributes()) {
      attributeCodeToAttributeMap.put(productLevel3AttributeRequest.getAttributeCode(), productLevel3AttributeRequest);
    }
    BeanUtils.copyProperties(request, product, "attributes", "productVariantUpdateRequest",
      "commonImages");
    product.setItems(new ArrayList<>());
    product.setAttributes(new ArrayList<>());
    product.setImages(new ArrayList<>());
    List<ProductLevel3Logistics> logistics = new ArrayList<>();
    if (CollectionUtils.isNotEmpty(request.getProductLevel3LogisticsRequest())) {
      for (ProductLevel3Logistics productLevel3LogisticsRequest : request
        .getProductLevel3LogisticsRequest()) {
        ProductLevel3Logistics productLevel3Logistics = ProductLevel3Logistics.builder()
          .logisticProductCode(productLevel3LogisticsRequest.getLogisticProductCode())
          .selected(productLevel3LogisticsRequest.isSelected()).build();
        logistics.add(productLevel3Logistics);
      }
    }
    for (ProductLevel3AttributeRequest attributeRequest : request.getAttributes()) {
      ProductLevel3Attribute attribute = new ProductLevel3Attribute();
      BeanUtils.copyProperties(attributeRequest, attribute);
      product.getAttributes().add(attribute);
    }
    for (ProductLevel3SummaryDetailsImageRequest imageRequest : request.getCommonImages()) {
      ProductLevel3Image image = new ProductLevel3Image();
      BeanUtils.copyProperties(imageRequest, image);
      product.getImages().add(image);
    }
    // preparing request for newly added items
    if (addDeleteVariantSwitch) {
      List<ProductItemLevel3> newlyAddedItems = new ArrayList<>();
      prepareNewlyAddedItemsRequest(request, attributeCodeToAttributeMap, newlyAddedItems);
      product.setNewlyAddedItems(newlyAddedItems);
      product.setDeletedItems(
          request.getDeletedProductItems().stream().map(DeletedProductItems::getItemCode).collect(Collectors.toList()));
    }
    product.setFreeSample(request.isFreeSample());
    if(Objects.nonNull(request.getPreOrder())) {
      PreOrderDTO preOrder = new PreOrderDTO();
      BeanUtils.copyProperties(request.getPreOrder(), preOrder);
      product.setPreOrder(preOrder);
    }
    product.setSizeChartCode(request.getSizeChartCode());
    product.setSizeChartChanged(request.isSizeChartChanged());
    product.setB2cActivated(request.getB2cActivated());
    product.setVideoAddEditRequest(request.getVideoAddEditRequest());
    product.setVideoUpdated(request.getVideoUpdated());
    return product;
  }

  private void prepareNewlyAddedItemsRequest(ProductL3UpdateRequest request,
      Map<String, ProductLevel3AttributeRequest> attributeCodeToAttributeMap, List<ProductItemLevel3> newlyAddedItems) {
    for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : request.getProductItems()) {
      if (productVariantPriceStockAndImagesRequest.isNewlyAddedItem()) {
        ProductItemLevel3 productItemLevel3 = new ProductItemLevel3();
        productItemLevel3.setItemName(productVariantPriceStockAndImagesRequest.getItemName());
        productItemLevel3.setUpcCode(productVariantPriceStockAndImagesRequest.getUpcCode());
        productItemLevel3.setItemAttributesMap(productVariantPriceStockAndImagesRequest.getAttributesMap());
        productItemLevel3.setAttributesValueTypeMap(productVariantPriceStockAndImagesRequest.getAttributesValueTypeMap());
        for (Map.Entry<String, String> entry : productVariantPriceStockAndImagesRequest.getAttributesMap().entrySet()) {
          ProductLevel3AttributeRequest productLevel3AttributeRequest = attributeCodeToAttributeMap.get(entry.getKey());
          if (familyColorAttributeCode.equals(entry.getKey())) {
            productLevel3AttributeRequest = new ProductLevel3AttributeRequest();
            productLevel3AttributeRequest.setAttributeCode(familyColorAttributeCode);
            attributeCodeToAttributeMap.put(familyColorAttributeCode, productLevel3AttributeRequest);
          }
          checkProductLevel3AttributeRequestForNull(productLevel3AttributeRequest);
          productLevel3AttributeRequest.setValues(Collections.singletonList(entry.getValue()));
          ProductLevel3Attribute itemAttribute = new ProductLevel3Attribute();
          BeanUtils.copyProperties(productLevel3AttributeRequest, itemAttribute);
          productItemLevel3.getItemAttributes().add(itemAttribute);
        }
        productVariantPriceStockAndImagesRequest.getAttributesMap().entrySet()
            .removeIf(stringStringEntry -> familyColorAttributeCode.equals(stringStringEntry.getKey()));
        prepareImageRequest(productVariantPriceStockAndImagesRequest, productItemLevel3);
        newlyAddedItems.add(productItemLevel3);
      }
    }
  }

  private static void prepareImageRequest(ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest,
      ProductItemLevel3 productItemLevel3) {
    for (ProductLevel3SummaryDetailsImageRequest imageRequest : productVariantPriceStockAndImagesRequest.getImages()) {
        ProductLevel3Image image = new ProductLevel3Image();
        BeanUtils.copyProperties(imageRequest, image);
        productItemLevel3.getImages().add(image);
    }
  }

  private static void checkProductLevel3AttributeRequestForNull(ProductLevel3AttributeRequest productLevel3AttributeRequest) {
    if (Objects.isNull(productLevel3AttributeRequest)) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ApiErrorCode.INVALID_ADD_DELETE_REQUEST.getDesc());
    }
  }

  @Override
  public void addBundleRecipeInWMS(ProductAndItemActivationRequest productAndItemActivationRequest) {
    try {
      log.info("Calling BOM to add recipe");
      if (productAndItemActivationRequest.getProduct().isBundleProduct()) {
        List<String> itemSkus = productAndItemActivationRequest.getItems().stream().filter(Objects::nonNull)
            .map(ItemActivationRequest::getBundleRecipe).filter(Objects::nonNull).flatMap(Set::stream)
            .filter(Objects::nonNull).map(BundleRecipeVo::getItemSku).filter(StringUtils::isNotBlank)
            .collect(Collectors.toList());
        List<ItemBasicDetailV2Response> itemBasicDetailV2Responses =
            xProductOutbound.getItemBasicDetailV2Response(itemSkus, false);
        Map<String, String> itemSkuAndItemCodeMap =
            itemBasicDetailV2Responses.stream().filter(Predicate.not(ItemBasicDetailV2Response::isMarkForDelete))
                .collect(Collectors.toMap(ItemBasicDetailV2Response::getItemSku, ItemBasicDetailV2Response::getItemCode,
                    (oldValue, newValue) -> newValue));
        for (ItemActivationRequest itemActivationRequest : productAndItemActivationRequest.getItems()) {
          CreateUpdateBillOfMaterialRecipeCommandRequest
            createUpdateBillOfMaterialRecipeRequest =
              new CreateUpdateBillOfMaterialRecipeCommandRequest();
          createUpdateBillOfMaterialRecipeRequest.setBillOfMaterialSetup(
              RequestHelper.getBillOfMaterialSetupOnActivation(itemActivationRequest.getBundleRecipe(),
                  itemSkuAndItemCodeMap));
          createUpdateBillOfMaterialRecipeRequest.setItemCode(itemActivationRequest.getItemCode());
          log.info("request BOM to add recipe : {} ",  createUpdateBillOfMaterialRecipeRequest);
          wareHouseOutBound.createAndUpdateProductBundle(createUpdateBillOfMaterialRecipeRequest);
        }
        log.info("Calling BOM to add recipe is success");
      }
    } catch (Exception e) {
      log.error("Error while creating bom: request : {} ", productAndItemActivationRequest, e);
    }
  }


  @Override
  public void autoFillFamilyColourAttribute(ProductL3UpdateRequest productL3UpdateRequest) {
    if (autoFillFamilyColourAttribute) {
      //step 1 che if warna present in product attribute
      boolean isWarnaPresentAndDefiningAttributeType =
          Optional.ofNullable(productL3UpdateRequest.getAttributes()).orElse(new ArrayList<>()).stream()
              .anyMatch(ProductContentUtil::isWarnaAndDefiningAttributeOrVariantCreationTrue);

      //step 2 get list of newly created variants without family colour
      List<ProductVariantPriceStockAndImagesRequest>
          newlyAddedItemProductVariantPriceStockAndImagesRequestWithoutFamilyColour =
          Optional.ofNullable(productL3UpdateRequest.getProductItems()).orElse(new ArrayList<>()).stream()
              .filter(ProductVariantPriceStockAndImagesRequest::isNewlyAddedItem).filter(
                  productVariantPriceStockAndImagesRequest -> ProductContentUtil.isFamilyColourAttributeNotPresentInItem(
                      productVariantPriceStockAndImagesRequest, familyColorAttributeCode)).collect(Collectors.toList());

      //step 3 if isWarnaPresentAndDefiningAttributeType = true and newlyAddedItemProductVariantPriceStockAndImagesRequestWithoutFamilyColour is not empty then category detail to check if category ahs family colour attribute
      if (isWarnaPresentAndDefiningAttributeType && org.apache.commons.collections4.CollectionUtils.isNotEmpty(
          newlyAddedItemProductVariantPriceStockAndImagesRequestWithoutFamilyColour)) {
        CategoryDetailResponse categoryDetail =
            productOutbound.getCategoryDetailByCategoryCode(productL3UpdateRequest.getCategoryCode());
        CategoryAttributeResponse familyColourCategoryAttributeResponse =
            Optional.ofNullable(categoryDetail).map(CategoryDetailResponse::getCategoryAttributes)
                .orElse(new ArrayList<>()).stream().filter(
                    categoryAttributeResponse -> ProductContentUtil.isFamilyColourPresentInCategoryAttribute(
                        categoryAttributeResponse, familyColorAttributeCode)).findFirst().orElse(null);
        if (Objects.nonNull(familyColourCategoryAttributeResponse)) {
          for (ProductVariantPriceStockAndImagesRequest productVariantPriceStockAndImagesRequest : newlyAddedItemProductVariantPriceStockAndImagesRequestWithoutFamilyColour) {
            TreeMap<String, String> attributeMap =
                Optional.ofNullable(productVariantPriceStockAndImagesRequest.getAttributesMap())
                    .orElse(new TreeMap<>());
            attributeMap.put(familyColorAttributeCode, com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils.EMPTY);
            productVariantPriceStockAndImagesRequest.setAttributesMap(attributeMap);
          }
        }
      }
    }
  }

  @Override
  public List<ProductBundleRecipeRequest> validateShareProductRecipe(ProductL3UpdateRequest request,
      Map<String, String> itemSkuAndItemCodeMap) {
    if (sharedProductBundleRecipeEditEnabled) {
      // get item codes of the skus for which bundle recipe is edited
      Set<String> itemCodes = request.getProductBundleRecipe().stream().map(ProductBundleRecipeRequest::getItemSku)
          .filter(itemSkuAndItemCodeMap::containsKey).map(itemSkuAndItemCodeMap::get).collect(Collectors.toSet());

      // get shared product recipe details from x-product
      List<SharedProductBundleRecipeResponse> sharedProductBundleRecipeResponses =
          xProductOutbound.getSharedProductBundleRecipeDetails(itemCodes);

      // get map of shared item codes and recipe
      Map<String, Set<SkuCodeBundleRecipeResponse>> skuCodeAndRecipeMap =
          getSkuCodeAndBundleRecipeMapping(sharedProductBundleRecipeResponses);

      // iterate through shared product list and check if the recipe is same or not
      return validateSharedProductRecipe(request, itemSkuAndItemCodeMap, skuCodeAndRecipeMap);
    } else {
      return request.getProductBundleRecipe();
    }
  }

  private List<ProductBundleRecipeRequest> validateSharedProductRecipe(ProductL3UpdateRequest request,
      Map<String, String> itemSkuAndItemCodeMap, Map<String, Set<SkuCodeBundleRecipeResponse>> skuCodeAndRecipeMap) {
    List<ProductBundleRecipeRequest> bundleRecipeRequests = new ArrayList<>();
    for (ProductBundleRecipeRequest productBundleRecipeRequest : request.getProductBundleRecipe()) {
      boolean containsAllItemCodes =
          Optional.ofNullable(productBundleRecipeRequest.getBundleRecipe()).orElse(new HashSet<>()).stream()
              .map(ProductBundleRecipe::getItemSku).noneMatch(Predicate.not(itemSkuAndItemCodeMap::containsKey));
      String parentItemCode = itemSkuAndItemCodeMap.get(productBundleRecipeRequest.getItemSku());
      if (containsAllItemCodes && skuCodeAndRecipeMap.containsKey(parentItemCode)) {
        // check if recipe already present or not. If its not present then don't do any validation and create recipe in WMS/.
        // If its present then validate that recipe should be same and don't call OMS
        Set<SkuCodeBundleRecipeResponse> sharedProductRecipe = skuCodeAndRecipeMap.get(parentItemCode);
        if (CollectionUtils.isEmpty(sharedProductRecipe)) {
          bundleRecipeRequests.add(productBundleRecipeRequest);
        } else {
          Set<SkuCodeBundleRecipeResponse> updatedSharedProductRecipe =
              toSkuCodeBundleRecipeResponse(productBundleRecipeRequest.getBundleRecipe(), itemSkuAndItemCodeMap);
          if(!sharedProductRecipe.equals(updatedSharedProductRecipe)) {
            bundleRecipeRequests.add(productBundleRecipeRequest);
          }
        }
      } else {
        // if its not a shared product then create recipe in OMS.
        bundleRecipeRequests.add(productBundleRecipeRequest);
      }
    }
    return bundleRecipeRequests;
  }

  private Map<String, Set<SkuCodeBundleRecipeResponse>> getSkuCodeAndBundleRecipeMapping(
      List<SharedProductBundleRecipeResponse> sharedProductBundleRecipeResponses) {
    Map<String, Set<SkuCodeBundleRecipeResponse>> skuCodeAndRecipeMap =
        Optional.ofNullable(sharedProductBundleRecipeResponses).orElse(new ArrayList<>()).stream()
            .collect(
                Collectors.toMap(SharedProductBundleRecipeResponse::getItemCode,
                    sharedProductBundleRecipeResponse -> Optional.ofNullable(
                        sharedProductBundleRecipeResponse.getBundleRecipe()).orElse(new HashSet<>()), (v1, v2) -> v2));
    return skuCodeAndRecipeMap;
  }

  private Set<SkuCodeBundleRecipeResponse> toSkuCodeBundleRecipeResponse(Set<ProductBundleRecipe> productBundleRecipes,
      Map<String, String> itemSkuAndItemCodeMap) {
    Set<SkuCodeBundleRecipeResponse> skuCodeBundleRecipeResponses = new HashSet<>();
    for (ProductBundleRecipe productBundleRecipe : productBundleRecipes) {
      SkuCodeBundleRecipeResponse skuCodeBundleRecipeResponse =
          SkuCodeBundleRecipeResponse.builder().itemCode(itemSkuAndItemCodeMap.get(productBundleRecipe.getItemSku()))
              .quantity(productBundleRecipe.getQuantity()).build();
      skuCodeBundleRecipeResponses.add(skuCodeBundleRecipeResponse);
    }
    return skuCodeBundleRecipeResponses;
  }


}

