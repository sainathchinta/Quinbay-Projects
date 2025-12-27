package com.gdn.partners.pcu.external.web.controller.util;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.B2bDetailsDTO;
import com.gda.mta.product.dto.BundleRecipeRequest;
import com.gda.mta.product.dto.DimensionAndUomRequest;
import com.gda.mta.product.dto.DistributionItemRequest;
import com.gda.mta.product.dto.PickupPointCreateRequest;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.impl.exception.ValidationException;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import com.gdn.partners.pcu.external.web.model.request.DimensionAndUomWebRequest;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoUpdateRequest;
import com.gdn.partners.pcu.external.web.model.request.DistributionInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemUomInfoDTO;
import com.gdn.partners.pcu.external.web.model.request.ProductItemUomInfoWebRequest;
import com.gdn.partners.pcu.external.web.model.request.SubmitEvidenceIPRWebRequest;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.partners.pcu.external.service.impl.helper.BeanUtils;

import com.gda.mta.product.dto.PreOrderRequest;
import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductCopyRequest;
import com.gdn.partners.pcu.external.client.model.ProductCreationRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gda.mta.product.dto.ProductItemLogisticsRequest;
import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerAttributeServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.model.request.ProductItemBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.service.model.request.UploadImageRequest;
import com.gdn.partners.pcu.external.web.model.request.AllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.B2bFieldsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.BundleRecipeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.CatalogWebRequest;
import com.gdn.partners.pcu.external.web.model.request.CopyProductItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ImageRequest;
import com.gdn.partners.pcu.external.web.model.request.PickupPointCreateWebRequest;
import com.gdn.partners.pcu.external.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerAttributeWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemBusinessPartnerWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemCreationWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemLogisticsWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductItemWholesalePriceWebRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;

/**
 * Created by govind on 09/12/2018 AD.
 */
@Slf4j
public class ConverterUtil {

  private static final String REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE = "([^\\x00-\\x7F])|(/ +/ig)";
  private static final String REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      "(\\<.*?\\>|&\\w+.;)|(/\\r\\n|\\n|\\r/gm)|([\\•|\\)]\\s+)";
  private static final Pattern PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT =
      Pattern.compile(REGEX_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT);
  private static final Pattern PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE =
      Pattern.compile(REGEX_FOR_NON_ASCII_CHARS_AND_NEW_LINE);
  private static final String REGEX_FOR_EXTRA_SPACE = "\\s+";
  private static final Pattern PATTERN_FOR_EXTRA_SPACE = Pattern.compile(REGEX_FOR_EXTRA_SPACE);

  public static ProductBusinessPartnerServiceRequest toProductBusinessPartnerServiceRequest(
      ProductBusinessPartnerWebRequest productBusinessPartnerWebRequest, MandatoryParameterHelper mandatoryParameterHelper) {
    ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest =
        new ProductBusinessPartnerServiceRequest();
    BeanUtils.copyProperties(productBusinessPartnerWebRequest, productBusinessPartnerServiceRequest);
    productBusinessPartnerServiceRequest
        .setProductItemBusinessPartners(toProductItemBusinessPartnerServiceListRequest(
            productBusinessPartnerWebRequest.getProductItemBusinessPartners(),
            productBusinessPartnerWebRequest.getProductItemLogisticsWebRequests()));
    productBusinessPartnerServiceRequest.setProductBusinessPartnerAttributes(
        toProductBusinessPartnerAttributeServiceListRequest(
            productBusinessPartnerWebRequest.getProductBusinessPartnerAttributes()));
    productBusinessPartnerServiceRequest.setStoreId(mandatoryParameterHelper.getStoreId());
    productBusinessPartnerServiceRequest.setRequestId(mandatoryParameterHelper.getRequestId());
    productBusinessPartnerServiceRequest.setUsername(mandatoryParameterHelper.getUsername());
    productBusinessPartnerServiceRequest.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    productBusinessPartnerServiceRequest.setCreatedDate(new Date());
    return productBusinessPartnerServiceRequest;
  }

  private static List<ProductItemBusinessPartnerServiceRequest> toProductItemBusinessPartnerServiceListRequest(
      List<ProductItemBusinessPartnerWebRequest> productItemBusinessPartnerWebRequestList,
      List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequestList) {
    return productItemBusinessPartnerWebRequestList.stream()
        .map(productItemBusinessPartnerWebRequest -> ConverterUtil
            .toProductItemBusinessPartnerServiceRequest(productItemBusinessPartnerWebRequest,
                productItemLogisticsWebRequestList))
        .collect(Collectors.toList());
  }

  private static ProductItemBusinessPartnerServiceRequest toProductItemBusinessPartnerServiceRequest(
      ProductItemBusinessPartnerWebRequest productItemBusinessPartnerWebRequest,
      List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequestList) {
    ProductItemBusinessPartnerServiceRequest productItemBusinessPartnerServiceRequest =
        new ProductItemBusinessPartnerServiceRequest();
    productItemBusinessPartnerServiceRequest.setProductItemWholesalePriceRequests(
        productItemBusinessPartnerWebRequest.getProductItemWholesalePriceRequests());
    productItemBusinessPartnerServiceRequest
        .setProductItemLogisticsWebRequests(productItemLogisticsWebRequestList);
    BeanUtils.copyProperties(productItemBusinessPartnerWebRequest, productItemBusinessPartnerServiceRequest);
    return productItemBusinessPartnerServiceRequest;
  }

  private static ProductBusinessPartnerAttributeServiceRequest
  toProductBusinessPartnerAttributeServiceRequest(
      ProductBusinessPartnerAttributeWebRequest productBusinessPartnerAttributeWebRequest) {
    ProductBusinessPartnerAttributeServiceRequest productBusinessPartnerAttributeServiceRequest =
        new ProductBusinessPartnerAttributeServiceRequest();
    BeanUtils.copyProperties(productBusinessPartnerAttributeWebRequest,
        productBusinessPartnerAttributeServiceRequest);
    return productBusinessPartnerAttributeServiceRequest;
  }

  private static List<ProductBusinessPartnerAttributeServiceRequest>
  toProductBusinessPartnerAttributeServiceListRequest(
      List<ProductBusinessPartnerAttributeWebRequest>
          productBusinessPartnerAttributeWebRequestList) {
    return productBusinessPartnerAttributeWebRequestList.stream()
        .map(ConverterUtil::toProductBusinessPartnerAttributeServiceRequest)
        .collect(Collectors.toList());
  }

  public static ProductCreationRequest toProductCreationRequest(ProductCreationWebRequest productCreationWebRequest,
      MandatoryParameterHelper mandatoryParameterHelper, boolean isMPPFlow, boolean ranchIntegrationEnabled) {
    ProductCreationRequest productCreationRequest = new ProductCreationRequest();
    String storeId = mandatoryParameterHelper.getStoreId();
    BeanUtils
        .copyProperties(productCreationWebRequest, productCreationRequest, "productItemRequests",
            "productBusinessPartnerAttributes", "productCategories", "productAttributes",
            "productItems", "images");
    productCreationRequest.setStoreId(storeId);
    productCreationRequest.setCreatedDate(new Date());
    productCreationRequest.setUpdatedDate(new Date());
    productCreationRequest.setImagesUpdated(productCreationWebRequest.isImagesUpdated());
    productCreationRequest.setCreatedBy(mandatoryParameterHelper.getUsername());
    productCreationRequest.setUpdatedBy(mandatoryParameterHelper.getUsername());
    productCreationRequest.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    List<ProductItemCreationRequest> productItemRequests =
        productCreationWebRequest.getProductItemRequests().stream()
            .map(productItemRequest -> toProductItemCreationRequest(storeId, productItemRequest,
                productCreationWebRequest.getProductItemLogisticsWebRequests(), isMPPFlow, ranchIntegrationEnabled))
            .collect(Collectors.toList());
    productCreationRequest.setProductItemRequests(productItemRequests);
    productCreationRequest.setProductBusinessPartnerAttributes(
        toProductBusinessPartnerAttributeListRequest(
            storeId, productCreationWebRequest.getProductBusinessPartnerAttributes()));
    productCreationRequest.setImages(toImageList(storeId, productCreationWebRequest.getImages()));
    productCreationRequest.setCommonImages(toImageList(storeId, productCreationWebRequest.getCommonImages()));
    productCreationRequest.setProductItems(toProductItemRequests(storeId, productCreationWebRequest.getProductItems()));
    productCreationRequest.setProductCategories(toProductCategoryRequests(storeId, productCreationWebRequest.getProductCategories()));
    productCreationRequest.setProductAttributes(toProductAttributeRequests(storeId, productCreationWebRequest.getProductAttributes()));
    productCreationRequest.setForReview(productCreationWebRequest.isForReview());
    productCreationRequest.setFreeSample(productCreationWebRequest.isFreeSample());
    productCreationRequest.setOff2OnChannelActive(productCreationWebRequest.isOff2OnChannelActive());
    productCreationRequest.setOnline(productCreationWebRequest.isOnline());
    productCreationRequest.setExternalUser(productCreationWebRequest.isExternalUser());
    productCreationRequest.setVideoAddEditRequest(toVideoAddEditRequest(productCreationWebRequest));
    productCreationRequest.setVideoUpdated(productCreationWebRequest.getVideoUpdated());
    if (Objects.nonNull(productCreationWebRequest.getPreOrder())) {
      PreOrderRequest preOrderRequest = new PreOrderRequest();
      BeanUtils.copyProperties(productCreationWebRequest.getPreOrder(), preOrderRequest);
      productCreationRequest.setPreOrder(preOrderRequest);
    }
    if (isMPPFlow) {
      if (CollectionUtils.isNotEmpty(productCreationWebRequest.getProductItemLogisticsWebRequests())) {
        productCreationRequest.setProductItemLogisticsRequests(
            productCreationWebRequest.getProductItemLogisticsWebRequests().stream().map(
                productItemLogisticsWebRequest -> ProductItemLogisticsRequest.builder()
                    .logisticProductCode(productItemLogisticsWebRequest.getLogisticProductCode())
                    .isSelected(productItemLogisticsWebRequest.isSelected()).build()).collect(Collectors.toList()));
      }
    }
    if (ranchIntegrationEnabled) {
      productCreationRequest.setDistributionInfoRequest(productCreationWebRequest.getDistributionInfoRequest());
    }
    return productCreationRequest;
  }

  private static VideoAddEditRequest toVideoAddEditRequest(ProductCreationWebRequest productCreationWebRequest) {
    return Optional.ofNullable(productCreationWebRequest.getVideoAddEditRequest()).map(source -> {
      VideoAddEditRequest videoAddEditRequestForPBP = new VideoAddEditRequest();
      BeanUtils.copyProperties(source, videoAddEditRequestForPBP);
      return videoAddEditRequestForPBP;
    }).orElse(null);
  }

  private static ProductItemCreationRequest toProductItemCreationRequest(String storeId,
      ProductItemCreationWebRequest productItemCreationWebRequest,
      List<ProductItemLogisticsWebRequest> productItemLogisticsWebRequests, boolean isMPPFlow,
      boolean ranchIntegrationEnabled) {
    ProductItemCreationRequest productItemRequest = new ProductItemCreationRequest();
    BeanUtils.copyProperties(productItemCreationWebRequest, productItemRequest, "images",
        "attributesMap" ,"productItemAttributeValueRequests");
    productItemRequest.setContentChanged(productItemCreationWebRequest.isContentChanged());
    TreeMap<String, String> attributeMap = productItemCreationWebRequest.getAttributesMap();
    productItemRequest.setUpcCode(productItemCreationWebRequest.getUpcCode());
    productItemRequest.setAttributesMap(attributeMap);
    productItemRequest.setImages(toImageList(storeId, productItemCreationWebRequest.getImages()));
    productItemRequest.setProductItemAttributeValueRequests(toProductItemAttributeValueRequests(storeId,
        productItemCreationWebRequest.getProductItemAttributeValueRequests()));
    if (isMPPFlow) {
      List<PickupPointCreateRequest> pickupPointCreateRequests = new ArrayList<>();
      for (PickupPointCreateWebRequest pickupPointCreateWebRequest : productItemCreationWebRequest.getPickupPoints()) {
        PickupPointCreateRequest pickupPointCreateRequest = new PickupPointCreateRequest();
        BeanUtils.copyProperties(pickupPointCreateWebRequest, pickupPointCreateRequest);
        if (CollectionUtils.isNotEmpty(pickupPointCreateWebRequest.getProductItemWholesalePriceRequests())) {
          pickupPointCreateRequest.setProductItemWholesalePriceRequests(
              pickupPointCreateWebRequest.getProductItemWholesalePriceRequests().stream()
                  .map(ConverterUtil::toProductItemWholesalePriceRequest).collect(Collectors.toList()));
        }
        if(Objects.nonNull(pickupPointCreateWebRequest.getB2bFields())){
          pickupPointCreateRequest.setB2bFields(toB2bFieldsRequest(pickupPointCreateWebRequest.getB2bFields()));
        }
        pickupPointCreateRequests.add(pickupPointCreateRequest);
      }
      productItemRequest.setPickupPoints(pickupPointCreateRequests);
      productItemRequest.setBundleRecipe(Optional.ofNullable(productItemCreationWebRequest.getBundleRecipe())
          .map(ConverterUtil::toBundleRecipeRequests).orElse(new HashSet<>()));
    } else {
      if (CollectionUtils.isNotEmpty(productItemCreationWebRequest.getProductItemWholesalePriceRequests())) {
        productItemRequest.setProductItemWholesalePriceRequests(
            productItemCreationWebRequest.getProductItemWholesalePriceRequests().stream()
                .map(ConverterUtil::toProductItemWholesalePriceRequest).collect(Collectors.toList()));
      }
      if (CollectionUtils.isNotEmpty(productItemLogisticsWebRequests)) {
        productItemRequest.setProductItemLogisticsRequests(productItemLogisticsWebRequests.stream().map(
            productItemLogisticsWebRequest -> ProductItemLogisticsRequest.builder()
                .logisticProductCode(productItemLogisticsWebRequest.getLogisticProductCode())
                .isSelected(productItemLogisticsWebRequest.isSelected()).build()).collect(Collectors.toList()));
      }
    }
    if (ranchIntegrationEnabled && Objects.nonNull(productItemCreationWebRequest.getDistributionItemInfoRequest())) {
      DistributionItemRequest distributionItemRequest = new DistributionItemRequest();
      BeanUtils.copyProperties(productItemCreationWebRequest.getDistributionItemInfoRequest(), distributionItemRequest);
      productItemRequest.setDistributionItemInfoRequest(distributionItemRequest);
      List<DimensionAndUomRequest> dimensionAndUomRequests = new ArrayList<>();
      if (CollectionUtils.isNotEmpty(productItemCreationWebRequest.getDimensionsAndUOMRequest())) {
        for (DimensionAndUomWebRequest dimensionAndUomWebRequest : productItemCreationWebRequest.getDimensionsAndUOMRequest()) {
          DimensionAndUomRequest dimensionAndUomRequest = new DimensionAndUomRequest();
          BeanUtils.copyProperties(dimensionAndUomWebRequest, dimensionAndUomRequest);
          dimensionAndUomRequests.add(dimensionAndUomRequest);
        }
      }
      productItemRequest.setDimensionsAndUOMRequest(dimensionAndUomRequests);
    }
    return productItemRequest;
  }

  private static ProductItemWholesalePriceRequest toProductItemWholesalePriceRequest(ProductItemWholesalePriceWebRequest productItemWholesalePriceWebRequest) {
    ProductItemWholesalePriceRequest productItemWholesalePriceRequest = new ProductItemWholesalePriceRequest();
    BeanUtils.copyProperties(productItemWholesalePriceWebRequest, productItemWholesalePriceRequest);
    return productItemWholesalePriceRequest;
  }

  private static B2bDetailsDTO toB2bFieldsRequest(B2bFieldsWebRequest b2bFieldsWebRequest) {
    B2bDetailsDTO b2bDetailsDTO = new B2bDetailsDTO();
    BeanUtils.copyProperties(b2bFieldsWebRequest, b2bDetailsDTO);
    return b2bDetailsDTO;
  }


  private static List<ProductAttributeRequest> toProductAttributeRequests(
      String storeId, List<ProductAttributeWebRequest> productAttributeWebRequests) {
    return Optional.ofNullable(productAttributeWebRequests)
        .orElseGet(Collections::emptyList)
        .stream().map(productAttributeWebRequest -> toProductAttributeRequest(storeId, productAttributeWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductAttributeRequest toProductAttributeRequest(
      String storeId, ProductAttributeWebRequest productAttributeWebRequest) {
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    BeanUtils.copyProperties(productAttributeWebRequest, productAttributeRequest, "attribute",
        "productAttributeValues");
    productAttributeRequest.setStoreId(storeId);
    productAttributeRequest
        .setAttribute(toAttributeRequest(storeId, productAttributeWebRequest.getAttribute()));
    productAttributeRequest.setProductAttributeValues(
        toProductAttributeValues(productAttributeWebRequest.getProductAttributeValues()));
    return productAttributeRequest;
  }

  private static List<ProductAttributeValueRequest> toProductAttributeValues(
      List<ProductAttributeValueWebRequest> productAttributeValueWebRequests) {
    return productAttributeValueWebRequests.stream()
        .map(ConverterUtil::toProductAttributeValueWebRequest).collect(Collectors.toList());
  }

  private static ProductAttributeValueRequest toProductAttributeValueWebRequest(
      ProductAttributeValueWebRequest productAttributeValueWebRequest){
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    BeanUtils.copyProperties(productAttributeValueWebRequest, productAttributeValueRequest,
        "allowedAttributeValue", "descriptiveAttributeValueType",
        "predefinedAllowedAttributeValue");
    productAttributeValueRequest.setAllowedAttributeValue(
        toAllowedAttributeValue(productAttributeValueWebRequest.getAllowedAttributeValue()));
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(
        toPredefinedAllowedAttributeValue(
            productAttributeValueWebRequest.getPredefinedAllowedAttributeValue()));
    if (Objects.nonNull(productAttributeValueWebRequest.getDescriptiveAttributeValueType())) {
      productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType
          .valueOf(productAttributeValueWebRequest.getDescriptiveAttributeValueType().name()));
    }
    return productAttributeValueRequest;
  }

  private static List<ProductCategoryRequest> toProductCategoryRequests(
      String storeId, List<ProductCategoryWebRequest> productCategoryWebRequests) {
    return Optional.ofNullable(productCategoryWebRequests)
        .orElseGet(Collections::emptyList)
        .stream().map(productCategoryWebRequest -> toProductCategoryRequest(storeId, productCategoryWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductCategoryRequest toProductCategoryRequest(
      String storeId, ProductCategoryWebRequest productCategoryWebRequest){
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    CategoryRequest categoryRequest = new CategoryRequest();
    BeanUtils.copyProperties(productCategoryWebRequest, categoryRequest, "catalog");
    categoryRequest.setStoreId(storeId);
    categoryRequest.setCatalog(toCatalogRequest(productCategoryWebRequest.getCatalog()));
    productCategoryRequest.setCategory(categoryRequest);
    return productCategoryRequest;
  }

  private static CatalogRequest toCatalogRequest(CatalogWebRequest catalogWebRequest) {
    CatalogRequest catalogRequest = new CatalogRequest();
    BeanUtils.copyProperties(catalogWebRequest, catalogRequest);
    return catalogRequest;
  }

  private static List<ProductItemRequest> toProductItemRequests(String storeId,
      List<ProductItemWebRequest> productItemWebRequests) {
    return Optional.ofNullable(productItemWebRequests)
        .orElseGet(Collections::emptyList)
        .stream().map(productItemWebRequest -> toProductItemRequest(storeId, productItemWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductItemRequest toProductItemRequest(String storeId, ProductItemWebRequest productItemWebRequest){
    ProductItemRequest productItemRequest = new ProductItemRequest();
    BeanUtils
        .copyProperties(productItemWebRequest, productItemRequest, "productItemAttributeValues",
            "images", "attributesMap", "bundleRecipe");
    productItemRequest.setImages(toImageList(storeId, productItemWebRequest.getImages()));
    productItemRequest.setAttributesMap(productItemWebRequest.getAttributesMap());
    productItemRequest.setStoreId(storeId);
    productItemRequest.setProductItemAttributeValues(
        toProductItemAttributeValueRequests(storeId, productItemWebRequest.getProductItemAttributeValues()));
    return productItemRequest;
  }

  private static Set<BundleRecipeRequest> toBundleRecipeRequests(Set<BundleRecipeWebRequest> bundleRecipeWebRequests) {
    Set<BundleRecipeRequest> bundleRecipeRequests = new HashSet<>();
    for (BundleRecipeWebRequest bundleRecipeWebRequest : bundleRecipeWebRequests) {
      BundleRecipeRequest bundleRecipeRequest = new BundleRecipeRequest();
      BeanUtils.copyProperties(bundleRecipeWebRequest, bundleRecipeRequest);
      bundleRecipeRequests.add(bundleRecipeRequest);
    }
    return bundleRecipeRequests;
  }

  private static List<ProductItemAttributeValueRequest> toProductItemAttributeValueRequests(
      String storeId, List<ProductItemAttributeValueWebRequest> productItemAttributeValueWebRequests){
    return Optional.ofNullable(productItemAttributeValueWebRequests)
        .orElseGet(Collections::emptyList).stream()
        .map(productItemAttributeValueWebRequest -> toProductItemAttributeValueRequest(storeId, productItemAttributeValueWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductItemAttributeValueRequest toProductItemAttributeValueRequest(
      String storeId, ProductItemAttributeValueWebRequest productItemAttributeValueWebRequest) {
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        new ProductItemAttributeValueRequest();
    BeanUtils.copyProperties(productItemAttributeValueWebRequest, productItemAttributeValueRequest, "attribute");
    productItemAttributeValueRequest
        .setAttribute(toAttributeRequest(storeId, productItemAttributeValueWebRequest.getAttribute()));
    return productItemAttributeValueRequest;
  }

  private static AttributeRequest toAttributeRequest(String storeId, AttributeWebRequest attributeWebRequest) {
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(attributeWebRequest, attributeRequest, "attributeType", "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    if (Objects.isNull(attributeWebRequest.getAttributeType())) {
      log.error("Invalid attribute type: {} ", attributeWebRequest);
      throw new ValidationException(ErrorMessages.INVALID_ATTRIBUTE + Optional.ofNullable(attributeWebRequest.getName())
          .orElse(StringUtils.EMPTY));
    }
    attributeRequest.setStoreId(storeId);
    attributeRequest.setAttributeType(AttributeType.valueOf(attributeWebRequest.getAttributeType().name()));
    attributeRequest.setAllowedAttributeValues(
        toAllowedAttributeValues(attributeWebRequest.getAllowedAttributeValues()));
    attributeRequest.setPredefinedAllowedAttributeValues(
        toPredefinedAllowedAttributeValues(attributeWebRequest.getPredefinedAllowedAttributeValues()));
    return attributeRequest;
  }

  private static List<PredefinedAllowedAttributeValueRequest> toPredefinedAllowedAttributeValues(
      List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValueWebRequests) {
    return Optional.ofNullable(predefinedAllowedAttributeValueWebRequests)
        .orElseGet(Collections::emptyList).stream()
        .map(ConverterUtil::toPredefinedAllowedAttributeValue)
        .collect(Collectors.toList());
  }

  private static PredefinedAllowedAttributeValueRequest toPredefinedAllowedAttributeValue(
      PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValueWebRequest) {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    if(Objects.nonNull(predefinedAllowedAttributeValueWebRequest))
    BeanUtils.copyProperties(predefinedAllowedAttributeValueWebRequest,
        predefinedAllowedAttributeValueRequest);
    return predefinedAllowedAttributeValueRequest;
  }

  private static List<AllowedAttributeValueRequest> toAllowedAttributeValues(
      List<AllowedAttributeValueWebRequest> allowedAttributeValueWebRequests) {
    return Optional.ofNullable(allowedAttributeValueWebRequests)
        .orElseGet(Collections::emptyList).stream()
        .map(ConverterUtil::toAllowedAttributeValue)
        .collect(Collectors.toList());
  }

  private static AllowedAttributeValueRequest toAllowedAttributeValue(
      AllowedAttributeValueWebRequest allowedAttributeValueWebRequest) {
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    if(Objects.nonNull(allowedAttributeValueWebRequest)) {
      BeanUtils.copyProperties(allowedAttributeValueWebRequest, allowedAttributeValueRequest);
    }
    return allowedAttributeValueRequest;
  }

  private static List<Image> toImageList(String storeId, List<ImageRequest> imageRequests) {
    return Optional.ofNullable(imageRequests)
        .orElseGet(Collections::emptyList).stream()
        .map(imageRequest -> toImage(storeId, imageRequest))
        .collect(Collectors.toList());
  }

  private static Image toImage(String storeId, ImageRequest imageRequest){
    Image image = new Image();
    BeanUtils.copyProperties(imageRequest, image);
    image.setActive(imageRequest.isActive());
    image.setStoreId(storeId);
    image.setCommonImage(imageRequest.isCommonImage());
    return image;
  }

  private static ProductBusinessPartnerAttributeRequest toProductBusinessPartnerAttributeRequest(
      String storeId, ProductBusinessPartnerAttributeWebRequest productBusinessPartnerAttributeWebRequest) {
    ProductBusinessPartnerAttributeRequest productBusinessPartnerAttributeRequest =
        new ProductBusinessPartnerAttributeRequest();
    BeanUtils.copyProperties(productBusinessPartnerAttributeWebRequest,
        productBusinessPartnerAttributeRequest);
    productBusinessPartnerAttributeRequest.setStoreId(storeId);
    return productBusinessPartnerAttributeRequest;
  }

  private static List<ProductBusinessPartnerAttributeRequest>
  toProductBusinessPartnerAttributeListRequest(
      String storeId, List<ProductBusinessPartnerAttributeWebRequest>
          productBusinessPartnerAttributeWebRequestList) {
    return Optional.ofNullable(productBusinessPartnerAttributeWebRequestList)
        .orElseGet(Collections::emptyList).stream()
        .map(productBusinessPartnerAttributeWebRequest -> toProductBusinessPartnerAttributeRequest(
                storeId, productBusinessPartnerAttributeWebRequest))
        .collect(Collectors.toList());
  }

  public static UploadImageRequest toUploadImageRequest(String imageFileName, String productCode, byte[] bytes,
      boolean active, boolean retryRequest, String originalFileName) {
    UploadImageRequest request =
        UploadImageRequest.builder().imageFileName(imageFileName).productCode(productCode).bytes(bytes).active(active)
            .retryRequest(retryRequest)
            .originalFileType(originalFileName.substring(originalFileName.lastIndexOf(Constants.DOT) + 1)).build();
    return request;
  }

  public static ProductCopyRequest toProductCopyRequest(CopyProductItemWebRequest webRequest, MandatoryParameterHelper mandatoryParameterHelper) {
    ProductCopyRequest request = new ProductCopyRequest();
    request.setBusinessPartnerCode(mandatoryParameterHelper.getBusinessPartnerCode());
    request.setSourceBusinessPartnerCode(mandatoryParameterHelper.getLinkedBusinessPartnerCode());
    request.setGdnItemSkus(Objects.nonNull(webRequest.getItemSkus()) ? webRequest.getItemSkus() : Collections.emptyMap());
    return request;
  }

  public static String getFilterUSP(String uniqueSellingPoint) {
    String usp = getUSPWithoutTags(PATTERN_FOR_NON_ASCII_CHARS_AND_NEW_LINE,
        getUSPWithoutTags(PATTERN_FOR_HTML_TAGS_AND_NEW_LINE_AND_BULLET_POINT, uniqueSellingPoint, StringUtils.EMPTY),
        StringUtils.EMPTY);
    return getUSPWithoutTags(PATTERN_FOR_EXTRA_SPACE, usp, StringUtils.SPACE);
  }

  private static String getUSPWithoutTags(Pattern pattern, String usp, String replace) {
    return pattern.matcher(usp).replaceAll(replace);
  }

  public static SubmitEvidenceIPRRequest convertToSubmitEvidenceIPRRequest(
      SubmitEvidenceIPRWebRequest submitEvidenceIPRWebRequest){
    SubmitEvidenceIPRRequest submitEvidenceIPRRequest = new SubmitEvidenceIPRRequest();
    BeanUtils.copyProperties(submitEvidenceIPRWebRequest, submitEvidenceIPRRequest);
    submitEvidenceIPRRequest.setEvidenceSubmittedNotes(submitEvidenceIPRWebRequest.getNotes());
    submitEvidenceIPRRequest.setEvidenceFilePath(
        Optional.ofNullable(submitEvidenceIPRWebRequest.getEvidenceFilePath()).map(
                evidenceFilePath -> Arrays.asList(
                    evidenceFilePath.split(Constants.COMMA_DELIMITER_NO_SPACE)))
            .orElseGet(Collections::emptyList));
    submitEvidenceIPRRequest.setEvidenceUrl(
        Optional.ofNullable(submitEvidenceIPRWebRequest.getEvidenceUrl()).map(
                evidenceUrl -> Arrays.asList(evidenceUrl.split(Constants.COMMA_DELIMITER_NO_SPACE)))
            .orElseGet(Collections::emptyList));
    return submitEvidenceIPRRequest;
  }

  public static DistributionInfoUpdateRequest convertToDistributionInfoUpdateRequest(
      DistributionInfoWebRequest distributionInfoWebRequest, String businessPartnerCode) {
    DistributionInfoUpdateRequest distributionInfoUpdateRequest = new DistributionInfoUpdateRequest();
    distributionInfoUpdateRequest.setSellerCode(businessPartnerCode);
    distributionInfoUpdateRequest.setDistributionInfoRequest(
        distributionInfoWebRequest.getDistributionInfoRequest());
    distributionInfoUpdateRequest.setProductItems(
        distributionInfoWebRequest.getProductItems().stream()
            .map(ConverterUtil::convertToProductItemUomInfoDTO).collect(Collectors.toList()));
    return distributionInfoUpdateRequest;
  }

  public static ProductItemUomInfoDTO convertToProductItemUomInfoDTO(
      ProductItemUomInfoWebRequest productItemUomInfoWebRequest) {
    ProductItemUomInfoDTO productItemUomInfoDTO = new ProductItemUomInfoDTO();
    productItemUomInfoDTO.setSkuCode(productItemUomInfoWebRequest.getSkuCode());
    productItemUomInfoDTO.setDistributionItemInfoRequest(
        productItemUomInfoWebRequest.getDistributionItemInfoRequest());
    productItemUomInfoDTO.setDimensionsAndUOMRequest(productItemUomInfoWebRequest.getDimensionsAndUOMRequest());
    return productItemUomInfoDTO;
  }
}
