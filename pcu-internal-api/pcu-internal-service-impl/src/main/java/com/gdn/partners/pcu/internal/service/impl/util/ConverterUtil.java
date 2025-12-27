package com.gdn.partners.pcu.internal.service.impl.util;

import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.UUID;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthCreateWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthHistoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.UploadImageRequest;
import com.gdn.partners.pcu.internal.web.model.response.CategoryHistoryWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.response.CategoryHistoryResponse;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.web.model.request.AllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.AttributeTypeWeb;
import com.gdn.partners.pcu.internal.web.model.request.AttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CatalogWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.CategoryConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ImageRequest;
import com.gdn.partners.pcu.internal.web.model.request.ItemNotesWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.MerchantConfigurationWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.PredefinedAllowedAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductAttributeWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductCategoryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemAttributeValueWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductItemWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductReturnForCorrectionWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.ProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RejectReason;
import com.gdn.partners.pcu.internal.web.model.request.UpdateBrandWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductAttributeRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductDetailRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductImageRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.DistributionProductItemRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ItemNotesRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.ProductNotesRequest;
import com.gdn.x.mta.distributiontask.util.RejectReasonRequest;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.brand.UpdateBrandRequest;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;

/**
 * Created by govind on 11/01/2019 AD.
 */
public class ConverterUtil {

  private ConverterUtil() {}

  public static ProductReturnForCorrectionRequest toProductReturnForCorrectionRequest(
      ProductReturnForCorrectionWebRequest productReturnForCorrectionWebRequest){
    ProductReturnForCorrectionRequest productReturnForCorrectionRequest = new ProductReturnForCorrectionRequest();
    BeanUtils.copyProperties(productReturnForCorrectionWebRequest, productReturnForCorrectionRequest);
    return productReturnForCorrectionRequest;
  }

  public static ProductRequest toProductRequest(ProductWebRequest productWebRequest,
      ClientParameterHelper clientParameterHelper, boolean productNameTrimSwitch,
    boolean webpConversionEnabled) {
    String userType = clientParameterHelper.getUserType();
    if (Constants.USER_TYPE_EXTERNAL.equals(userType)) {
      setRequestId(clientParameterHelper);
    }
    ProductRequest productRequest = new ProductRequest();
    String storeId = clientParameterHelper.getStoreId();
    BeanUtils
        .copyProperties(productWebRequest, productRequest, "productCategories", "productAttributes",
            "productItems", "images");
    
    if (webpConversionEnabled) {
      setWebpExtensionForNewImages(productWebRequest);
    }
    productRequest.setImages(toImageList(storeId, productWebRequest.getImages()));
    productRequest.setProductItems(toProductItemRequests(storeId, productWebRequest.getProductItems()));
    productRequest
        .setProductCategories(toProductCategoryRequests(storeId, productWebRequest.getProductCategories()));
    productRequest
        .setProductAttributes(toProductAttributeRequests(storeId, productWebRequest.getProductAttributes()));
    productRequest.setStoreId(storeId);
    productRequest.setUpdatedDate(Calendar.getInstance().getTime());
    productRequest.setUpdatedBy(clientParameterHelper.getUsername());
    if (productNameTrimSwitch) {
      String productName = productWebRequest.getName().trim();
      productRequest.setName(productName);
    }
    return productRequest;
  }

  private static List<ProductItemRequest> toProductItemRequests(String storeId,
      List<ProductItemWebRequest> productItemWebRequests) {
    return Optional.ofNullable(productItemWebRequests)
        .orElseGet(Collections::emptyList).stream().map(productItemWebRequest -> toProductItemRequest(storeId, productItemWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductItemRequest toProductItemRequest(String storeId, ProductItemWebRequest productItemWebRequest){
    ProductItemRequest productItemRequest = new ProductItemRequest();
    BeanUtils
        .copyProperties(productItemWebRequest, productItemRequest, "productItemAttributeValues",
            "images", "attributesMap");
    productItemRequest.setImages(toImageList(storeId, productItemWebRequest.getImages()));
    productItemRequest.setAttributesMap(productItemWebRequest.getAttributesMap());
    productItemRequest.setProductItemAttributeValues(
        toProductItemAttributeValueRequests(storeId, productItemWebRequest.getProductItemAttributeValues()));
    productItemRequest.setStoreId(storeId);
    return productItemRequest;
  }

  private static List<ProductItemAttributeValueRequest> toProductItemAttributeValueRequests(String storeId,
      List<ProductItemAttributeValueWebRequest> productItemAttributeValueWebRequests) {
    return Optional.ofNullable(productItemAttributeValueWebRequests)
        .orElseGet(Collections::emptyList).stream().map(productItemAttributeValueWebRequest -> ConverterUtil
            .toProductItemAttributeValueRequest(storeId, productItemAttributeValueWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductItemAttributeValueRequest toProductItemAttributeValueRequest(String storeId,
      ProductItemAttributeValueWebRequest productItemAttributeValueWebRequest) {
    ProductItemAttributeValueRequest productItemAttributeValueRequest =
        new ProductItemAttributeValueRequest();
    BeanUtils.copyProperties(productItemAttributeValueWebRequest, productItemAttributeValueRequest, "attribute");
    productItemAttributeValueRequest
        .setAttribute(toAttributeRequest(storeId, productItemAttributeValueWebRequest.getAttribute()));
    return productItemAttributeValueRequest;
  }

  private static AttributeRequest toAttributeRequest(String storeId, AttributeWebRequest attributeWebRequest) {
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(attributeWebRequest, attributeRequest, "attributeType",
        "allowedAttributeValues", "predefinedAllowedAttributeValues");
    attributeRequest
        .setAttributeType(AttributeType.valueOf(attributeWebRequest.getAttributeType().name()));
    attributeRequest.setAllowedAttributeValues(toAllowedAttributeValues(attributeWebRequest.getAllowedAttributeValues()));
    attributeRequest.setPredefinedAllowedAttributeValues(toPredefinedAllowedAttributeValues(storeId,
        attributeWebRequest.getPredefinedAllowedAttributeValues()));
    return attributeRequest;
  }

  private static List<PredefinedAllowedAttributeValueRequest> toPredefinedAllowedAttributeValues(String storeId,
      List<PredefinedAllowedAttributeValueWebRequest> predefinedAllowedAttributeValueWebRequests) {
    return Optional.ofNullable(predefinedAllowedAttributeValueWebRequests)
        .orElseGet(Collections::emptyList).stream()
        .map(predefinedAllowedAttributeValueWebRequest -> toPredefinedAllowedAttributeValue(storeId,
            predefinedAllowedAttributeValueWebRequest))
        .collect(Collectors.toList());
  }

  private static PredefinedAllowedAttributeValueRequest toPredefinedAllowedAttributeValue(String storeId,
      PredefinedAllowedAttributeValueWebRequest predefinedAllowedAttributeValueWebRequest) {
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    if (Objects.nonNull(predefinedAllowedAttributeValueWebRequest)) {
      BeanUtils.copyProperties(predefinedAllowedAttributeValueWebRequest,
          predefinedAllowedAttributeValueRequest);
    }
    predefinedAllowedAttributeValueRequest.setStoreId(storeId);
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
    image.setStoreId(storeId);
    BeanUtils.copyProperties(imageRequest, image);
    return image;
  }

  private static List<ProductCategoryRequest> toProductCategoryRequests(String storeId,
      List<ProductCategoryWebRequest> productCategoryWebRequests) {
    return Optional.ofNullable(productCategoryWebRequests)
        .orElseGet(Collections::emptyList).stream().map(productCategoryWebRequest ->toProductCategoryRequest(storeId, productCategoryWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductCategoryRequest toProductCategoryRequest(
      String storeId, ProductCategoryWebRequest productCategoryWebRequest){
    ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
    productCategoryRequest.setStoreId(storeId);
    CategoryRequest categoryRequest = new CategoryRequest();
    BeanUtils.copyProperties(productCategoryWebRequest, categoryRequest, "catalog");
    categoryRequest.setStoreId(storeId);
    categoryRequest.setCatalog(toCatalogRequest(productCategoryWebRequest.getCatalog()));
    productCategoryRequest.setCategory(categoryRequest);
    productCategoryRequest.setMarkForDelete(productCategoryWebRequest.isMarkForDelete());
    return productCategoryRequest;
  }

  private static CatalogRequest toCatalogRequest(CatalogWebRequest catalogWebRequest) {
    CatalogRequest catalogRequest = new CatalogRequest();
    BeanUtils.copyProperties(catalogWebRequest, catalogRequest);
    return catalogRequest;
  }

  private static ProductAttributeRequest toProductAttributeRequest(
      String storeId, ProductAttributeWebRequest productAttributeWebRequest) {
    ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
    BeanUtils.copyProperties(productAttributeWebRequest, productAttributeRequest, "attribute",
        "productAttributeValues");
    productAttributeRequest
        .setAttribute(toAttributeRequest(storeId, productAttributeWebRequest.getAttribute()));
    productAttributeRequest.setStoreId(storeId);
    productAttributeRequest.setProductAttributeValues(
        toProductAttributeValues(storeId, productAttributeWebRequest.getProductAttributeValues()));
    return productAttributeRequest;
  }

  public static List<ProductAttributeRequest> toProductAttributeRequests(
    String storeId, List<ProductAttributeWebRequest> productAttributeWebRequests) {
    return Optional.ofNullable(productAttributeWebRequests)
      .orElseGet(Collections::emptyList).stream()
      .filter(productAttributeWebRequest -> !shouldSkip(productAttributeWebRequest)) // Skip the undesired ones
      .map(productAttributeWebRequest -> toProductAttributeRequest(storeId, productAttributeWebRequest))
      .collect(Collectors.toList());
  }


  private static boolean shouldSkip(ProductAttributeWebRequest request) {
    AttributeTypeWeb attributeTypeWeb = request.getAttribute().getAttributeType();
    boolean isExtractionEnabled = request.getAttribute().isDsExtraction();

    if (!isExtractionEnabled) {
      return false;
    }

    List<ProductAttributeValueWebRequest> productAttributeValues =
      request.getProductAttributeValues();

    // For DESCRIPTIVE types
    if (attributeTypeWeb == AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE
      || attributeTypeWeb == AttributeTypeWeb.DESCRIPTIVE_MULTIVALUE) {
      return productAttributeValues.stream()
        .map(ProductAttributeValueWebRequest::getDescriptiveAttributeValue)
        .allMatch(value -> StringUtils.isEmpty(value) || Constants.HYPHEN.equals(value));
    }

    // For PREDEFINED types
    if (attributeTypeWeb == AttributeTypeWeb.PREDEFINED_ATTRIBUTE
      || attributeTypeWeb == AttributeTypeWeb.PREDEFINED_MULTIVALUE) {
      return productAttributeValues.stream().map(
        value -> value.getPredefinedAllowedAttributeValue() != null ?
          value.getPredefinedAllowedAttributeValue().getValue() :
          null).allMatch(val -> StringUtils.isEmpty(val) || Constants.HYPHEN.equals(val));
    }
    return false;
  }


  private static List<ProductAttributeValueRequest> toProductAttributeValues(String storeId,
      List<ProductAttributeValueWebRequest> productAttributeValueWebRequests) {
    return productAttributeValueWebRequests.stream().map(
        productAttributeValueWebRequest -> toProductAttributeValueWebRequest(storeId, productAttributeValueWebRequest))
        .collect(Collectors.toList());
  }

  private static ProductAttributeValueRequest toProductAttributeValueWebRequest(String storeId,
      ProductAttributeValueWebRequest productAttributeValueWebRequest){
    ProductAttributeValueRequest productAttributeValueRequest = new ProductAttributeValueRequest();
    BeanUtils.copyProperties(productAttributeValueWebRequest, productAttributeValueRequest,
        "allowedAttributeValue", "descriptiveAttributeValueType",
        "predefinedAllowedAttributeValue");
    productAttributeValueRequest.setAllowedAttributeValue(
        toAllowedAttributeValue(productAttributeValueWebRequest.getAllowedAttributeValue()));
    productAttributeValueRequest.setPredefinedAllowedAttributeValue(toPredefinedAllowedAttributeValue(storeId,
        productAttributeValueWebRequest.getPredefinedAllowedAttributeValue()));
    productAttributeValueRequest.setDescriptiveAttributeValueType(DescriptiveAttributeValueType
        .valueOf(productAttributeValueWebRequest.getDescriptiveAttributeValueType().name()));
    return productAttributeValueRequest;
  }

  public static void setRequestId(ClientParameterHelper clientParameterHelper) {
    String username = clientParameterHelper.getUsername();
    String businessPartnerCode = clientParameterHelper.getBusinessPartnerCode();
    clientParameterHelper
        .set(Constants.REQUEST_ID, username + Constants.HYPHEN + businessPartnerCode + Constants.HYPHEN + UUID.randomUUID().toString());
  }

  public static UpdateBrandRequest toUpdateBrandRequest(UpdateBrandWebRequest updateBrandWebRequest) {
    UpdateBrandRequest updateBrandRequest = new UpdateBrandRequest();
    BeanUtils.copyProperties(updateBrandWebRequest, updateBrandRequest);
    return updateBrandRequest;
  }

  public static DistributionProductDetailRequest convertProductWebRequestToDistributionProductRequest(
    ProductWebRequest productWebRequest, ClientParameterHelper clientParameterHelper) {
    DistributionProductDetailRequest distributionProductDetailRequest =
        new DistributionProductDetailRequest();
    BeanUtils.copyProperties(productWebRequest, distributionProductDetailRequest, "productCategories",
        "productAttributes", "productItems", "images");
    distributionProductDetailRequest.setProductName(productWebRequest.getName());
    distributionProductDetailRequest.setVideoUrl(productWebRequest.getUrl());
    distributionProductDetailRequest.setNotes(productWebRequest.getNotes());
    distributionProductDetailRequest.setStoreId(clientParameterHelper.getStoreId());
    distributionProductDetailRequest.setUpdatedDate(Calendar.getInstance().getTime());
    distributionProductDetailRequest.setUpdatedBy(clientParameterHelper.getUsername());
    distributionProductDetailRequest.setProductType(productWebRequest.getProductType());
    Map<String, ItemNotesWebRequest> skuCodeItemNotesMap = generateItemNotes(productWebRequest);
    setProductImages(productWebRequest, distributionProductDetailRequest);
    setProductAttributes(productWebRequest, distributionProductDetailRequest);
    setProductItems(productWebRequest, distributionProductDetailRequest, skuCodeItemNotesMap);
    setCategoryInfo(productWebRequest, distributionProductDetailRequest);
    setProductNotes(productWebRequest, distributionProductDetailRequest);
    return distributionProductDetailRequest;
  }

  private static void setWebpExtensionForNewImages(ProductWebRequest productWebRequest) {
    for (ImageRequest image : CollectionUtils.emptyIfNull(productWebRequest.getImages())) {
      if (Constants.NEW_IMAGE_TYPE.equals(image.getType())) {
        image.setLocationPath(
          FilenameUtils.removeExtension(image.getLocationPath()) + Constants.WEBP_FORMAT);
      }
    }

    for (ProductItemWebRequest item : CollectionUtils.emptyIfNull(
      productWebRequest.getProductItems())) {
      for (ImageRequest image : CollectionUtils.emptyIfNull(item.getImages())) {
        if (Constants.NEW_IMAGE_TYPE.equals(image.getType())) {
          image.setLocationPath(
            FilenameUtils.removeExtension(image.getLocationPath()) + Constants.WEBP_FORMAT);
        }
      }
    }
  }

  private static Map<String, ItemNotesWebRequest> generateItemNotes(ProductWebRequest productWebRequest) {
    Map<String, ItemNotesWebRequest> skuCodeItemNotesMap = new HashMap<>();
    if (Objects.nonNull(productWebRequest.getNeedRevisionNotes()) && CollectionUtils.isNotEmpty(
        productWebRequest.getNeedRevisionNotes().getItemNotes())) {
      skuCodeItemNotesMap = productWebRequest.getNeedRevisionNotes().getItemNotes().stream()
          .collect(Collectors.toMap(ItemNotesWebRequest::getSkuCode, Function.identity()));
    }
    return skuCodeItemNotesMap;
  }

  private static void setProductNotes(ProductWebRequest productWebRequest,
      DistributionProductDetailRequest distributionProductDetailRequest) {
    if (Objects.nonNull(productWebRequest.getNeedRevisionNotes())) {
      distributionProductDetailRequest.setProductNotes(
          ProductNotesRequest.builder().vendorNotes(productWebRequest.getNeedRevisionNotes().getVendorNotes())
              .allVariants(productWebRequest.getNeedRevisionNotes().getAllVariants())
              .contentAdditionalNotes(productWebRequest.getNeedRevisionNotes().getContentAdditionalNotes())
              .imagesAdditionalNotes(productWebRequest.getNeedRevisionNotes().getImagesAdditionalNotes())
              .imageReason(productWebRequest.getNeedRevisionNotes().getImageReason())
              .vendorErrorFields(productWebRequest.getNeedRevisionNotes().getVendorErrorFields()).build());
    }
  }

  private static void setCategoryInfo(ProductWebRequest productWebRequest,
      DistributionProductDetailRequest distributionProductDetailRequest) {
    if(CollectionUtils.isEmpty(productWebRequest.getProductCategories())){
      return;
    }
    distributionProductDetailRequest.setMarginExceeded(productWebRequest.isMarginExceed());
    distributionProductDetailRequest.setCategoryCode(productWebRequest.getProductCategories().get(0).getCategoryCode());
    distributionProductDetailRequest.setCategoryName(productWebRequest.getProductCategories().get(0).getName());
  }

  private static void setProductItems(ProductWebRequest productRequest,
      DistributionProductDetailRequest distributionProductDetailRequest,
      Map<String, ItemNotesWebRequest> skuCodeItemNotesMap) {
    if(CollectionUtils.isEmpty(productRequest.getProductItems())){
      return;
    }
    List<DistributionProductItemRequest> distributionProductItemRequestList = new ArrayList<>();
    for (ProductItemWebRequest productItemWebRequest : productRequest.getProductItems()) {
      DistributionProductItemRequest distributionProductItemRequest =
          new DistributionProductItemRequest();
      BeanUtils.copyProperties(productItemWebRequest, distributionProductItemRequest,
          "productItemAttributeValues", "images");
      setProductItemImages(productItemWebRequest, distributionProductItemRequest);
      setProductItemAttributes(productItemWebRequest, distributionProductItemRequest);
      setItemNotes(productItemWebRequest, distributionProductItemRequest, skuCodeItemNotesMap);
      distributionProductItemRequestList.add(distributionProductItemRequest);
    }
    distributionProductDetailRequest.setProductItems(distributionProductItemRequestList);
  }

  private static void setItemNotes(ProductItemWebRequest productItemWebRequest,
      DistributionProductItemRequest distributionProductItemRequest,
      Map<String, ItemNotesWebRequest> skuCodeItemNotesMap) {
    if (MapUtils.isNotEmpty(skuCodeItemNotesMap) && skuCodeItemNotesMap.containsKey(
        productItemWebRequest.getSkuCode())) {
      ItemNotesWebRequest itemNotesWebRequest = skuCodeItemNotesMap.get(productItemWebRequest.getSkuCode());
      distributionProductItemRequest.setItemNotes(
          ItemNotesRequest.builder().vendorNotes(itemNotesWebRequest.getVendorNotes())
              .skuCode(itemNotesWebRequest.getSkuCode()).itemName(itemNotesWebRequest.getItemName())
              .itemNumber(itemNotesWebRequest.getItemNumber())
              .vendorErrorFields(itemNotesWebRequest.getVendorErrorFields()).itemSku(itemNotesWebRequest.getItemSku())
              .build());
    }
  }

  private static void setProductItemAttributes(ProductItemWebRequest productItemRequest,
      DistributionProductItemRequest distributionProductItemRequest) {
    List<DistributionProductAttributeRequest> distributionProductItemAttributeRequestList =
        new ArrayList<>();
    if (CollectionUtils.isNotEmpty(productItemRequest.getProductItemAttributeValues())) {
      for (ProductItemAttributeValueWebRequest productItemAttributeValueRequest : productItemRequest
          .getProductItemAttributeValues()) {
        DistributionProductAttributeRequest distributionProductAttributeRequest =
            new DistributionProductAttributeRequest();
        BeanUtils.copyProperties(productItemAttributeValueRequest, distributionProductAttributeRequest, "attribute");
        distributionProductAttributeRequest.setValue(productItemAttributeValueRequest.getValue());
        AttributeWebRequest attributeRequest = productItemAttributeValueRequest.getAttribute();
        if (attributeRequest != null) {
          distributionProductAttributeRequest.setAttributeCode(attributeRequest.getAttributeCode());
          distributionProductAttributeRequest.setName(attributeRequest.getName());
          distributionProductAttributeRequest
              .setAttributeType(attributeRequest.getAttributeType().toString());
        }
        distributionProductItemAttributeRequestList.add(distributionProductAttributeRequest);
      }
    }
    distributionProductItemRequest
        .setProductItemAttributes(distributionProductItemAttributeRequestList);
  }

  private static void setProductItemImages(ProductItemWebRequest productItemRequest,
      DistributionProductItemRequest distributionProductItemRequest) {
    if(CollectionUtils.isEmpty(productItemRequest.getImages())){
      return;
    }
    List<DistributionProductImageRequest> distributionProductItemImageRequestList =
        new ArrayList<>();
    for (ImageRequest image : productItemRequest.getImages()) {
      if(!image.isMarkForDelete()){
        DistributionProductImageRequest distributionProductImageRequest =
            new DistributionProductImageRequest();
        BeanUtils.copyProperties(image, distributionProductImageRequest, "id");
        distributionProductImageRequest.setMainImage(image.isMainImages());
        distributionProductItemImageRequestList.add(distributionProductImageRequest);
      }
    }
    distributionProductItemRequest.setProductItemImages(distributionProductItemImageRequestList);
  }

  private static void setProductAttributes(ProductWebRequest productRequest,
      DistributionProductDetailRequest distributionProductDetailRequest) {
    if(CollectionUtils.isEmpty(productRequest.getProductAttributes())){
      return;
    }
    List<DistributionProductAttributeRequest> distributionProductAttributeRequestList =
        new ArrayList<>();
    for (ProductAttributeWebRequest productAttributeRequest : productRequest.getProductAttributes()) {
      DistributionProductAttributeRequest distributionProductAttributeRequest =
          new DistributionProductAttributeRequest();
      AttributeWebRequest attributeRequest = productAttributeRequest.getAttribute();
      BeanUtils.copyProperties(attributeRequest, distributionProductAttributeRequest, "id");
      distributionProductAttributeRequest
          .setName(productAttributeRequest.getProductAttributeName());
      ProductAttributeValueWebRequest productAttributeValueRequest =
          productAttributeRequest.getProductAttributeValues().get(0);
      setProductAttributeType(distributionProductAttributeRequest, attributeRequest,
          productAttributeValueRequest);
      distributionProductAttributeRequestList.add(distributionProductAttributeRequest);
    }
    distributionProductDetailRequest.setProductAttributes(distributionProductAttributeRequestList);
  }

  private static void setProductAttributeType(DistributionProductAttributeRequest distributionProductAttributeRequest,
      AttributeWebRequest attributeRequest,
      ProductAttributeValueWebRequest productAttributeValueRequest) {
    if (attributeRequest.getAttributeType().equals(AttributeTypeWeb.DEFINING_ATTRIBUTE)) {
      if (productAttributeValueRequest != null
          && productAttributeValueRequest.getAllowedAttributeValue() != null) {
        distributionProductAttributeRequest
            .setValue(productAttributeValueRequest.getAllowedAttributeValue().getValue());
      }
      distributionProductAttributeRequest
          .setAttributeType(AttributeTypeWeb.DEFINING_ATTRIBUTE.toString());
    } else if (attributeRequest.getAttributeType().equals(AttributeTypeWeb.PREDEFINED_ATTRIBUTE)) {
      if (productAttributeValueRequest != null
          && productAttributeValueRequest.getPredefinedAllowedAttributeValue() != null) {
        distributionProductAttributeRequest.setValue(
            productAttributeValueRequest.getPredefinedAllowedAttributeValue().getValue());
      }
      distributionProductAttributeRequest
          .setAttributeType(AttributeTypeWeb.PREDEFINED_ATTRIBUTE.toString());
    } else {
      if (productAttributeValueRequest != null) {
        distributionProductAttributeRequest
            .setValue(productAttributeValueRequest.getDescriptiveAttributeValue());
      }
      distributionProductAttributeRequest
          .setAttributeType(AttributeTypeWeb.DESCRIPTIVE_ATTRIBUTE.toString());
    }
  }

  private static void setProductImages(ProductWebRequest productRequest,
      DistributionProductDetailRequest distributionProductDetailRequest) {
    if(CollectionUtils.isEmpty(productRequest.getImages())){
      return;
    }
    List<DistributionProductImageRequest> distributionProductImageRequestList = new ArrayList<>();
    for (ImageRequest image : productRequest.getImages()) {
      if(!image.isMarkForDelete()){
        DistributionProductImageRequest distributionProductImageRequest =
            new DistributionProductImageRequest();
        BeanUtils.copyProperties(image, distributionProductImageRequest);
        distributionProductImageRequest.setMainImage(image.isMainImages());
        distributionProductImageRequestList.add(distributionProductImageRequest);
      }
    }
    distributionProductDetailRequest.setProductImages(distributionProductImageRequestList);
  }

  public static List<CategoryConfigurationRequest> toCategoryConfigurationListRequest(
      List<CategoryConfigurationWebRequest> categoryConfigurationWebRequestList) {
    return Optional.ofNullable(categoryConfigurationWebRequestList).orElseGet(Collections::emptyList).stream().map(
        categoryConfigurationWebRequest -> ConverterUtil
            .toCategoryConfigurationRequest(categoryConfigurationWebRequest)).collect(Collectors.toList());
  }

  private static CategoryConfigurationRequest toCategoryConfigurationRequest(
      CategoryConfigurationWebRequest categoryConfigurationWebRequest) {
    CategoryConfigurationRequest categoryConfigurationRequest = new CategoryConfigurationRequest();
    BeanUtils.copyProperties(categoryConfigurationWebRequest, categoryConfigurationRequest);
    return categoryConfigurationRequest;
  }

  public static List<MerchantConfigurationRequest> toMerchantConfigurationListRequest(
      List<MerchantConfigurationWebRequest> merchantConfigurationWebRequestList) {

    return Optional.ofNullable(merchantConfigurationWebRequestList).orElseGet(Collections::emptyList).stream().map(
        merchantConfigurationWebRequest -> ConverterUtil
            .toMerchantConfigurationRequest(merchantConfigurationWebRequest)).collect(Collectors.toList());
  }

  private static MerchantConfigurationRequest toMerchantConfigurationRequest(
      MerchantConfigurationWebRequest merchantConfigurationWebRequest) {
    MerchantConfigurationRequest merchantConfigurationRequest = new MerchantConfigurationRequest();
    merchantConfigurationRequest.setBusinessPartnerCode(merchantConfigurationWebRequest.getMerchantCode());
    merchantConfigurationRequest.setReviewConfig(merchantConfigurationWebRequest.getReviewConfig());
    return merchantConfigurationRequest;
  }

  public static RejectReasonRequest toRejectReason(RejectReason rejectReasonSource) {
    RejectReasonRequest rejectReasonTarget = new RejectReasonRequest();
    BeanUtils.copyProperties(rejectReasonSource, rejectReasonTarget);
    return rejectReasonTarget;
  }

  public static BrandAuthCreateRequest toUpdateBrandRequest(
    BrandAuthCreateWebRequest webRequest) {
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    BeanUtils.copyProperties(webRequest, brandAuthCreateRequest);
    return brandAuthCreateRequest;
  }

  public static BrandAuthCreateWipRequest toCreateBrandWipRequest(BrandAuthCreateWipRequest request){
    BrandAuthCreateWipRequest brandAuthCreateRequest = new BrandAuthCreateWipRequest();
    BeanUtils.copyProperties(request, brandAuthCreateRequest);
    return brandAuthCreateRequest;
}

  public static BrandAuthUpdateRequest toBrandAuthUpdateRequest(
      BrandAuthCreateWebRequest webRequest) {
    BrandAuthUpdateRequest brandAuthUpdateRequest = new BrandAuthUpdateRequest();
    BeanUtils.copyProperties(webRequest, brandAuthUpdateRequest);
    return brandAuthUpdateRequest;
  }

  public static BrandAuthHistoryRequest toBrandAuthHistoryRequest(BrandAuthHistoryWebRequest webRequest){
    BrandAuthHistoryRequest brandAuthHistoryRequest = new BrandAuthHistoryRequest();
    BeanUtils.copyProperties(webRequest,brandAuthHistoryRequest);
    return brandAuthHistoryRequest;
  }

  public static CategoryHistoryWebResponse toCategoryHistoryWebResponse(
      CategoryHistoryResponse categoryHistoryResponse){
    CategoryHistoryWebResponse categoryHistoryWebResponse = new CategoryHistoryWebResponse();
    BeanUtils.copyProperties(categoryHistoryResponse,categoryHistoryWebResponse);
    categoryHistoryWebResponse.setCurrentValue(categoryHistoryResponse.getNewStatus());
    categoryHistoryWebResponse.setPreviousValue(categoryHistoryResponse.getOldStatus());
    return categoryHistoryWebResponse;
  }

  public static UploadImageRequest toUploadImageRequest(String imageFileName, String productCode,
      byte[] bytes, boolean active, boolean retryRequest, String originalFileName) {
    UploadImageRequest request =
        UploadImageRequest.builder().imageFileName(imageFileName).productCode(productCode)
            .bytes(bytes).active(active).retryRequest(retryRequest).originalFileType(
                originalFileName.substring(originalFileName.lastIndexOf(Constants.DOT) + 1)).build();
    return request;
  }
}
