package com.gdn.mta.bulk.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.ProductItemWholesalePriceRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.bulk.dto.product.AllowedValueDtoResponse;
import com.gdn.x.businesspartner.dto.PickupPointDTO;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import com.gdn.mta.bulk.service.util.BeanUtils;

import com.gda.mta.product.dto.ProductBusinessPartnerAttributeRequest;
import com.gda.mta.product.dto.ProductItemCreationRequest;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.product.AttributeMapDto;
import com.gdn.mta.bulk.dto.product.ProductItemRequestDto;
import com.gdn.mta.bulk.dto.product.ProductItemV2Request;
import com.gdn.mta.bulk.dto.product.ProductV2Request;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CatalogRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;

public class CreateProductV2Util {
  
  public static final String ATTR_NAME_BRAND = "Brand";

  private CreateProductV2Util() {
  }

  /**
   * Validate that pickup point from client is a valid and granted value for selected business partner
   *
   * @param bp
   * @param ppResponseList
   * @param pickupPointCode
   * @throws Exception
   */
  public static void validatePickupPoint(ProfileResponse bp,
    List<PickupPointResponse> ppResponseList, String pickupPointCode) throws ApplicationException {
    boolean isNotValid = true;
    for(PickupPointResponse pickupPoint : ppResponseList){
      if(pickupPoint.getCode().equals(pickupPointCode)){
        isNotValid = false;
        break;
      }
    }
    if(isNotValid){
      throw new ApplicationException(ErrorCategory.INVALID_FORMAT,
          " Pickup point " + pickupPointCode + " is not valid for merchant " + bp.getCompany().getName());
    }
  }
  
  /**
   * Build product's item request 
   * 
   * @param request
   * @return
   * @throws Exception
   */
  public static List<ProductItemCreationRequest> buildProductItemRequest(ProductV2Request request,
      ProductItemRequestDto productItemRequestDto) {
    List<ProductItemCreationRequest> result = new ArrayList<>();
    for (ProductItemV2Request item : request.getProductItems()) {
      appendItemAttributeMapWithVariantCreationAttribute(request, productItemRequestDto, item);
      ProductItemCreationRequest itemReq = new ProductItemCreationRequest();
      itemReq.setProductType(request.getProductType());
      itemReq.setMerchantSku(item.getMerchantSku());
      itemReq.setPrice(item.getPrice());
      itemReq.setSalePrice(item.getSalePrice());
      itemReq.setStock(item.getStock());
      itemReq.setMinimumStock(item.getMinimumStock());
      itemReq.setPickupPointId(request.getPickupPointCode());
      itemReq.setDisplay(item.isDisplayable());
      itemReq.setBuyable(item.isBuyable());
      itemReq.setAttributesMap(item.getAttributesMap());
      itemReq.setImages(CreateProductV2Util.buildImageRequest(item.getImages(),
          productItemRequestDto.getProductCode()));
      itemReq.setUpcCode(item.getUpcCode());
      itemReq.setProductItemAttributeValueRequests(getProductItemAttributeValueRequests(item,
          productItemRequestDto.getNonDefiningItemAttributeDetails()));
      itemReq.setWholesalePriceActivated(item.getWholesalePriceActivated());
      itemReq.setProductItemWholesalePriceRequests(getProductItemWholesalePriceRequests(item));
      result.add(itemReq);
    }
    return result;
  }

  private static List<ProductItemWholesalePriceRequest> getProductItemWholesalePriceRequests(
      ProductItemV2Request item) {
    return Optional.ofNullable(item.getWholesale())
        .map(Collection::stream)
        .map(wholeSaleDataStream ->
            wholeSaleDataStream.map(wholeSaleData ->
                ProductItemWholesalePriceRequest.builder()
                  .quantity(wholeSaleData.getQuantity())
                  .wholesaleDiscount(wholeSaleData.getDiscount())
                  .build())
                .collect(Collectors.toList()))
    .orElse(null);
  }

  private static void appendItemAttributeMapWithVariantCreationAttribute(ProductV2Request request,
      ProductItemRequestDto productItemRequestDto, ProductItemV2Request item) {
    productItemRequestDto.getNonDefAttrMap()
        .values()
        .stream()
        .filter(CreateProductV2Util::isDescriptiveVariantCreator)
        .filter(attributeResponse ->
            Objects.nonNull(request.getProductNonDefiningAttributes()
                .get(attributeResponse.getAttributeCode())))
        .forEach(attributeResponse -> item.getAttributesMap()
            .putIfAbsent(attributeResponse.getAttributeCode(), 
                request.getProductNonDefiningAttributes()
                    .get(attributeResponse.getAttributeCode())));
  }

  private static List<ProductItemAttributeValueRequest> getProductItemAttributeValueRequests(
      ProductItemV2Request productItemV2Request, List<AttributeResponse> nonDefiningItemAttributeDetails) {
    return nonDefiningItemAttributeDetails.stream()
        .map(attributeResponse -> getProductItemAttributeValueRequest(
            Optional.ofNullable(productItemV2Request.getNonDefiningItemAttributes())
                .orElseGet(HashMap::new), 
            attributeResponse))
        .collect(Collectors.toList());
  }

  private static ProductItemAttributeValueRequest getProductItemAttributeValueRequest(
      Map<String, String> nonDefiningItemAttributes, AttributeResponse attributeResponse) {
    AttributeRequest attributeRequest = new AttributeRequest();
    BeanUtils.copyProperties(attributeResponse, attributeRequest, "allowedAttributeValues");
    return new ProductItemAttributeValueRequest(attributeRequest,
        nonDefiningItemAttributes.get(attributeResponse.getAttributeCode()), attributeResponse.getStoreId());
  }
  
  /**
   * Build image request both for product and item's image level. Image request will generated from List<String> of images name.
   * First element of List<String> will become product or item's main image.
   * Image path from client input will be appended with new generated product code from PBP.
   * 
   * @param imageList
   * @param productCode
   * @return
   */
  public static List<Image> buildImageRequest(List<String> imageList, String productCode) {
    List<Image> result = new ArrayList<>();
    int i = 0;
    for(String imagePath : imageList){
      Image imageReq = new Image(i==0, productCode + Constant.SLASH + imagePath, i);
      result.add(imageReq);
      i++;
    }
    
    return result;
  }
  
  /**
   * Build list of product business partner attribute for PBP API request. It will assigned by non defining attribute with SKU value true.
   * 
   * @param nonDefAttrMapSkuValue
   * @param request
   * @return
   */
  public static List<ProductBusinessPartnerAttributeRequest> buildPrdBPAttrReq(
      Map<String, AttributeResponse> nonDefAttrMapSkuValue, ProductV2Request request){
    return nonDefAttrMapSkuValue.keySet()
        .stream()
        .filter(attributeCode -> Objects.nonNull(
            request.getProductNonDefiningAttributes().get(attributeCode)))
        .map(attributeCode -> createPbpAttrRequest(nonDefAttrMapSkuValue, request, attributeCode))
        .collect(Collectors.toList());
  }

  private static ProductBusinessPartnerAttributeRequest createPbpAttrRequest(Map<String, AttributeResponse> nonDefAttrMapSkuValue,
      ProductV2Request request, String attributeCode) {
    AttributeResponse attrResp = nonDefAttrMapSkuValue.get(attributeCode);
    ProductBusinessPartnerAttributeRequest prdBPReq =
        new ProductBusinessPartnerAttributeRequest();
    prdBPReq.setAttributeId(attrResp.getId());
    prdBPReq.setValue(request.getProductNonDefiningAttributes().get(attributeCode));
    return prdBPReq;
  }

  /**
   * Build category response for PBP API request
   * 
   * @param categoryResp
   * @return
   */
  public static List<ProductCategoryRequest> buildProductCategoryReq(
      CategoryDetailResponse categoryResp){
    List<ProductCategoryRequest> result = new ArrayList<>();
    ProductCategoryRequest prdCatReq = new ProductCategoryRequest();
    CategoryRequest catReq = new CategoryRequest();
    catReq.setId(categoryResp.getId());
    catReq.setStoreId(categoryResp.getStoreId());
    catReq.setName(categoryResp.getName());
    catReq.setSequence(categoryResp.getSequence());
    catReq.setCategoryCode(categoryResp.getCategoryCode());
    catReq.setActivated(categoryResp.isActivated());
    catReq.setViewable(categoryResp.isViewable());
    
    CatalogRequest catalogReq = new CatalogRequest();
    catalogReq.setId(categoryResp.getCatalog().getId());
    catalogReq.setStoreId(categoryResp.getCatalog().getStoreId());
    catalogReq.setName(categoryResp.getCatalog().getName());
    catalogReq.setCatalogCode(categoryResp.getCatalog().getCatalogCode());
    catalogReq.setCatalogType(categoryResp.getCatalog().getCatalogType());
    catReq.setCatalog(catalogReq);
    
    prdCatReq.setCategory(catReq);
    result.add(prdCatReq);
    return result;
  }
  
  /**
   * Build product attribute request for PBP API request. No need to validate client category attribute because it's done by validateCatAttrReq.
   * 
   * @param request
   * @param attrMap
   * @param allowedValues
   * @return
   */
  public static List<ProductAttributeRequest> buildProductAttributeRequest(
      ProductV2Request request, AttributeMapDto attrMap,
      Map<String, List<AllowedValueDtoResponse>> allowedValues) throws ApplicationException {
    List<ProductAttributeRequest> result = new ArrayList<>();
    CreateProductV2Util.appendProductAttrReq(result, request, attrMap.getNonDefAttrMap(), 
        allowedValues);
    CreateProductV2Util.appendProductAttrReq(result, request, attrMap.getNonDefAttrMapSkuValue(), 
        allowedValues);
    CreateProductV2Util.appendProductAttrReq(result, request, attrMap.getDefAttrMap(), 
        allowedValues);
    
    return result;
  }

  /**
   * Append specified product attribute request for non defining attribute. It used to build product
   * attribute request to PBP.
   *
   * @param result
   * @param request
   * @param attrMap
   */
  public static void appendProductAttrReq(List<ProductAttributeRequest> result,
      ProductV2Request request,
      Map<String, AttributeResponse> attrMap,
      Map<String, List<AllowedValueDtoResponse>> allowedValues) throws ApplicationException {
    for(Map.Entry<String, AttributeResponse> attrEntry : attrMap.entrySet()) {
      if(isProductRequestAttributeValid(attrEntry.getKey(), request)) {
        appendProductAttrReqToResult(result, request, attrEntry, allowedValues);
      }
    }
  }

  private static boolean isProductRequestAttributeValid(String attributeCode, ProductV2Request request) {
    return !attributeCode.equals(request.getFamilyColorCode());
  }

  private static void appendProductAttrReqToResult(List<ProductAttributeRequest> result,
      ProductV2Request request, 
      Map.Entry<String, AttributeResponse> attrEntry,
      Map<String, List<AllowedValueDtoResponse>> allowedValues) throws ApplicationException {
    ProductAttributeRequest prdAttrReq = new ProductAttributeRequest();
    AttributeRequest attrReq = new AttributeRequest();
    attrReq.setAttributeCode(attrEntry.getValue().getAttributeCode());
    attrReq.setId(attrEntry.getValue().getId());
    attrReq.setStoreId(attrEntry.getValue().getStoreId());
    attrReq.setName(attrEntry.getValue().getName());
    attrReq.setAttributeType(AttributeType.valueOf(attrEntry.getValue().getAttributeType()));
    attrReq.setDescription(attrEntry.getValue().getDescription());
    attrReq.setSkuValue(attrEntry.getValue().isSkuValue());
    attrReq.setVariantCreation(attrEntry.getValue().isVariantCreation());
    attrReq.setMandatory(attrEntry.getValue().isMandatory());
    prdAttrReq.setAttribute(attrReq);
    prdAttrReq.setProductAttributeName(attrEntry.getValue().getName());
    prdAttrReq.setSequence(0);

    if (attrEntry.getValue().getAttributeType()
        .equals(AttributeType.DEFINING_ATTRIBUTE.toString()) || 
        (isDescriptiveVariantCreator(attrEntry.getValue()) && 
            request.getProductDefiningAttributes().containsKey(attrEntry.getKey()))) {
      prdAttrReq.setProductAttributeValues(
          CreateProductV2Util.buildProductAttributeValue(attrEntry.getValue(), null,
              request.getProductDefiningAttributes().get(attrEntry.getKey()), allowedValues));
    } else {
      String attrValue = request.getProductNonDefiningAttributes().get(attrEntry.getKey());
      if (ATTR_NAME_BRAND.equals(attrEntry.getValue().getName())) {
        attrValue = Optional.ofNullable(request.getBrand()).orElse(StringUtils.EMPTY);
      }
      prdAttrReq.setProductAttributeValues(
          CreateProductV2Util.buildProductAttributeValue(attrEntry.getValue(), attrValue,
              null, allowedValues));
    }

    result.add(prdAttrReq);
  }

  /**
   * Build product attribute value request that will be used by appendProductAttrReq
   * 
   * @param attrResp
   * @param value
   * @param valueList
   * @return
   */
  public static List<ProductAttributeValueRequest> buildProductAttributeValue(
      AttributeResponse attrResp, String value, List<String> valueList,
      Map<String, List<AllowedValueDtoResponse>> allowedValues) throws ApplicationException {
    List<ProductAttributeValueRequest> result = new ArrayList<>();
    if(CollectionUtils.isNotEmpty(valueList)){
      for(String definingValue : valueList){
        ProductAttributeValueRequest prdAttrValueReq = new ProductAttributeValueRequest();
        prdAttrValueReq.setStoreId(attrResp.getStoreId());
        prdAttrValueReq.setPredefinedAllowedAttributeValue(null);
        if(!isDescriptiveVariantCreator(attrResp)){
          prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
          prdAttrValueReq.setDescriptiveAttributeValue(value);
          prdAttrValueReq.setAllowedAttributeValue(
              CreateProductV2Util.findDefiningAllowedValue(attrResp, definingValue, allowedValues));
        } else {
          prdAttrValueReq.setDescriptiveAttributeValue(definingValue);
          prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
        }
        result.add(prdAttrValueReq);
      }
    } else if(StringUtils.isNotEmpty(value)){
      ProductAttributeValueRequest prdAttrValueReq = new ProductAttributeValueRequest();
      if(attrResp.getAttributeType().equals(AttributeType.DESCRIPTIVE_ATTRIBUTE.toString())){
        prdAttrValueReq.setStoreId(attrResp.getStoreId());
        prdAttrValueReq.setDescriptiveAttributeValue(value);
        prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
        prdAttrValueReq.setPredefinedAllowedAttributeValue(null);
      } else if(attrResp.getAttributeType().equals(AttributeType.PREDEFINED_ATTRIBUTE.toString())) {
        prdAttrValueReq.setStoreId(attrResp.getStoreId());
        prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
        prdAttrValueReq.setPredefinedAllowedAttributeValue(
            CreateProductV2Util.findPredefinedAllowedValue(attrResp, value, allowedValues));
      }
      result.add(prdAttrValueReq);
    } else {
      result.add(createDefaultProductAttributeValue(attrResp));
    }
    return result;
  }

  private static ProductAttributeValueRequest createDefaultProductAttributeValue(
      AttributeResponse attrResp) {
    ProductAttributeValueRequest prdAttrValueReq = new ProductAttributeValueRequest();
    prdAttrValueReq.setStoreId(attrResp.getStoreId());
    if(AttributeType.PREDEFINED_ATTRIBUTE.toString().equals(attrResp.getAttributeType())){
      prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.PREDEFINED);
      prdAttrValueReq.setPredefinedAllowedAttributeValue(createDefaultPredefinedAllowedValue());
    } else if(AttributeType.DEFINING_ATTRIBUTE.toString().equals(attrResp.getAttributeType())) {
      prdAttrValueReq.setPredefinedAllowedAttributeValue(null);
      prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.NONE);
      prdAttrValueReq.setDescriptiveAttributeValue(StringUtils.EMPTY);
      prdAttrValueReq.setAllowedAttributeValue(createDefaultDefiningAllowedValue());
    } else {
      prdAttrValueReq.setPredefinedAllowedAttributeValue(null);
      prdAttrValueReq.setDescriptiveAttributeValueType(DescriptiveAttributeValueType.SINGLE);
      prdAttrValueReq.setDescriptiveAttributeValue(StringUtils.EMPTY);
    }
    return prdAttrValueReq;
  }

  /**
   * Find suitable defining attribute value based on client input
   *
   * @param attrResp
   * @param definingValue
   * @return
   */
  public static AllowedAttributeValueRequest findDefiningAllowedValue(AttributeResponse attrResp,
      String definingValue, Map<String, List<AllowedValueDtoResponse>> allowedValueMap)
      throws ApplicationException {
    if(!allowedValueMap.containsKey(attrResp.getAttributeCode())) {
      throw new ApplicationException(ErrorCategory.VALIDATION,
          " The attribute's value is not valid for attribute: " + attrResp.getAttributeCode() + " "
              + "with value: " + definingValue);
    }
    for(AllowedValueDtoResponse allowedValue : allowedValueMap.get(attrResp.getAttributeCode())){
      if(definingValue.equals(allowedValue.getValue())){
        AllowedAttributeValueRequest result = new AllowedAttributeValueRequest();
        result.setId(allowedValue.getAllowedValueId());
        result.setValue(allowedValue.getValue());
        result.setSequence(0);
        result.setAllowedAttributeCode(allowedValue.getAllowedValueCode());
        return result;
      }
    }
    return null;
  }

  public static AllowedAttributeValueRequest createDefaultDefiningAllowedValue(){
    AllowedAttributeValueRequest allowedAttributeValueRequest = new AllowedAttributeValueRequest();
    allowedAttributeValueRequest.setValue(StringUtils.EMPTY);
    return allowedAttributeValueRequest;
  }

  /**
   * Find suitable predefine attribute value based on client input
   *
   * @param attrResp
   * @param value
   * @return
   */
  public static PredefinedAllowedAttributeValueRequest findPredefinedAllowedValue(AttributeResponse attrResp,
      String value, Map<String, List<AllowedValueDtoResponse>> allowedValueMap)
      throws ApplicationException {
    if(!allowedValueMap.containsKey(attrResp.getAttributeCode())) {
      throw new ApplicationException(ErrorCategory.VALIDATION,
          " The attribute's value is not valid for attribute: " + attrResp.getAttributeCode() + " "
              + "with value: " + value);
    }
    for(AllowedValueDtoResponse allowedValue : allowedValueMap.get(attrResp.getAttributeCode())){
      if(value.equals(allowedValue.getValue())){
        PredefinedAllowedAttributeValueRequest result = new PredefinedAllowedAttributeValueRequest();
        result.setId(allowedValue.getAllowedValueId());
        result.setValue(allowedValue.getValue());
        result.setSequence(0);
        result.setPredefinedAllowedAttributeCode(allowedValue.getAllowedValueCode());
        return result;
      }
    }
    return null;
  }

  public static PredefinedAllowedAttributeValueRequest createDefaultPredefinedAllowedValue(){
    PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest =
        new PredefinedAllowedAttributeValueRequest();
    predefinedAllowedAttributeValueRequest.setValue(StringUtils.EMPTY);
    return predefinedAllowedAttributeValueRequest;
  }
  
  /**
   * Populate client input for predefine and defining attribute into a single map
   *
   * @param request
   * @return
   */
  public static Map<String, List<String>> populateClientAttributeIntoMap(ProductV2Request request){
    Map<String, List<String>> result = request.getProductNonDefiningAttributes()
        .entrySet()
        .stream()
        .collect(Collectors.toMap(
            Map.Entry::getKey, 
            entry -> Collections.singletonList(entry.getValue())));
    
    Optional.ofNullable(request.getProductDefiningAttributes())
        .orElseGet(HashMap::new)
        .forEach(result::put);

    return result;
  }
  
  /**
   * Populate category's attribute into three categories
   * 
   * @param category
   * @return
   */
  public static AttributeMapDto populateCatAttr(CategoryDetailResponse category) {
    Map<String, AttributeResponse> defAttrMap = new HashMap();
    Map<String, AttributeResponse> nonDefAttrMap = new HashMap();
    Map<String, AttributeResponse> nonDefAttrMapSkuValue = new HashMap();
    
    for (CategoryAttributeResponse catAttr : category.getCategoryAttributes()) {
      if (!catAttr.isMarkForDelete()) {
        if (AttributeType.DEFINING_ATTRIBUTE.toString()
            .equals(catAttr.getAttribute().getAttributeType())) {
          defAttrMap.put(catAttr.getAttribute().getAttributeCode(), catAttr.getAttribute());
        } else {
          if (catAttr.getAttribute().isSkuValue()) {
            nonDefAttrMapSkuValue.put(catAttr.getAttribute().getAttributeCode(),
                catAttr.getAttribute());
          } else {
            nonDefAttrMap.put(catAttr.getAttribute().getAttributeCode(), catAttr.getAttribute());
          }
        }
      }
    }
    
    return new AttributeMapDto(defAttrMap, nonDefAttrMap, nonDefAttrMapSkuValue);
  }

  public static boolean isDescriptiveVariantCreator(AttributeResponse attribute) {
    return AttributeType.DESCRIPTIVE_ATTRIBUTE.toString()
        .equals(attribute.getAttributeType())
        && attribute.isVariantCreation();
  }
  
  /**
   * Convert gram to kilogram for shipping weight value
   * 
   * @param gram
   * @return
   */
  public static Double getKilogramFromGram(Double gram) {
    return gram / 1000;
  }
  
}
