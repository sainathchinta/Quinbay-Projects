package com.gdn.x.mta.distributiontask.inbound.util;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.mta.domain.event.modal.PriceInfoDTO;
import com.gdn.x.mta.distributiontask.model.enums.SellerBadgeConstants;
import com.gdn.x.mta.distributiontask.model.enums.SellerType;
import com.gdn.x.mta.distributiontask.model.type.ProductLabels;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ImageQcProcessedAndBrandResponse;
import com.gda.mta.product.dto.response.RestrictedKeywordsByFieldResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.modal.ScreeningProductApprovalEvent;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.model.ProductAttribute;
import com.gdn.x.mta.distributiontask.model.ProductImage;
import com.gdn.x.mta.distributiontask.model.ProductItem;
import com.gdn.x.mta.distributiontask.model.ProductItemAttribute;
import com.gdn.x.mta.distributiontask.model.ProductItemImage;
import com.gdn.x.mta.distributiontask.model.type.WorkflowState;
import com.gdn.x.productcategorybase.dto.AttributeType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;

@Slf4j
public class ProductDomainEventModelConverterUtils {

  private static final String CATEGORY_NOT_FOUND_FOR_PRODUCT_CODE = "Category not found for productCode ";
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

  public static Product convertProductDomainEventModelToProduct(ProductDetailResponse productDetailResponse,
      ScreeningProductApprovalEvent screeningProductApprovalEvent, boolean autoHealProduct,
      ImageQcProcessedAndBrandResponse imageQcProcessedAndBrandResponse)
      throws JsonProcessingException {
    Product product = new Product();
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, productDetailResponse.getCreatedBy());
    product.setStoreId(productDetailResponse.getStoreId());
    product.setBrand(productDetailResponse.getBrand());
    CategoryResponse category = productDetailResponse.getProductCategoryResponses().stream()
        .filter(productCategoryResponse -> !productCategoryResponse.isMarkForDelete())
        .map(ProductCategoryResponse::getCategory)
        .findFirst().orElseThrow(() -> new ApplicationRuntimeException(
            ErrorCategory.DATA_NOT_FOUND, CATEGORY_NOT_FOUND_FOR_PRODUCT_CODE + productDetailResponse.getProductCode()));
    product.setCategoryCode(category.getCategoryCode());
    product.setCategoryName(category.getName());
    product.setProductCreatedDate(Calendar.getInstance().getTime());
    product.setCreatedBy(productDetailResponse.getCreatedBy());
    product.setDescription(productDetailResponse.getDescription());
    product.setHeight(productDetailResponse.getHeight());
    product.setLength(productDetailResponse.getLength());
    product.setLongDescription(productDetailResponse.getDescription());
    product.setProductName(productDetailResponse.getName());
    product.setProductCode(productDetailResponse.getProductCode());
    product.setPostLive(screeningProductApprovalEvent.isPostLive());
    product.setRestrictedKeywordsPresent(screeningProductApprovalEvent.isRestrictedKeywordsPresent());
    product.setPromoSKU(productDetailResponse.isPromoSKU());
    product.setEdited(productDetailResponse.isEdited());
    setProductAttributes(productDetailResponse, product);
    setProductImages(productDetailResponse, product, autoHealProduct);
    setProductItems(productDetailResponse, product, autoHealProduct);
    if (CollectionUtils.isNotEmpty(screeningProductApprovalEvent.getPriceInfo())) {
      setProductPriceInfo(productDetailResponse, product, screeningProductApprovalEvent);
    }

    product.setProductStory(productDetailResponse.getProductStory());
    product.setShippingWeight(productDetailResponse.getShippingWeight());
    product.setStoreId(productDetailResponse.getStoreId());
    product.setUniqueSellingPoint(productDetailResponse.getUniqueSellingPoint());
    product.setVideoUrl(productDetailResponse.getUrl());
    product.setWeight(productDetailResponse.getWeight());
    product.setWidth(productDetailResponse.getWidth());
    product.setUom(productDetailResponse.getUom());
    product.setBusinessPartnerCode(screeningProductApprovalEvent.getMerchantCode());
    product.setBusinessPartnerName(screeningProductApprovalEvent.getMerchantName());
    if(Objects.nonNull(productDetailResponse.getAiGeneratedFieldsResponse())) {
        product.setAiGeneratedFields(OBJECT_MAPPER.writeValueAsString(
            productDetailResponse.getAiGeneratedFieldsResponse()));
    }

    // Reset status
    product.setCurrentVendor(null);
    product.setState(WorkflowState.UNASSIGNED);
    product.setUpdatedBy(productDetailResponse.getUpdatedBy());
    product.setUpdatedDate(new Date());
    if (Objects.nonNull(imageQcProcessedAndBrandResponse)) {
      product.setBrandCode(imageQcProcessedAndBrandResponse.getBrandCode());
      product.setBrandApprovalStatus(imageQcProcessedAndBrandResponse.getBrandApprovalStatus());
      product.setProductType(imageQcProcessedAndBrandResponse.getProductType());
      if (Objects.nonNull(imageQcProcessedAndBrandResponse.getImageQcProcessedResponse())) {
        product.setProductPredictionScore(
            imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getProductPredictionScore());
        product.setImageViolations(imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getImageViolations());
        product.setTextViolations(imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getTextViolations());
        product.setPredictedBrand(imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().getPredictedBrand());
        product.setForceReview(imageQcProcessedAndBrandResponse.getImageQcProcessedResponse().isForceReview());
      }
    } else {
      product.setImageViolations(ProductLabels.PENDING.getDescription());
      product.setTextViolations(ProductLabels.PENDING.getDescription());
    }
    if (product.getRejectedCount() == null) {
      product.setRejectedCount(0);
    }
    product.setSellerType(screeningProductApprovalEvent.isTrustedSeller() ?
      SellerType.TRUSTED_SELLER : SellerType.NON_TRUSTED_SELLER);
    product.setSellerBadge(SellerBadgeConstants.fromSellerBadgeConstants(
      screeningProductApprovalEvent.getSellerBadge()));
    setRestrictedKeywordDetectedInJson(product, screeningProductApprovalEvent.getRestrictedKeywordsDetected());
    product.setB2cActivated(screeningProductApprovalEvent.isB2cActivated());
    product.setB2bActivated(screeningProductApprovalEvent.isB2bActivated());
    product.setDistributionMappingStatus(
        screeningProductApprovalEvent.getDistributionMappingStatus());
    product.setProductCreationType(screeningProductApprovalEvent.getProductCreationType());
    return product;
  }

  private static void setRestrictedKeywordDetectedInJson(Product product,
      List<RestrictedKeywordsByFieldResponse> restrictedKeywordsByFieldResponseList) {
    try {
      product
          .setRestrictedKeywordsDetected(new ObjectMapper().writeValueAsString(restrictedKeywordsByFieldResponseList));
    } catch (JsonProcessingException e) {
      log.error("Error while parsing restricted keyword list to json. restrictedKeywordsByFieldResponseList : {} ",
          restrictedKeywordsByFieldResponseList, e);
    }
  }

  private static void setProductAttributes(ProductDetailResponse productDetailResponse, Product product) {
    List<ProductAttribute> productAttributes = new ArrayList<>();
    Map<String, ProductAttributeResponse> uniqueAttributeCodeMap =
      productDetailResponse.getProductAttributeResponses().stream().collect(Collectors.toMap(
        productAttributeResponse -> productAttributeResponse.getAttribute().getAttributeCode(),
        Function.identity(), (attributeCode1, attributeCode2) -> attributeCode1,
        LinkedHashMap::new));

    List<ProductAttributeResponse> attributeResponses =
      uniqueAttributeCodeMap.values().stream().distinct().collect(Collectors.toList());

    for(ProductAttributeResponse productAttributeResponse : attributeResponses){
        if (!productAttributeResponse.isMarkForDelete()) {
          for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeResponse.getProductAttributeValues()) {
            if (!productAttributeValueResponse.isMarkForDelete()) {
              if (AttributeType.DEFINING_ATTRIBUTE.name()
                .equalsIgnoreCase(productAttributeResponse.getAttribute().getAttributeType())) {
                String attributeValue =
                  Optional.ofNullable(productAttributeValueResponse.getAllowedAttributeValue()).map(AllowedAttributeValueResponse::getValue).orElse(StringUtils.EMPTY);
                ProductAttribute productAttribute = new ProductAttribute(product,
                  productAttributeResponse.getAttribute().getAttributeCode(),
                  productAttributeResponse.getAttribute().getName(), attributeValue,
                  AttributeType.DEFINING_ATTRIBUTE.name());
                productAttribute.setVariantCreation(
                  productAttributeResponse.getAttribute().isVariantCreation());
                productAttributes.add(productAttribute);
              } else if (AttributeType.DESCRIPTIVE_ATTRIBUTE.name().equalsIgnoreCase(productAttributeResponse.getAttribute().getAttributeType())) {
                String attributeValue =
                  Optional.ofNullable(productAttributeValueResponse.getDescriptiveAttributeValue()).orElse(StringUtils.EMPTY);
                ProductAttribute productAttribute = new ProductAttribute(product,
                  productAttributeResponse.getAttribute().getAttributeCode(),
                  productAttributeResponse.getAttribute().getName(), attributeValue,
                  AttributeType.DESCRIPTIVE_ATTRIBUTE.name());
                productAttribute.setVariantCreation(
                  productAttributeResponse.getAttribute().isVariantCreation());
                productAttribute.setExtractedValue(
                  productAttributeResponse.getAttribute().isExtractedValue());
                productAttribute.setDsExtraction(
                    productAttributeResponse.getAttribute().isDsExtraction());
                productAttributes.add(productAttribute);
              } else if (AttributeType.PREDEFINED_ATTRIBUTE.name().equalsIgnoreCase(productAttributeResponse.getAttribute().getAttributeType())) {
                String attributeValue = Optional.ofNullable(productAttributeValueResponse.getPredefinedAllowedAttributeValue())
                  .map(PredefinedAllowedAttributeValueResponse::getValue).orElse(StringUtils.EMPTY);
                ProductAttribute productAttribute = new ProductAttribute(product,
                  productAttributeResponse.getAttribute().getAttributeCode(),
                  productAttributeResponse.getAttribute().getName(), attributeValue,
                  AttributeType.PREDEFINED_ATTRIBUTE.name());
                productAttribute.setVariantCreation(
                  productAttributeResponse.getAttribute().isVariantCreation());
                productAttribute.setDsExtraction(
                    productAttributeResponse.getAttribute().isDsExtraction());
                productAttributes.add(productAttribute);
              }
            }
          }
        }
      }

    product.setProductAttributes(productAttributes);
  }

  private static void setProductImages(ProductDetailResponse productDetailResponse, Product product,
    boolean autoHealProduct) {
    product.setProductImages(new ArrayList<>());
    for (Image image : productDetailResponse.getImages()) {
      if(!image.isMarkForDelete() || autoHealProduct) {
        ProductImage productImage = new ProductImage();
        productImage.setLocationPath(image.getLocationPath());
        productImage.setSequence(image.getSequence());
        productImage.setMainImage(image.isMainImages());
        productImage.setActive(image.isActive());
        productImage.setEdited(image.isEdited());
        productImage.setRevised(image.isRevised());
        productImage.setProduct(product);
        productImage.setHashCode(image.getHashCode());
        productImage.setCommonImage(image.isCommonImage());
        productImage.setMarkForDelete(image.isMarkForDelete());
        if (Objects.nonNull(image.getOriginalImage())) {
          productImage.setOriginalImage(image.getOriginalImage());
        }
        product.getProductImages().add(productImage);
      }
    }
  }

  private static void setProductItems(ProductDetailResponse productDetailResponse, Product product,
    boolean autoHealProduct) {
    product.setProductItems(new ArrayList<>());
    for (ProductItemResponse productItemResponse : productDetailResponse.getProductItemResponses()) {
      if(!productItemResponse.isMarkForDelete()) {
        ProductItem productItem = new ProductItem();
        productItem.setProduct(product);
        productItem.setGeneratedItemName(productItemResponse.getGeneratedItemName());
        productItem.setNewlyAdded(productItemResponse.isNewlyAddedItem());
        productItem.setUpcCode(productItemResponse.getUpcCode());
        productItem.setSkuCode(productItemResponse.getSkuCode());
        if (Objects.nonNull(productItemResponse.getDangerousGoodsLevel())) {
          productItem.setDangerousGoodsLevel(productItemResponse.getDangerousGoodsLevel());
        } else {
          productItem.setDangerousGoodsLevel(0);
        }
        productItem.setProductItemImages(new ArrayList<>());
        for (Image image : productItemResponse.getImages()) {
          if(!image.isMarkForDelete() || autoHealProduct) {
            ProductItemImage productItemImage = new ProductItemImage();
            productItemImage.setLocationPath(image.getLocationPath());
            productItemImage.setSequence(image.getSequence());
            productItemImage.setMainImage(image.isMainImages());
            productItemImage.setActive(image.isActive());
            productItemImage.setEdited(image.isEdited());
            productItemImage.setRevised(image.isRevised());
            productItemImage.setProductItem(productItem);
            productItemImage.setHashCode(productItemImage.getHashCode());
            productItemImage.setCommonImage(image.isCommonImage());
            productItemImage.setMarkForDelete(image.isMarkForDelete());
            if (Objects.nonNull(image.getOriginalImage())) {
              productItemImage.setOriginalImage(image.getOriginalImage());
            }
            productItem.getProductItemImages().add(productItemImage);
          }
        }
        setProductItemAttributeValues(productItemResponse, productItem);
        product.getProductItems().add(productItem);
      }
    }
  }

  private static void setProductPriceInfo(ProductDetailResponse productDetailResponse,
      Product product, ScreeningProductApprovalEvent screeningProductApprovalEvent) {

    Map<String, PriceInfoDTO> itemIdToPriceInfoMap = screeningProductApprovalEvent.getPriceInfo().stream()
        .collect(Collectors.toMap(PriceInfoDTO::getItemId, Function.identity()));

    Map<String, PriceInfoDTO> skuToPriceInfoMap = productDetailResponse.getProductItemResponses().stream()
        .filter(productItemResponse -> itemIdToPriceInfoMap.containsKey(productItemResponse.getId()))
        .collect(Collectors.toMap(ProductItemResponse::getSkuCode,
            productItemResponse -> itemIdToPriceInfoMap.get(productItemResponse.getId())));

    product.getProductItems().forEach(productItem -> {
      PriceInfoDTO priceInfo = skuToPriceInfoMap.get(productItem.getSkuCode());
      if (priceInfo != null) {
        productItem.setItemSku(priceInfo.getItemSku());
        productItem.setMinPrice(priceInfo.getMinPrice());
        productItem.setMaxPrice(priceInfo.getMaxPrice());
      }
    });
  }

  public static void setPriceInfoForImageEdit(ProductItem productItem,
      Map<String, ProductItem> updatedProductItemMap) {
    ProductItem updatedProductItem = updatedProductItemMap.get(productItem.getSkuCode());
    if (updatedProductItem != null) {
      productItem.setItemSku(updatedProductItem.getItemSku());
      productItem.setMinPrice(updatedProductItem.getMinPrice());
      productItem.setMaxPrice(updatedProductItem.getMaxPrice());
    }
  }

  private static void setProductItemAttributeValues(ProductItemResponse productItemResponse, ProductItem productItem) {
    try {
      List<ProductItemAttribute> productItemAttributes = new ArrayList<>();
      Map<String, ProductItemAttributeValueResponse> uniqueAttributeCodeMap =
        productItemResponse.getProductItemAttributeValueResponses().stream().collect(Collectors.toMap(
          productItemAttributeValueResponse -> productItemAttributeValueResponse.getAttributeResponse().getAttributeCode(),
          Function.identity(), (attributeCode1, attributeCode2) -> attributeCode1,
          LinkedHashMap::new));

      List<ProductItemAttributeValueResponse> productItemAttributeValueResponses =
        uniqueAttributeCodeMap.values().stream().distinct().collect(Collectors.toList());

      for(ProductItemAttributeValueResponse productItemAttributeValueResponse : productItemAttributeValueResponses){
      if(!productItemAttributeValueResponse.isMarkForDelete()) {
          ProductItemAttribute productItemAttribute = new ProductItemAttribute();
          productItemAttribute.setProduct(productItem);
          productItemAttribute
              .setAttributeCode(productItemAttributeValueResponse.getAttributeResponse().getAttributeCode());
          productItemAttribute
              .setAttributeType(productItemAttributeValueResponse.getAttributeResponse().getAttributeType());
          productItemAttribute.setName(productItemAttributeValueResponse.getAttributeResponse().getName());
          productItemAttribute.setValue(productItemAttributeValueResponse.getValue());
          productItemAttribute
              .setVariantCreation(productItemAttributeValueResponse.getAttributeResponse().isVariantCreation());
          productItemAttributes.add(productItemAttribute);
        }
      }

      productItem.setProductItemAttributes(productItemAttributes);
    } catch (Exception ex) {
      log.error("Error while setting ProductItemAttributeDomainEventModal for itemCode: {} productCode: {}",
          productItem.getSkuCode(), productItem.getProduct().getProductCode(), ex);
    }
  }
}
