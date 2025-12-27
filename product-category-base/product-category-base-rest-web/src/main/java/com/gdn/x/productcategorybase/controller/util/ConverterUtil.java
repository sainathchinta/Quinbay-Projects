package com.gdn.x.productcategorybase.controller.util;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.x.productcategorybase.dto.VideoDTO;
import org.apache.commons.codec.binary.Base64;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringEscapeUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.ImmutablePair;
import com.gdn.common.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.entity.Origin;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.productcategorybase.AttributeSortType;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.AttributeValueDTO;
import com.gdn.x.productcategorybase.dto.AttributeValueUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryDTO;
import com.gdn.x.productcategorybase.dto.CategoryDetailDTO;
import com.gdn.x.productcategorybase.dto.CategoryInfoUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryKeywordsUpdateListDTO;
import com.gdn.x.productcategorybase.dto.CategoryMappingsUpdateDTO;
import com.gdn.x.productcategorybase.dto.CategoryServiceDTO;
import com.gdn.x.productcategorybase.dto.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.MasterAttributeUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateDTO;
import com.gdn.x.productcategorybase.dto.ProductBrandUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.RestrictedKeywordsUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateDTO;
import com.gdn.x.productcategorybase.dto.SimpleMasterProductUpdateResponseDTO;
import com.gdn.x.productcategorybase.dto.WholesaleMappingDTO;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import com.gdn.x.productcategorybase.dto.request.AllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeAndValueByTypeRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeSortTypeRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeValueUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryDetailRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryInfoUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryMappingsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeAddRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.MasterAttributeUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.PredefinedAllowedAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.RestrictedKeywordsUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.VideoAddEditRequest;
import com.gdn.x.productcategorybase.dto.request.WholesaleMappingRequest;
import com.gdn.x.productcategorybase.dto.response.AllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailAndShippingResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryReferenceResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.LookupResponse;
import com.gdn.x.productcategorybase.dto.response.MasterAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import com.gdn.x.productcategorybase.dto.response.OriginalSalesCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.PredefinedAllowedAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndAttributeDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.ShippingResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Catalog;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryAttribute;
import com.gdn.x.productcategorybase.entity.CategoryReference;
import com.gdn.x.productcategorybase.entity.Lookup;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.service.FileStorageService;
import com.gdn.x.productcategorybase.util.ValidationUtil;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConverterUtil {

  private static final String UPC_CODE_PARAM = "upcCode";
  private static final String ITEM_IMAGES_PARAM = "itemImages";
  private static final String ITEM_ATTRIBUTES = "itemAttributes";
  private static final String SOURCE_ITEM_CODE = "sourceItemCode";
  private static final String CONTENT_CHANGED = "contentChanged";
  private static final String ITEM_DISTRIBUTION_INFO = "itemDistributionInfo";
  private static final String OMNI_CHANNEL_SKU = "omniChannelSku";
  private static final String WARNA = "Warna";
  private static final String COLOR = "Color";

  private static FileStorageService fileStorageService;

  public static void setFileStorageService(FileStorageService fileStorageService) {
    ConverterUtil.fileStorageService = fileStorageService;
  }

  // convertProductToProductDetailResponse
  public static CategoryDetailResponse convertCategoryToCategoryDetailResponse(Category category,
      Set<String> attributeConfigurationSet) {
    CategoryDetailResponse response = getCategoryDetailResponse(category);

    for (CategoryAttribute categoryAttribute : category.getCategoryAttributes()) {
      CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
      BeanUtils.copyProperties(categoryAttribute, categoryAttributeResponse);
      AttributeResponse attributeResponse = new AttributeResponse();
      BeanUtils.copyProperties(categoryAttribute.getAttribute(), attributeResponse, "allowedAttributeValues",
          "predefinedAllowedAttributeValues");
      attributeResponse.setAttributeType(categoryAttribute.getAttribute().getAttributeType().toString());
      categoryAttributeResponse
          .setMainAttributeFlag(attributeConfigurationSet.contains(attributeResponse.getAttributeCode()));
      categoryAttributeResponse.setAttribute(attributeResponse);
      response.getCategoryAttributes().add(categoryAttributeResponse);
    }

    for (CategoryReference masterCategoryReferences : category.getMasterCategoryReferences()) {
      if (!masterCategoryReferences.isMarkForDelete()) {
        CategoryReferenceResponse masterCategoryReferenceResponse = new CategoryReferenceResponse();
        BeanUtils.copyProperties(masterCategoryReferences, masterCategoryReferenceResponse);
        CategoryResponse categoryResponse = new CategoryResponse();
        BeanUtils.copyProperties(masterCategoryReferences.getMasterCategory(), categoryResponse);
        masterCategoryReferenceResponse.setMasterCategoryReference(categoryResponse);
        response.getMasterCategoryReferences().add(masterCategoryReferenceResponse);
      }
    }

    for (CategoryReference salesCategoryReferences : category.getSalesCategoryReferences()) {
      if (!salesCategoryReferences.isMarkForDelete() && !salesCategoryReferences.getSalesCategory().isHalalCategory()) {
        CategoryReferenceResponse salesCategoryReferenceResponse = new CategoryReferenceResponse();
        BeanUtils.copyProperties(salesCategoryReferences, salesCategoryReferenceResponse);
        CategoryResponse categoryResponse = new CategoryResponse();
        BeanUtils.copyProperties(salesCategoryReferences.getSalesCategory(), categoryResponse);
        salesCategoryReferenceResponse.setSalesCategoryReference(categoryResponse);
        response.getSalesCategoryReferences().add(salesCategoryReferenceResponse);
      }
    }

    for (CategoryReference b2bSalesCategoryReferences : category.getB2bSalesCategoryReferences()) {
      if (!b2bSalesCategoryReferences.isMarkForDelete() && !b2bSalesCategoryReferences.getSalesCategory()
          .isHalalCategory()) {
        CategoryReferenceResponse b2bSalesCategoryReferenceResponse = new CategoryReferenceResponse();
        CategoryResponse categoryResponse = new CategoryResponse();
        BeanUtils.copyProperties(b2bSalesCategoryReferences.getSalesCategory(), categoryResponse);
        b2bSalesCategoryReferenceResponse.setB2bSalesCategoryReference(categoryResponse);
        response.getB2bSalesCategoryReferences().add(b2bSalesCategoryReferenceResponse);
      }
    }

    if (CollectionUtils.isNotEmpty(category.getHalalSalesCategoryReferences())) {
      for (CategoryReference halalSalesCategoryReferences : category.getHalalSalesCategoryReferences()) {
        if (!halalSalesCategoryReferences.isMarkForDelete()) {
          CategoryReferenceResponse halalSalesCategoryReferenceResponse = new CategoryReferenceResponse();
          CategoryResponse categoryResponse = new CategoryResponse();
          BeanUtils.copyProperties(halalSalesCategoryReferences.getSalesCategory(), categoryResponse);
          if (Objects.nonNull(halalSalesCategoryReferences.getSalesCategory().getCatalog())) {
            CatalogResponse catalog = new CatalogResponse();
            BeanUtils.copyProperties(halalSalesCategoryReferences.getSalesCategory().getCatalog(), catalog);
            categoryResponse.setCatalog(catalog);
          }
          halalSalesCategoryReferenceResponse.setHalalSalesCategoryReference(categoryResponse);
          response.getHalalSalesCategoryReferences().add(halalSalesCategoryReferenceResponse);
        }
      }
    }

    return response;
  }

  public static CategoryDetailResponse getCategoryDetailResponse(Category category) {
    CategoryDetailResponse response = new CategoryDetailResponse();
    BeanUtils.copyProperties(category, response, "categoryAttributes", "productCategories", "masterCategoryReferences",
        "salesCategoryReferences", "b2bSalesCategoryReferences", "halalSalesCategoryReferences");
    CatalogResponse catalog = new CatalogResponse();
    BeanUtils.copyProperties(category.getCatalog(), catalog);
    catalog.setCatalogType(category.getCatalog().getCatalogType().toString());
    response.setCatalog(catalog);

    populateOriginalSalesCategory(category, response);

    if (category.getParentCategory() != null) {
      response.setParentCategoryId(category.getParentCategory().getId());
    }
    return response;
  }

  private static void populateOriginalSalesCategory(Category category, CategoryDetailResponse response) {
    OriginalSalesCategoryResponse originalSalesCategoryResponse = null;
    if (Objects.nonNull(category.getOriginalSalesCategory())) {
      originalSalesCategoryResponse = new OriginalSalesCategoryResponse();
      BeanUtils.copyProperties(category.getOriginalSalesCategory(), originalSalesCategoryResponse, "masterCategories");
      response.setOscId(originalSalesCategoryResponse.getId());
    }
    response.setOriginalSalesCategoryResponse(originalSalesCategoryResponse);
  }

  /**
   * convert category information into response along with shipping details
   *
   * @param category
   * @param attributeConfigurationSet
   * @param shippingResponses
   * @param fetchHideForSellerAttributes
   * @return
   */
  public static CategoryDetailAndShippingResponse convertCategoryToCategoryDetailAndShippingResponse(
      Category category, Set<String> attributeConfigurationSet,
      List<ShippingResponse> shippingResponses, boolean fetchHideForSellerAttributes) throws Exception {
    CategoryDetailAndShippingResponse response = new CategoryDetailAndShippingResponse();
    BeanUtils.copyProperties(category, response, "categoryAttributes", "productCategories", "masterCategoryReferences",
        "salesCategoryReferences", "b2bSalesCategoryReferences", "halalSalesCategoryReferences");
    CatalogResponse catalog = new CatalogResponse();
    BeanUtils.copyProperties(category.getCatalog(), catalog);
    catalog.setCatalogType(category.getCatalog().getCatalogType().name());
    response.setCatalog(catalog);

    populateOriginalSalesCategory(category, response);

    if (Objects.nonNull(category.getParentCategory())) {
      response.setParentCategoryId(category.getParentCategory().getId());
    }
    getCategoryAttributes(category, attributeConfigurationSet, response, fetchHideForSellerAttributes);
    getCategoryReferences(category, response);
    response.setShippingResponses(shippingResponses);
    return response;
  }

  private static void getCategoryAttributes(Category category, Set<String> attributeConfigurationSet,
      CategoryDetailResponse response, boolean fetchHideForSellerAttributes) throws Exception {
    if (CollectionUtils.isNotEmpty(category.getCategoryAttributes())) {
      for (CategoryAttribute categoryAttribute : category.getCategoryAttributes()) {
        if (!fetchHideForSellerAttributes && categoryAttribute.getAttribute()
            .isHideForSeller()) {
          // this is to exclude hideForSeller true attributes
          continue;
        }
        ObjectMapper mapper = new ObjectMapper();
        CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
        BeanUtils.copyProperties(categoryAttribute, categoryAttributeResponse);
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(categoryAttribute.getAttribute(), attributeResponse, "allowedAttributeValues",
            "predefinedAllowedAttributeValues", Constants.ATTRIBUTE_TYPE, Constants.VALUE_TYPES);
        attributeResponse.setAttributeType(categoryAttribute.getAttribute().getAttributeType().name());
        if (StringUtils.isNotBlank(categoryAttribute.getAttribute().getValueTypes())) {
          attributeResponse.setValueTypes(
              mapper.readValue(categoryAttribute.getAttribute().getValueTypes(),
                  new TypeReference<>() {}));
        }
        categoryAttributeResponse.setMainAttributeFlag(attributeConfigurationSet.contains(attributeResponse.getAttributeCode()));
        categoryAttributeResponse.setAttribute(attributeResponse);
        response.getCategoryAttributes().add(categoryAttributeResponse);
      }
    }
  }

  private static void getCategoryReferences(Category category, CategoryDetailResponse response) {
    if (CollectionUtils.isNotEmpty(category.getMasterCategoryReferences())) {
      for (CategoryReference masterCategoryReferences : category.getMasterCategoryReferences()) {
        if (!masterCategoryReferences.isMarkForDelete()) {
          CategoryReferenceResponse masterCategoryReferenceResponse = new CategoryReferenceResponse();
          BeanUtils.copyProperties(masterCategoryReferences, masterCategoryReferenceResponse);
          CategoryResponse categoryResponse = new CategoryResponse();
          BeanUtils.copyProperties(masterCategoryReferences.getMasterCategory(), categoryResponse);
          masterCategoryReferenceResponse.setMasterCategoryReference(categoryResponse);
          response.getMasterCategoryReferences().add(masterCategoryReferenceResponse);
        }
      }
    }

    if (CollectionUtils.isNotEmpty(category.getSalesCategoryReferences())) {
      for (CategoryReference salesCategoryReferences : category.getSalesCategoryReferences()) {
        if (!salesCategoryReferences.isMarkForDelete() && !salesCategoryReferences.getSalesCategory()
                .isHalalCategory()) {
          CategoryReferenceResponse salesCategoryReferenceResponse = new CategoryReferenceResponse();
          BeanUtils.copyProperties(salesCategoryReferences, salesCategoryReferenceResponse);
          CategoryResponse categoryResponse = new CategoryResponse();
          BeanUtils.copyProperties(salesCategoryReferences.getSalesCategory(), categoryResponse);
          salesCategoryReferenceResponse.setSalesCategoryReference(categoryResponse);
          response.getSalesCategoryReferences().add(salesCategoryReferenceResponse);
        }
      }
    }

    if (CollectionUtils.isNotEmpty(category.getB2bSalesCategoryReferences())) {
      for (CategoryReference b2bSalesCategoryReferences : category.getB2bSalesCategoryReferences()) {
        if (!b2bSalesCategoryReferences.isMarkForDelete() && !b2bSalesCategoryReferences.getSalesCategory()
                .isHalalCategory()) {
          CategoryReferenceResponse b2bSalesCategoryReferenceResponse = new CategoryReferenceResponse();
          CategoryResponse categoryResponse = new CategoryResponse();
          BeanUtils.copyProperties(b2bSalesCategoryReferences.getSalesCategory(), categoryResponse);
          b2bSalesCategoryReferenceResponse.setB2bSalesCategoryReference(categoryResponse);
          response.getB2bSalesCategoryReferences().add(b2bSalesCategoryReferenceResponse);
        }
      }
    }

    if (CollectionUtils.isNotEmpty(category.getHalalSalesCategoryReferences())) {
      for (CategoryReference halalSalesCategoryReferences : category.getHalalSalesCategoryReferences()) {
        if (!halalSalesCategoryReferences.isMarkForDelete()) {
          CategoryReferenceResponse halalSalesCategoryReferenceResponse = new CategoryReferenceResponse();
          CategoryResponse categoryResponse = new CategoryResponse();
          BeanUtils.copyProperties(halalSalesCategoryReferences.getSalesCategory(), categoryResponse);
          halalSalesCategoryReferenceResponse.setHalalSalesCategoryReference(categoryResponse);
          response.getHalalSalesCategoryReferences().add(halalSalesCategoryReferenceResponse);
        }
      }
    }

  }

  public static ProductAttributeResponse convertProductAttributeToResponse(ProductAttribute productAttribute) {
    ProductAttributeResponse response = new ProductAttributeResponse();
    BeanUtils.copyProperties(productAttribute, response, "productAttributeValues", "attribute");
    AttributeResponse attribute = new AttributeResponse();
    BeanUtils.copyProperties(productAttribute.getAttribute(), attribute, Constants.ATTRIBUTE_TYPE, "allowedAttributeValues",
        "predefinedAllowedAttributeValues");
    attribute.setAttributeType(productAttribute.getAttribute().getAttributeType().toString());
    attribute.setExtractedValue(productAttribute.isExtractedValue());
    attribute.setSizeAttribute(productAttribute.getAttribute().isSizeAttribute());
    response.setAttribute(attribute);
    List<ProductAttributeValueResponse> productAttributeValueResponses = new ArrayList<ProductAttributeValueResponse>();
    for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
      ProductAttributeValueResponse productAttributeValueResponse = new ProductAttributeValueResponse();
      BeanUtils.copyProperties(productAttributeValue, productAttributeValueResponse, "allowedAttributeValue",
          "predefinedAllowedAttributeValue", "descriptiveAttributeValueType");
      productAttributeValueResponse.setDescriptiveAttributeValueType(
          DescriptiveAttributeValueType.valueOf(productAttributeValue.getDescriptiveAttributeValueType().toString()));
      if (productAttributeValueResponse.getDescriptiveAttributeValueType().equals(DescriptiveAttributeValueType.NONE)) {
        AllowedAttributeValueResponse allowedAttributeValue = new AllowedAttributeValueResponse();
        BeanUtils.copyProperties(productAttributeValue.getAllowedAttributeValue(), allowedAttributeValue);
        productAttributeValueResponse.setAllowedAttributeValue(allowedAttributeValue);
      } else {
        PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
            productAttributeValue.getPredefinedAllowedAttributeValue();
        if (predefinedAllowedAttributeValue != null) {
          PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
              new PredefinedAllowedAttributeValueResponse();
          BeanUtils.copyProperties(predefinedAllowedAttributeValue, predefinedAllowedAttributeValueResponse);
          productAttributeValueResponse.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValueResponse);
        } else {
          productAttributeValueResponse
              .setDescriptiveAttributeValue(productAttributeValue.getDescriptiveAttributeValue());
        }
      }
      productAttributeValueResponses.add(productAttributeValueResponse);
    }
    response.setProductAttributeValues(productAttributeValueResponses);
    return response;
  }

  public static GdnRestListResponse<AttributeValueResponse> convertAttributeValueDTOToAttributeValueResponse(
      Page<AttributeValueDTO> attributeValueDTOPage, Pageable pageable, String requestId) {
    List<AttributeValueResponse> attributeResponses = new ArrayList<>();
    if (Objects.nonNull(attributeValueDTOPage)) {
      for (AttributeValueDTO attributeValueDTO : attributeValueDTOPage) {
        AttributeValueResponse attributeValueResponse = new AttributeValueResponse();
        BeanUtils.copyProperties(attributeValueDTO, attributeValueResponse);
        attributeResponses.add(attributeValueResponse);
      }
      return new GdnRestListResponse<>(null, null, true, attributeResponses,
          new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), attributeValueDTOPage.getTotalElements()),
          requestId);
    }
    return new GdnRestListResponse<>(null, null, true, attributeResponses,
        new PageMetaData(pageable.getPageSize(), pageable.getPageNumber(), 0),
        requestId);
  }

  public static ProductCategoryResponse convertProductCategoryToResponse(ProductCategory productCategory) {
    ProductCategoryResponse response = new ProductCategoryResponse();
    BeanUtils.copyProperties(productCategory, response, "product", "category");
    CategoryResponse category = new CategoryResponse();
    BeanUtils.copyProperties(productCategory.getCategory(), category);
    category.setCatalog(getCatalogResponseFromCategory(productCategory.getCategory()));
    response.setCategory(category);
    return response;
  }

  private static CatalogResponse getCatalogResponseFromCategory(Category category) {
    CatalogResponse catalogResponse = new CatalogResponse();
    if (Objects.nonNull(category.getCatalog())) {
      BeanUtils.copyProperties(category.getCatalog(), catalogResponse);
      catalogResponse
          .setCatalogType(Optional.ofNullable(category.getCatalog().getCatalogType().name()).orElse(StringUtils.EMPTY));
    }
    return catalogResponse;
  }

  public static List<ProductItemDetailResponse> convertProductItemsToProductItemDetailResponseList(
      List<ProductItem> productItems) {
    return productItems.stream().map(ConverterUtil::convertProductItemToProductItemDetailResponse)
        .collect(Collectors.toList());
  }

  private static ProductItemDetailResponse convertProductItemToProductItemDetailResponse(ProductItem productItem) {
    List<Image> imageResponses =
        productItem.getProductItemImages().stream().filter(productItemImage -> productItemImage.isMainImages())
            .filter(productItemImage -> !productItemImage.isMarkForDelete())
            .map(ConverterUtil::convertProductItemImageToResponse).collect(Collectors.toList());
    ProductItemDetailResponse response = new ProductItemDetailResponse();
    BeanUtils.copyProperties(productItem, response);
    ProductResponse productResponse = new ProductResponse();
    productResponse.setProductCode(productItem.getProduct().getProductCode());
    productResponse.setId(productItem.getProduct().getId());
    response.setProductResponse(productResponse);
    if (CollectionUtils.isNotEmpty(imageResponses)) {
      response.setImages(imageResponses);
    }
    return response;
  }

  public static Image convertProductImageToResponse(ProductImage productImage) {
    Image imageResponse = new Image();
    BeanUtils.copyProperties(productImage, imageResponse);
    return imageResponse;
  }

  public static Image convertProductItemImageToResponse(ProductItemImage productItemImage) {
    Image imageResponse = new Image();
    BeanUtils.copyProperties(productItemImage, imageResponse);
    return imageResponse;
  }

  public static ProductItemAttributeValueResponse convertProductItemAttributeValueToResponse(
      ProductItemAttributeValue productItemAttributeValue) {
    ProductItemAttributeValueResponse response = new ProductItemAttributeValueResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    BeanUtils.copyProperties(productItemAttributeValue.getAttribute(), attributeResponse, new String[] {
        Constants.ATTRIBUTE_TYPE, "allowedAttributeValues", "predefinedAllowedAttributeValues", "categoryAttributes"});
    attributeResponse.setAttributeType(productItemAttributeValue.getAttribute().getAttributeType().name());
    BeanUtils.copyProperties(productItemAttributeValue, response, new String[] {"productItem", "attribute"});
    response.setAttributeResponse(attributeResponse);
    return response;
  }

  public static ProductItemResponse convertProductItemToResponse(ProductItem productItem, boolean originalImages) {
    ProductItemResponse response = new ProductItemResponse();
    BeanUtils.copyProperties(productItem, response, "product", "productItemAttributeValues", "productItemImages");
    response.setProductItemAttributeValueResponses(new ArrayList<>());
    List<ProductItemImage> productItemImages = productItem.getProductItemImages();
    if (!originalImages) {
      productItemImages =
          productItem.getProductItemImages().stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
              .filter(com.gdn.x.productcategorybase.util.ConverterUtil::filterProcessedItemImages).collect(Collectors.toList());
    }
    for (ProductItemImage productItemImage : productItemImages) {
      response.getImages().add(convertProductItemImageToResponse(productItemImage));
    }
    for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
      response.getProductItemAttributeValueResponses().add(
          convertProductItemAttributeValueToResponse(productItemAttributeValue));
    }
    return response;
  }

  public static ProductDetailResponse convertProductToProductDetailResponse(Product product, boolean originalImages) {
    return convertProductToProductDetailResponse(product, false, originalImages, true);
  }

  public static ProductDetailResponse convertProductToProductDetailResponse(Product product, boolean rejectedProducts,
      boolean originalImages, boolean loadItemResponses) {
    ProductDetailResponse response = new ProductDetailResponse();
    BeanUtils.copyProperties(product, response);
    response.setLongDescription(product.getDescription());
    Set<ProductItemResponse> productItemResponses = new HashSet<>();
    List<ProductCategoryResponse> productCategoryResponses = new ArrayList<>();
    List<ProductAttributeResponse> productAttributeResponses = new ArrayList<>();
    List<Image> productImageResponses = new ArrayList<>();
    if (loadItemResponses) {
      for (ProductItem productItem : product.getProductItems()) {
        productItemResponses.add(convertProductItemToResponse(productItem, originalImages));
      }
      response.setProductItemResponses(productItemResponses);
    }
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      if (!productAttribute.isMarkForDelete() || rejectedProducts) {
        productAttributeResponses.add(convertProductAttributeToResponse(productAttribute));
      }
    }
    for (ProductCategory productCategory : product.getProductCategories()) {
      if (!productCategory.isMarkForDelete() || rejectedProducts) {
        ProductCategoryResponse productCategoryResponse = convertProductCategoryToResponse(productCategory);
        if (!productCategoryResponse.isMarkForDelete()) {
          productCategoryResponses.add(0, productCategoryResponse);
        } else {
          productCategoryResponses.add(productCategoryResponse);
        }
      }
    }
    List<ProductImage> productImages = product.getProductImages();
    if (!originalImages) {
      productImages = product.getProductImages().stream().filter(productImage -> !productImage.isMarkForDelete())
          .filter(productImage -> com.gdn.x.productcategorybase.util.ConverterUtil
              .filterProcessedProductImages(productImage)).collect(Collectors.toList());
    }
    for (ProductImage productImage : productImages) {
      if (!productImage.isMarkForDelete() || rejectedProducts) {
        productImageResponses.add(convertProductImageToResponse(productImage));
      }
    }
    if (StringUtils.isNotBlank(product.getDistributionInfo())) {
      response.setDistributionInfoResponse(CommonUtil.getDistributionInfoResponse(product));
    }

    if (StringUtils.isNotBlank(product.getAiGeneratedFields())) {
      response.setAiGeneratedFieldsResponse(
          CommonUtil.getAIGeneratedFieldsResponse(product.getAiGeneratedFields()));
    }
    response.setProductAttributeResponses(productAttributeResponses);
    response.setProductCategoryResponses(productCategoryResponses);
    response.setImages(productImageResponses);
    return response;
  }

  public static MasterProductResponse convertProductToMasterProductResponse(Product product) {
    MasterProductResponse response = new MasterProductResponse();
    BeanUtils.copyProperties(product, response, "productCategory", "productItems", "productAttributes");
    response.setCategoryCode(product.getProductCategories().stream().findFirst().map(ProductCategory::getCategory)
        .map(Category::getCategoryCode).orElse(null));
    response.setdGLevel(
        product.getProductItems().stream().findFirst().map(ProductItem::getDangerousGoodsLevel).orElse(null));
    return response;
  }

  public static Attribute convertRequestToAttribute(AttributeRequest request) {
    Attribute attribute = new Attribute();
    BeanUtils.copyProperties(request, attribute, "allowedAttributeValues");
    List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<AllowedAttributeValue>();

    for (AllowedAttributeValueRequest allowedAttributeValueRequest : request.getAllowedAttributeValues()) {
      AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
      BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
      allowedAttributeValues.add(allowedAttributeValue);
    }
    attribute.setAllowedAttributeValues(allowedAttributeValues);
    return attribute;
  }

  @Deprecated
  public static Product convertRequestToProduct(ProductRequest request) {
    Product product = new Product();

    List<ProductAttribute> productAttributes = new ArrayList<ProductAttribute>();
    List<ProductItem> productItems = new ArrayList<ProductItem>();
    List<ProductCategory> productCategories = new ArrayList<ProductCategory>();
    BeanUtils.copyProperties(request, product, "productAttributes", "productItems", "productCategories");

    if (request.getProductAttributes() != null) {
      for (ProductAttributeRequest productAttributeRequest : request.getProductAttributes()) {
        ProductAttribute productAttribute = new ProductAttribute();
        BeanUtils.copyProperties(productAttributeRequest, productAttribute, "productAttributeValues");
        productAttributes.add(productAttribute);

        Attribute attribute = new Attribute();
        BeanUtils.copyProperties(productAttributeRequest.getAttribute(), attribute, "allowedAttributeValues");

        List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<AllowedAttributeValue>();
        if (productAttributeRequest.getAttribute().getAllowedAttributeValues() != null) {
          for (AllowedAttributeValueRequest allowedAttributeValueRequest : productAttributeRequest.getAttribute()
              .getAllowedAttributeValues()) {
            AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
            BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
            allowedAttributeValues.add(allowedAttributeValue);
          }
        }
        attribute.setAllowedAttributeValues(allowedAttributeValues);

        List<ProductAttributeValue> productAttributeValues = new ArrayList<ProductAttributeValue>();
        if (productAttributeRequest.getProductAttributeValues() != null) {
          for (ProductAttributeValueRequest productAttributeValueRequest : productAttributeRequest
              .getProductAttributeValues()) {
            ProductAttributeValue productAttributeValue = new ProductAttributeValue();
            BeanUtils.copyProperties(productAttributeValueRequest, productAttributeValue, "allowedAttributeValue");

            AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
            BeanUtils.copyProperties(productAttributeValueRequest.getAllowedAttributeValue(), allowedAttributeValue);
            allowedAttributeValue.setAttribute(attribute);
            productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
            productAttributeValue.setProductAttribute(productAttribute);

            productAttributeValues.add(productAttributeValue);
          }
        }
        productAttribute.setAttribute(attribute);
        productAttribute.setProductAttributeValues(productAttributeValues);
      }
    }
    product.setProductAttributes(productAttributes);

    if (request.getProductItems() != null) {
      for (ProductItemRequest productItemRequest : request.getProductItems()) {
        ProductItem productItem = new ProductItem();
        BeanUtils.copyProperties(productItemRequest, productItem, "productItemAttributeValues");

        if (productItemRequest.getProductItemAttributeValues() != null) {
          for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemRequest
              .getProductItemAttributeValues()) {
            ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
            BeanUtils.copyProperties(productItemAttributeValueRequest, productItemAttributeValue, "attribute");

            Attribute attribute = new Attribute();
            BeanUtils.copyProperties(productItemAttributeValueRequest.getAttribute(), attribute,
                "allowedAttributeValues");

            List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<AllowedAttributeValue>();
            if (productItemAttributeValueRequest.getAttribute().getAllowedAttributeValues() != null) {
              for (AllowedAttributeValueRequest allowedAttributeValueRequest : productItemAttributeValueRequest
                  .getAttribute().getAllowedAttributeValues()) {
                AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
                BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
                allowedAttributeValues.add(allowedAttributeValue);
              }
            }
            attribute.setAllowedAttributeValues(allowedAttributeValues);

            productItemAttributeValue.setProductItem(productItem);
          }
        }
        productItem.setProduct(product);
        productItems.add(productItem);
      }
    }
    product.setProductItems(productItems);

    if (request.getProductCategories() != null) {
      for (ProductCategoryRequest productCategoryRequest : request.getProductCategories()) {
        ProductCategory productCategory = new ProductCategory();
        Category category = new Category();
        Catalog catalog = new Catalog();
        productCategory.setProduct(product);

        BeanUtils.copyProperties(category, productCategoryRequest.getCategory(), "catalog", "parentCategory");

        BeanUtils.copyProperties(productCategoryRequest.getCategory().getCatalog(), catalog);

        setParentCategory(productCategoryRequest.getCategory(), category);
        category.setCatalog(catalog);
        productCategory.setCategory(category);
      }
    }
    product.setProductCategories(productCategories);

    return product;
  }

  public static List<ProductItemImage> convertRequestToProductItemImage(ProductRequest productRequest,
      ProductItemRequest itemRequest, ProductItem productItem) {
    List<ProductItemImage> productItemImages = new ArrayList<>();
    List<Image> source;
    if(CollectionUtils.isEmpty(itemRequest.getImages())) {
      source = productRequest.getImages();
    } else {
      source = itemRequest.getImages();
    }
    int piCounter = 0;
    for (Image image : source) {
      ProductItemImage productItemImage =
          new ProductItemImage(productItem, image.isMainImages(), image.getLocationPath(), image.getSequence());
      productItemImage.setId(image.getId());
      productItemImage.setMarkForDelete(image.isMarkForDelete());
      productItemImage.setHashCode(image.getHashCode());
      productItemImage.setOriginalImage(image.getOriginalImage());
      productItemImage.setActive(image.isActive());;
      productItemImage.setCommonImage(image.isCommonImage());
      if (StringUtils.isEmpty(productItemImage.getStoreId())) {
        productItemImage.setStoreId(productRequest.getStoreId());
      }
      productItemImages.add(productItemImage);
      if (image.isMainImages()) {
        piCounter++;
      }
    }
    if (piCounter == 0) {
      for (ProductItemImage image : productItemImages) {
        if (image.getSequence() != null && image.getSequence() == 0
            && !image.isMainImages()) {
          image.setMainImages(true);
        }
      }
    }
    return productItemImages;
  }

  private static void setParentCategory(CategoryRequest request, Category category) {
    if (request.getParentCategory() != null) {
      Category parentCategory = new Category();
      BeanUtils.copyProperties(request.getParentCategory(), parentCategory);
      category.setParentCategory(parentCategory);
      setParentCategory(request.getParentCategory(), parentCategory);
    }
  }

  public static List<CategoryResponse> getPaginatedCategoryList(List<CategoryResponse>
      categoryList, int page, int size) {
    //all logic is based on the fact that 1st page will be called with page-number 0
    int startIndex = 0;
    int categorySize = categoryList.size();
    List<CategoryResponse> filteredCategoryList = Collections.EMPTY_LIST;
    if (page > 0) {
      startIndex = page * size;
    }
    int endIndex = startIndex + size;
    if (startIndex < categorySize) {
      if (endIndex < categorySize) {
        filteredCategoryList = categoryList.subList(startIndex, endIndex);
      } else {
        filteredCategoryList = categoryList.subList(startIndex, categorySize);
      }
    }
    return filteredCategoryList;
  }

  public static void sortProductImagesBySequenceId(Product product){
    if(null != product && (!CollectionUtils.isEmpty(product.getProductImages()))) {
      Collections.sort(product.getProductImages(), new Comparator<ProductImage>() {
        @Override
        public int compare(ProductImage productImage1, ProductImage productImage2) {
          return productImage1.getSequence().compareTo(productImage2.getSequence());
        }
      });
    }
  }

  public static void sortProductItemImagesBySequenceId(ProductItem productItem){
    if(null != productItem && (!CollectionUtils.isEmpty(productItem.getProductItemImages()))) {
      Collections.sort(productItem.getProductItemImages(), new Comparator<ProductItemImage>() {
        @Override
        public int compare(ProductItemImage productItemImage1, ProductItemImage productItemImage2) {
          return productItemImage1.getSequence().compareTo(productItemImage2.getSequence());
        }
      });
    }
  }

  public static List<ProductItemDetailResponse> convertToProductItemDetailResponse(
      List<ProductItem> productItemList, boolean originalImages) {
    List<ProductItemDetailResponse> productItemResponses = new ArrayList<>();
    for (ProductItem productItem : productItemList) {
      ProductItemDetailResponse response = new ProductItemDetailResponse();
      BeanUtils.copyProperties(productItem, response);
      if (!CollectionUtils.isEmpty(productItem.getProductItemImages())) {
        List<ProductItemImage> productItemImages = productItem.getProductItemImages();
        productItemImages = filterProductItemImages(originalImages, productItemImages);
        response.setImages(new ArrayList<>());
        for (ProductItemImage image : productItemImages) {
          if (image.isMainImages() && !image.isMarkForDelete()) {
            Image imageResponse = convertProductItemImageToResponse(image);
            response.getImages().add(imageResponse);
          }
        }
      }
      response.setProductResponse(new ProductResponse());
      response.getProductResponse().setProductCode(productItem.getProduct().getProductCode());
      response.getProductResponse().setId(productItem.getProduct().getId());
      productItemResponses.add(response);
    }
    return productItemResponses;
  }

  private static List<PredefinedAllowedAttributeValueResponse> convertToPredefinedAllowedAttributeValueResponses(
      List<PredefinedAllowedAttributeValue> predefinedAllowedAttributeValues) {
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses =
        new ArrayList<>();
    if (!CollectionUtils.isEmpty(predefinedAllowedAttributeValues)) {
      for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : predefinedAllowedAttributeValues) {
        PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
            new PredefinedAllowedAttributeValueResponse();
        BeanUtils.copyProperties(predefinedAllowedAttributeValue,
            predefinedAllowedAttributeValueResponse, "attribute");
        predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse);
      }
    }
    return predefinedAllowedAttributeValueResponses;
  }

  private static List<AllowedAttributeValueResponse> convertToAllowedAttributeValueResponses(
      List<AllowedAttributeValue> allowedAttributeValues) {
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    if (!CollectionUtils.isEmpty(allowedAttributeValues)) {
      for (AllowedAttributeValue allowedAttributeValue : allowedAttributeValues) {
        AllowedAttributeValueResponse allowedAttributeValueResponse =
            new AllowedAttributeValueResponse();
        BeanUtils.copyProperties(allowedAttributeValue, allowedAttributeValueResponse, "attribute");
        allowedAttributeValueResponses.add(allowedAttributeValueResponse);
      }
    }
    return allowedAttributeValueResponses;
  }

  public static List<AttributeResponse> convertToAttributeResponses(List<Attribute> attributes,
      boolean fetchAttributeBasicDetails) {
    List<AttributeResponse> attributeResponses = new ArrayList<AttributeResponse>();
    if (!CollectionUtils.isEmpty(attributes)) {
      for (Attribute attribute : attributes) {
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(attribute, attributeResponse, "allowedAttributeValues",
            "predefinedAllowedAttributeValues");
        attributeResponse.setSkuValue(attribute.isSkuValue());
        attributeResponse.setAttributeType(attribute.getAttributeType().name());
        if (!fetchAttributeBasicDetails) {
          attributeResponse.setPredefinedAllowedAttributeValues(
              ConverterUtil.convertToPredefinedAllowedAttributeValueResponses(
                  attribute.getPredefinedAllowedAttributeValues()));
          attributeResponse.setAllowedAttributeValues(
              ConverterUtil.convertToAllowedAttributeValueResponses(attribute.getAllowedAttributeValues()));
        }
        attributeResponses.add(attributeResponse);
      }
    }
    return attributeResponses;
  }

  public static AttributeResponse toAttributeResponse(Attribute attribute) {
    AttributeResponse response = new AttributeResponse();
    BeanUtils.copyProperties(attribute, response, "allowedAttributeValues", "predefinedAllowedAttributeValues");
    response.setAttributeType(attribute.getAttributeType().toString());
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<AllowedAttributeValueResponse>();
    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses =
        new ArrayList<PredefinedAllowedAttributeValueResponse>();
    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      for (AllowedAttributeValue allowedAttributeValue : attribute.getAllowedAttributeValues()) {
        if (!allowedAttributeValue.isMarkForDelete()) {
          AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
          BeanUtils.copyProperties(allowedAttributeValue, allowedAttributeValueResponse);
          allowedAttributeValueResponses.add(allowedAttributeValueResponse);
        }
      }
    } else if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())
      || AttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType())) {
      for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : attribute.getPredefinedAllowedAttributeValues()) {
        if (!predefinedAllowedAttributeValue.isMarkForDelete()) {
          PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
            new PredefinedAllowedAttributeValueResponse();
          BeanUtils.copyProperties(predefinedAllowedAttributeValue,
            predefinedAllowedAttributeValueResponse);
          predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse);
        }
      }
    }
    response.setAllowedAttributeValues(allowedAttributeValueResponses);
    response.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueResponses);
    return response;
  }

  public static List<CategoryResponse> convertToCategoryResponse(List<Category> categories) {
    List<CategoryResponse> response = new ArrayList<>();
    categories.forEach(category -> {
      CategoryResponse categoryResponse = new CategoryResponse();
      BeanUtils.copyProperties(category, categoryResponse);
      response.add(categoryResponse);
    });
    return response;
  }

  public static Map<String, String> createAttributeAndItemIdMap(Product product, boolean valueTypeAdditionForDefiningAttributes, String sizeChartValueTypeDelimiter) {
    Map<String, Map<String, String>> attributeIdAndValueAndValueTypeMap = getValueAndValueTypeMapFromProductAttributes(product);
    return product.getProductItems().stream().map(productItem -> {
      Map<String, String> attributeTreeMap = productItem.getProductItemAttributeValues().stream()
          .filter(productItemAttributeValue -> isDefiningOrVariantCreationTrue(productItemAttributeValue)).collect(
              Collectors.toMap(productItemAttributeValue -> productItemAttributeValue.getAttribute().getAttributeCode(),
                  productItemAttributeValue -> combineAttributeValueAndValueTypeInAttributeMap(
                      productItemAttributeValue.getValue(), getAttributeId(productItemAttributeValue),
                      attributeIdAndValueAndValueTypeMap, valueTypeAdditionForDefiningAttributes,
                      sizeChartValueTypeDelimiter),
                  (productItemAttributeValue1, productItemAttributeValue2) -> productItemAttributeValue1,
                  TreeMap::new));
      return new ImmutablePair<>(attributeTreeMap, productItem.getId());
    }).collect(Collectors
        .toMap(pair -> Base64.encodeBase64String(pair.getLeft().toString().getBytes()), pair -> pair.getRight()));
  }

  private static Map<String, Map<String, String>> getValueAndValueTypeMapFromProductAttributes(Product product) {
    Map<String, Map<String, String>> attributeIdAndValueAndValueTypeMap = new HashMap<>();
    List<ProductAttribute> productAttributes =
        Optional.ofNullable(product.getProductAttributes()).orElse(new ArrayList<>());
    for (ProductAttribute productAttribute : productAttributes) {
      List<ProductAttributeValue> productAttributeValues =
          Optional.ofNullable(productAttribute.getProductAttributeValues()).orElse(new ArrayList<>());
      for (ProductAttributeValue productAttributeValue : productAttributeValues) {
        if (Objects.nonNull(productAttributeValue.getAllowedAttributeValue()) && StringUtils.isNotEmpty(
            productAttributeValue.getAllowedAttributeValue().getValueType())) {
          Map<String, String> valueAndValueTypeMap =
              attributeIdAndValueAndValueTypeMap.getOrDefault(getAttributeId(productAttribute), new HashMap<>());
          valueAndValueTypeMap.put(productAttributeValue.getAllowedAttributeValue().getValue(),
              productAttributeValue.getAllowedAttributeValue().getValueType());
          attributeIdAndValueAndValueTypeMap.put(getAttributeId(productAttribute), valueAndValueTypeMap);
        }
      }
    }

    return attributeIdAndValueAndValueTypeMap;
  }

  private static String getAttributeId(ProductAttribute productAttribute) {
    return Optional.ofNullable(productAttribute.getAttribute()).map(Attribute::getId)
        .orElse(productAttribute.getAttributeId());
  }

  private static String getAttributeId(ProductItemAttributeValue productItemAttributeValue) {
    return Optional.ofNullable(productItemAttributeValue.getAttribute()).map(Attribute::getId)
        .orElse(productItemAttributeValue.getAttributeId());
  }

  private static String combineAttributeValueAndValueTypeInAttributeMap(String value, String attributeId,
      Map<String, Map<String, String>> attributeCodeAndValueAndValueTypeMap,
      boolean valueTypeAdditionForDefiningAttributes, String sizeChartValueTypeDelimiter) {
    String valueType = Optional.ofNullable(attributeCodeAndValueAndValueTypeMap.get(attributeId))
        .map(valueAndValueTypeMap -> valueAndValueTypeMap.get(value)).orElse(StringUtils.EMPTY);
    if (valueTypeAdditionForDefiningAttributes && StringUtils.isNotEmpty(valueType)) {
      return combineAttributeValueAndValueType(valueType, value, sizeChartValueTypeDelimiter);
    } else {
      return value;
    }
  }

  private static String combineAttributeValueAndValueType(String valueType, String value, String sizeChartValueTypeDelimiter) {
    return valueType + sizeChartValueTypeDelimiter + value;
  }

  private static Map<String, ProductItemAttributeValueRequest> convertProductItemToMapOfItemAndColourFamily(
      ProductRequest request) {
    if (!CollectionUtils.isEmpty(request.getProductItems())) {
      Map<String, ProductItemAttributeValueRequest> map = new HashMap<>();
      for (ProductItemRequest productItem : request.getProductItems()) {
        if (!CollectionUtils.isEmpty(productItem.getProductItemAttributeValues())) {
          map.put(productItem.getGeneratedItemName(), productItem.getProductItemAttributeValues().get(0));
        }
      }
      return map;
    }
    return null;
  }

  // The only attribute sent is itemRequest is family colour
  private static Attribute getFamilyColourAttribute(ProductRequest request) {
    AttributeRequest colourFamily =
        request.getProductItems().stream().map(ProductItemRequest::getProductItemAttributeValues).flatMap(List::stream)
            .findFirst().map(ProductItemAttributeValueRequest::getAttribute).orElse(null);
    if (Objects.nonNull(colourFamily) && Constants.FAMILY_COLOUR.equals(colourFamily.getName())) {
      return convertRequestToAttribute(colourFamily);
    } else {
      return null;
    }
  }

  private static Map<String, String> generateFamilyColourMap(Product entity,
      Map<String, ProductItemAttributeValueRequest> itemNamesWithColourFamily) {
    Map<String, String> familyColourMap = new HashMap<>();
    for (ProductAttribute productAttribute : entity.getProductAttributes()) {
      if ((WARNA.equalsIgnoreCase(productAttribute.getAttribute().getName()) || COLOR
          .equalsIgnoreCase(productAttribute.getAttribute().getNameEnglish()))
          && isDefiningAttributeOrVariantCreationTrue(productAttribute.getAttribute())) {
        for (ProductAttributeValue productAttributeValue : productAttribute.getProductAttributeValues()) {
          String warna = getAllowedAttributeValueOrDescriptiveValue(productAttributeValue);
          if (!familyColourMap.containsKey(warna)) {
            String itemName = itemNamesWithColourFamily.keySet().stream()
                .filter(itemGeneratedName -> itemGeneratedName.contains(warna)).findFirst().orElse(StringUtils.EMPTY);
            ProductItemAttributeValueRequest productItemAttributeValue = itemNamesWithColourFamily.get(itemName);
            if (Objects.nonNull(productItemAttributeValue)) {
              familyColourMap.put(warna, productItemAttributeValue.getValue());
            }
          }
        }
      }
    }
    return familyColourMap;
  }

  private static boolean isDefiningAttributeOrVariantCreationTrue(Attribute attribute) {
    return AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()) || attribute.isVariantCreation();
  }

  private static String getAllowedAttributeValueOrDescriptiveValue(ProductAttributeValue productAttributeValue) {
    if (productAttributeValue.getProductAttribute().getAttribute().getAttributeType()
        .equals(AttributeType.DESCRIPTIVE_ATTRIBUTE)) {
      return productAttributeValue.getDescriptiveAttributeValue();
    }
    return productAttributeValue.getAllowedAttributeValue().getValue();
  }

  public static void setRequiredData(String storeId, Product product,
      Map<Map<String, String>, Map<String, Object>> dataMap, Map<String, Attribute> itemAttributeMap,
      String sourceDirectory, String fullImageSourceDirectory, ProductRequest request, boolean skipMissingVariants,
      boolean ranchIntegrationEnabled, Set<String> distributionSellerList) throws Exception {
    Set<String> uniqueImages = product.getProductImages().stream().map(productImages -> productImages.getLocationPath())
        .collect(Collectors.toSet());
    AtomicInteger imageUploadCounter = new AtomicInteger(0);
    boolean isProductMainImageSet = CollectionUtils.isNotEmpty(product.getProductImages());
    int productImageSequenceNumber = product.getProductImages().size();
    Set<String> uploadedImageLocations = Collections.synchronizedSet(new HashSet<>());
    List<ProductItem> productItems = new ArrayList<>();
    for(ProductItem productItem : product.getProductItems()) {
      productItem.setVatApplicable(Boolean.TRUE);  //set vat applicable flag to true
      productItem.setCreatedMerchant(product.getCreatedMerchant());
      Map<String, String> attributesMap = new TreeMap<>();
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        if (isDefiningOrVariantCreationTrue(productItemAttributeValue)) {
          attributesMap.put(productItemAttributeValue.getAttribute().getAttributeCode(), productItemAttributeValue.getValue());
        }
      }
      if (Objects.isNull(dataMap.get(attributesMap))) {
        if (skipMissingVariants) {
          continue;
        }
        if (!request.isIgnoreMissingItems()) {
          log.error("No matching attribute map recieved to match to this item. dataMap : {}, attributeMap : {}",
              dataMap, attributesMap);
          throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
              "Not able to match any item to attribute map " + attributesMap.toString());
        }
        // Create missing items family colour attribute if it exists
        List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
        // Checking if family colour exists
        Attribute familyColourAttribute = getFamilyColourAttribute(request);
        if (Objects.nonNull(familyColourAttribute)) {
          Map<String, ProductItemAttributeValueRequest> itemNamesWithColourFamily =
              convertProductItemToMapOfItemAndColourFamily(request);
          Map<String, String> familyColourMap = generateFamilyColourMap(product, itemNamesWithColourFamily);
          String warna = familyColourMap.keySet().stream()
              .filter(warnaValue -> productItem.getGeneratedItemName().contains(warnaValue)).findFirst()
              .orElse(StringUtils.EMPTY);
          ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
          productItemAttributeValue.setAttribute(itemAttributeMap.get(familyColourAttribute.getId()));
          productItemAttributeValue.setProductItem(productItem);
          productItemAttributeValue.setValue(familyColourMap.getOrDefault(warna, StringUtils.EMPTY));
          productItemAttributeValues.add(productItemAttributeValue);
          if (StringUtils.isEmpty(productItemAttributeValue.getStoreId())) {
            productItemAttributeValue.setStoreId(productItem.getStoreId());
          }
        }
        // Setting UPC code as empty
        productItem.setUpcCode(StringUtils.EMPTY);

        // The attribute which will get added here is family colour, if it exists
        productItem.getProductItemAttributeValues().addAll(productItemAttributeValues);

        // Set product images as item images
        List<Image> images = request.getImages();
        productItem.setDangerousGoodsLevel(0);
        if (CollectionUtils.isNotEmpty(images)) {
          Collections.sort(images, Comparator.comparing(Image::isMainImages).reversed());
          List<String> uniqueItemImages = new ArrayList<>();
          for (Image image : images) {
            if (uniqueItemImages.contains(image.getLocationPath())) {
              continue;
            }
            ProductItemImage productItemImage = convertToProductItemImages(image, product,
              sourceDirectory, fullImageSourceDirectory, uploadedImageLocations, imageUploadCounter);
            productItemImage.setProductItem(productItem);
            productItem.getProductItemImages().add(productItemImage);
            uniqueItemImages.add(image.getLocationPath());
            if (!uniqueImages.contains(image.getLocationPath())) {
              ProductImage productImage = convertRequestToProductImage(storeId, product, image,
                sourceDirectory, fullImageSourceDirectory, uploadedImageLocations, imageUploadCounter);
              if (!isProductMainImageSet && productImage.isMainImages()) {
                isProductMainImageSet = true;
              } else if (isProductMainImageSet && productImage.isMainImages()) {
                productImage.setMainImages(false);
              }
              productImage.setSequence(productImageSequenceNumber++);
              product.getProductImages().add(productImage);
              uniqueImages.add(image.getLocationPath());
            }
          }
        }
        productItems.add(productItem);
        continue;
      }
      productItems.add(productItem);
      productItem.setUpcCode(String.valueOf(dataMap.get(attributesMap).get(UPC_CODE_PARAM)));
      productItem.setContentChanged(Boolean.valueOf(String.valueOf(dataMap.get(attributesMap).get(CONTENT_CHANGED))));
      if (ranchIntegrationEnabled) {
        setItemDistributionInfo(storeId, dataMap, request, productItem, attributesMap);
        if (Optional.ofNullable(distributionSellerList).orElse(new HashSet<>())
            .contains(product.getCreatedMerchant())) {
          productItem.setOmniChannelSku(String.valueOf(
              Optional.ofNullable(dataMap.get(attributesMap).get(OMNI_CHANNEL_SKU)).orElse(StringUtils.EMPTY)));
        }
      }
      if (Objects.nonNull(dataMap.get(attributesMap).get(SOURCE_ITEM_CODE))) {
        productItem.setSourceItemCode(String.valueOf(dataMap.get(attributesMap).get(SOURCE_ITEM_CODE)));
      }
      productItem.getProductItemAttributeValues().addAll(
          ConverterUtil.convertProductItemAttributeRequests(
              (List<ProductItemAttributeValueRequest>) dataMap.get(attributesMap).get(ITEM_ATTRIBUTES),
              productItem, itemAttributeMap));
      removeDuplicateProductItemAttribute(productItem);
      List<Image> images = (List<Image>) dataMap.get(attributesMap).get(ITEM_IMAGES_PARAM);
      productItem.setDangerousGoodsLevel(0);
      if(CollectionUtils.isNotEmpty(images)) {
        Collections.sort(images, Comparator.comparing(Image::isMainImages).reversed());
        List<String> uniqueItemImages = new ArrayList<>();
        for (Image itemImage : images) {
          if (uniqueItemImages.contains(itemImage.getLocationPath())) {
            continue;
          }
          ProductItemImage productItemImage = convertToProductItemImages(itemImage, product, sourceDirectory, fullImageSourceDirectory,
            uploadedImageLocations, imageUploadCounter);
          productItemImage.setProductItem(productItem);
          productItem.getProductItemImages().add(productItemImage);
          uniqueItemImages.add(itemImage.getLocationPath());
          if (!uniqueImages.contains(itemImage.getLocationPath())) {
            ProductImage productImage = convertRequestToProductImage(storeId, product, itemImage, sourceDirectory, fullImageSourceDirectory,
              uploadedImageLocations, imageUploadCounter);
            if (!isProductMainImageSet && productImage.isMainImages()) {
              isProductMainImageSet = true;
            } else if (isProductMainImageSet && productImage.isMainImages()) {
              productImage.setMainImages(false);
            }
            productImage.setSequence(productImageSequenceNumber++);
            product.getProductImages().add(productImage);
            uniqueImages.add(itemImage.getLocationPath());
          }
        }
      }
    }
    log.info("Total Calls made to GCS for Image Uploads was : {} for product : {} ",
      imageUploadCounter.get(), product.getProductCode());
    if (skipMissingVariants) {
      if (CollectionUtils.isNotEmpty(productItems)) {
        product.setProductItems(productItems);
      } else {
        log.error("No item found to create product : {}", product);
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            ErrorMessage.NO_ITEM_FOUND_TO_CREATE_PRODUCT.getMessage());
      }
    }
    setProductItemImageForMissingVariants(product);
  }

  private static void setItemDistributionInfo(String storeId, Map<Map<String, String>, Map<String, Object>> dataMap, ProductRequest request,
      ProductItem productItem, Map<String, String> attributesMap) throws JsonProcessingException {
    ProductItemUomInfoDTO productItemUomInfoDTO =
        (ProductItemUomInfoDTO) dataMap.get(attributesMap).get(ITEM_DISTRIBUTION_INFO);
    setItemUomInfo(storeId, request, productItem, productItemUomInfoDTO);
  }

  public static void setItemUomInfo(String storeId, ProductRequest request, ProductItem productItem,
      ProductItemUomInfoDTO productItemUomInfoDTO) throws JsonProcessingException {
    if (Objects.nonNull(productItemUomInfoDTO) && CollectionUtils.isNotEmpty(
        productItemUomInfoDTO.getDimensionAndUomDTOList()) && Objects.nonNull(
        productItemUomInfoDTO.getDistributionItemInfoRequest())) {
      log.info("Setting distribution item info for product : {} and skuCode : {} with request : {} ",
          request.getProductCode(), productItem.getSkuCode(), productItemUomInfoDTO);
      ProductItemUomInfo productItemUomInfo = new ProductItemUomInfo();
      productItemUomInfo.setStoreId(storeId);
      productItemUomInfo.setOrigin(Origin.valueOf(
          Optional.ofNullable(productItemUomInfoDTO.getDistributionItemInfoRequest().getOrigin())
              .orElse(Origin.LOCAL.name())));
      productItemUomInfo.setExpiry(productItemUomInfoDTO.getDistributionItemInfoRequest().isExpiry());
      ObjectMapper objectMapper = new ObjectMapper();
      productItemUomInfo.setUom(objectMapper.writeValueAsString(productItemUomInfoDTO.getDimensionAndUomDTOList()));
      productItemUomInfo.setSkuCode(productItem.getSkuCode());
      productItemUomInfo.setSellerCode(request.getCreatedMerchant());
      productItemUomInfo.setProductCode(request.getProductCode());
      productItem.setOmniChannelSku(productItemUomInfoDTO.getDistributionItemInfoRequest().getOmniChannelSku());
      productItemUomInfo.setProductItem(productItem);
      productItem.setProductItemUomInfo(productItemUomInfo);
    }
  }

  public static void removeDuplicateProductItemAttribute(ProductItem productItem) {
    Map<String, List<ProductItemAttributeValue>> productItemAttributeValueGroup =
        Optional.ofNullable(productItem.getProductItemAttributeValues()).orElse(new ArrayList<>()).stream()
            .collect(Collectors.groupingBy(ConverterUtil::itemAttributeValueKey));
    if (MapUtils.isNotEmpty(productItemAttributeValueGroup)) {
      productItem.setProductItemAttributeValues(productItemAttributeValueGroup.values().stream()
          .map(productItemAttributeValues -> productItemAttributeValues.stream().findFirst().get())
          .collect(Collectors.toList()));
    }
  }

  private static String itemAttributeValueKey(ProductItemAttributeValue productItemAttributeValue) {
    return Optional.ofNullable(productItemAttributeValue.getProductItem()).orElse(new ProductItem()).getSkuCode()
        + Constants.HYPHEN + Optional.ofNullable(productItemAttributeValue.getAttribute()).orElse(new Attribute())
        .getAttributeCode();
  }

  public static void setProductItemImageForMissingVariants(Product product) {
    List<ProductImage> productImages =
        product.getProductImages().stream().filter(productImage -> !productImage.isMarkForDelete())
            .collect(Collectors.toList());
    if (CollectionUtils.isNotEmpty(productImages)) {
      ProductImage mainProductImage =
          productImages.stream().filter(ProductImage::isMainImages).findFirst().orElse(productImages.get(0));
      for (ProductItem productItem : product.getProductItems())
        if (CollectionUtils.isEmpty(productItem.getProductItemImages())) {
          ProductItemImage productItemImage = new ProductItemImage();
          BeanUtils.copyProperties(mainProductImage, productItemImage, "product", "productId",
              "productItem", "productItemId", Constants.ID);
          productItemImage.setProductItem(productItem);
          productItemImage.setStoreId(mainProductImage.getStoreId());
          List<ProductItemImage> productItemImages = new ArrayList<>();
          productItemImages.add(productItemImage);
          productItem.setProductItemImages(productItemImages);
        }
    }
  }

  private static boolean isDefiningOrVariantCreationTrue(ProductItemAttributeValue productItemAttributeValue) {
    return AttributeType.DEFINING_ATTRIBUTE.equals(productItemAttributeValue.getAttribute().getAttributeType())
        || productItemAttributeValue.getAttribute().isVariantCreation();
  }

  public static List<ProductItemAttributeValue> convertProductItemAttributeRequests(List<ProductItemAttributeValueRequest> productItemAttributeValueRequests,
      ProductItem productItem, Map<String, Attribute> itemAttributeMap) {
    if (Objects.isNull(productItemAttributeValueRequests)) {
      return Collections.emptyList();
    }
    List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
    for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemAttributeValueRequests){
      ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
      BeanUtils.copyProperties(productItemAttributeValueRequest, productItemAttributeValue,
          Constants.ID);
      productItemAttributeValue.setAttribute(itemAttributeMap.get(productItemAttributeValueRequest
          .getAttribute().getId()));
      productItemAttributeValue.setProductItem(productItem);
      productItemAttributeValues.add(productItemAttributeValue);
      if(StringUtils.isEmpty(productItemAttributeValue.getStoreId())) {
        productItemAttributeValue.setStoreId(productItem.getStoreId());
      }
    }
    return productItemAttributeValues;
  }

  public static ProductItemImage convertToProductItemImages(Image image, Product product, String imageSourceDirectory,
      String fullImageSourceDirectory, Set<String> uploadedImageLocations, AtomicInteger imageUploadCounter) throws Exception {
    ProductItemImage productItemImage = new ProductItemImage();
    updateImageLocation(image, product.getProductCode(), imageSourceDirectory, fullImageSourceDirectory,
      uploadedImageLocations, imageUploadCounter);
    BeanUtils.copyProperties(image, productItemImage, Constants.ID);
    if(StringUtils.isEmpty(productItemImage.getStoreId())) {
      productItemImage.setStoreId(product.getStoreId());
    }
    return productItemImage;
  }

  public static ProductImage convertRequestToProductImage(String storeId, Product product,
      Image productImageRequest, String imageSourceDirectory, String fullImageSourceDirectory,
    Set<String> uploadedImageLocations, AtomicInteger imageUploadCounter) throws Exception {
    ProductImage productImage =
        new ProductImage(product, productImageRequest.isMainImages(), productImageRequest.getLocationPath(),
            productImageRequest.getSequence(), storeId);
    updateImageLocation(productImageRequest, product.getProductCode(), imageSourceDirectory,
        fullImageSourceDirectory, uploadedImageLocations, imageUploadCounter);
    BeanUtils.copyProperties(productImageRequest, productImage, "product", Constants.ID);
    if(StringUtils.isEmpty(productImage.getStoreId())) {
      productImage.setStoreId(storeId);
    }
    return productImage;
  }

  public static ProductImage convertRequestToProductImage(String storeId, Product product,
      Image productImageRequest) {
    ProductImage productImage =
        new ProductImage(product, productImageRequest.isMainImages(), productImageRequest.getLocationPath(),
            productImageRequest.getSequence(), storeId);
    BeanUtils.copyProperties(productImageRequest, productImage, "product");
    if (StringUtils.isEmpty(productImage.getStoreId())) {
      productImage.setStoreId(storeId);
    }
    return productImage;
  }

  public static Map<Map<String, String>, Map<String, Object>> generateItemDataHashMap(ProductRequest request, String sizeChartValueTypeDelimiter) {
    return Optional.ofNullable(request.getProductItems()).orElse(new ArrayList<>()).stream().collect(Collectors.toMap(
        productItemRequest -> removeValueTypeFromAttributeMap(productItemRequest.getAttributesMap(),
            sizeChartValueTypeDelimiter), productItemRequest -> {
          Map<String, Object> dataMap = new HashMap<>();
          dataMap.put(UPC_CODE_PARAM, productItemRequest.getUpcCode());
          dataMap.put(ITEM_IMAGES_PARAM, productItemRequest.getImages());
          dataMap.put(ITEM_ATTRIBUTES, productItemRequest.getProductItemAttributeValues());
          dataMap.put(SOURCE_ITEM_CODE, productItemRequest.getSourceItemCode());
          dataMap.put(CONTENT_CHANGED, productItemRequest.isContentChanged());
          dataMap.put(ITEM_DISTRIBUTION_INFO, productItemRequest.getProductItemUomInfoDTO());
          dataMap.put(OMNI_CHANNEL_SKU, productItemRequest.getOmniChannelSku());
          return dataMap;
        }));
  }



  public static SimpleMasterProductUpdateDTO getSimpleMasterProductUpdateDTO(
      SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest){
    SimpleMasterProductUpdateDTO simpleMasterProductUpdateDTO = new SimpleMasterProductUpdateDTO();
    BeanUtils.copyProperties(simpleMasterProductUpdateRequest, simpleMasterProductUpdateDTO);
    return simpleMasterProductUpdateDTO;
  }

  public static SimpleMasterProductUpdateResponse getSimpleMasterProductUpdateResponse(
      SimpleMasterProductUpdateResponseDTO simpleMasterProductUpdateResponseDTO) {
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse = new SimpleMasterProductUpdateResponse();
    BeanUtils.copyProperties(simpleMasterProductUpdateResponseDTO, simpleMasterProductUpdateResponse);
    return simpleMasterProductUpdateResponse;
  }

  /**
   * create list as per there type
   * @param allowedAttributeValuesIds
   * @param predefinedAllowedAttributeValuesIds
   * @param predefinedAllowedAttributeValuesCodes
   * @param productAttributeValueRequest
   */
  public static void getAttributeValuesId(Set<String> allowedAttributeValuesIds,
      Set<String> predefinedAllowedAttributeValuesIds, Set<String> predefinedAllowedAttributeValuesCodes,
      ProductAttributeValueRequest productAttributeValueRequest) {
    if (com.gdn.x.productcategorybase.DescriptiveAttributeValueType.NONE.toString()
        .equals(productAttributeValueRequest.getDescriptiveAttributeValueType().toString())) {
      allowedAttributeValuesIds.add(productAttributeValueRequest.getAllowedAttributeValue().getId());
    } else {
      setPredefinedAttributesValuesIds(predefinedAllowedAttributeValuesIds, predefinedAllowedAttributeValuesCodes,
          productAttributeValueRequest);
    }
  }

  /**
   * setting predefined attribute value in predefinedAllowedAttributeValuesIds, if id is present,
   * otherwise set code in predefinedAllowedAttributeValuesIds
   * @param predefinedAllowedAttributeValuesIds
   * @param predefinedAllowedAttributeValuesCodes
   * @param productAttributeValueRequest
   */
  public static void setPredefinedAttributesValuesIds(Set<String> predefinedAllowedAttributeValuesIds,
      Set<String> predefinedAllowedAttributeValuesCodes, ProductAttributeValueRequest productAttributeValueRequest) {
    if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
      if (StringUtils.isNotEmpty(productAttributeValueRequest.getPredefinedAllowedAttributeValue().getId())) {
        predefinedAllowedAttributeValuesIds
            .add(productAttributeValueRequest.getPredefinedAllowedAttributeValue().getId());
      } else {
        if (StringUtils.isNotEmpty(productAttributeValueRequest.getPredefinedAllowedAttributeValue()
            .getPredefinedAllowedAttributeCode())) {
          predefinedAllowedAttributeValuesCodes.add(
              productAttributeValueRequest.getPredefinedAllowedAttributeValue()
                  .getPredefinedAllowedAttributeCode());
        }
      }
    }
  }

  /**
   * filter out attributes and values Id into corresponding sets
   *
   * @param allowedAttributeValuesIds
   * @param predefinedAllowedAttributeValuesIds
   * @param predefinedAllowedAttributeValuesCodes
   * @param productAttribute
   * @return
   */
  public static String getAttributeIds(Set<String> allowedAttributeValuesIds, Set<String> predefinedAllowedAttributeValuesIds,
      Set<String> predefinedAllowedAttributeValuesCodes, ProductAttributeRequest productAttribute) {
    productAttribute.getProductAttributeValues().forEach(productAttributeValueRequest -> ConverterUtil
        .getAttributeValuesId(allowedAttributeValuesIds, predefinedAllowedAttributeValuesIds,
            predefinedAllowedAttributeValuesCodes, productAttributeValueRequest));
    return productAttribute.getAttribute().getId();
  }

  private static Attribute setAttributeTypeAndSortType(Attribute attribute,
      MasterAttributeRequest request) {
    if (Objects.nonNull(request.getAttributeType())) {
      attribute.setAttributeType(AttributeType.valueOf(request.getAttributeType().toString()));
    }
    if (Objects.nonNull(request.getSortType())) {
      attribute.setSortType(AttributeSortType.valueOf(request.getSortType().toString()));
    }
    return attribute;
  }

  private static Attribute setAllowedAttribute(Attribute attribute, MasterAttributeRequest
      request, String storeId) {
    for (AllowedAttributeValueRequest allowedAttributeValueRequest : request
        .getAllowedAttributeValues()) {
      AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
      BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue, "attribute",
          "valueType");
      if (attribute.isValueTypeAttribute() && CollectionUtils.isNotEmpty(request.getValueTypes())) {
        if (StringUtils.isNotBlank(allowedAttributeValueRequest.getValueType())) {
          ValidationUtil.checkParameter(
              request.getValueTypes().contains(allowedAttributeValueRequest.getValueType()),
              ErrorMessage.VALUE_TYPE_PROVIDED_IS_WRONG);
        }
        allowedAttributeValue.setValueType(allowedAttributeValueRequest.getValueType());
      }
      allowedAttributeValue.setAttribute(attribute);
      if (StringUtils.isEmpty(allowedAttributeValue.getStoreId())) {
        allowedAttributeValue.setStoreId(storeId);
      }
      attribute.getAllowedAttributeValues().add(allowedAttributeValue);
    }
    return attribute;
  }

  private static Attribute setPredefinedAllowedAttribute(Attribute attribute,
      MasterAttributeRequest request, String storeId) {
    for (PredefinedAllowedAttributeValueRequest predefinedAllowedAttributeValueRequest : request
        .getPredefinedAllowedAttributeValues()) {
      if (Constants.HYPHEN.equals(predefinedAllowedAttributeValueRequest.getValue())) {
        continue;
      }
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
      BeanUtils.copyProperties(predefinedAllowedAttributeValueRequest, predefinedAllowedAttributeValue, "attribute");
      predefinedAllowedAttributeValue.setAttribute(attribute);
      if (StringUtils.isEmpty(predefinedAllowedAttributeValue.getStoreId())) {
        predefinedAllowedAttributeValue.setStoreId(storeId);
      }
      attribute.getPredefinedAllowedAttributeValues().add(predefinedAllowedAttributeValue);
    }
    attribute.getPredefinedAllowedAttributeValues().add(createDefaultAttributeValue(attribute));
    return attribute;
  }

  private static PredefinedAllowedAttributeValue createDefaultAttributeValue(Attribute attribute) {
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue = new PredefinedAllowedAttributeValue();
    predefinedAllowedAttributeValue.setValue(Constants.HYPHEN);
    predefinedAllowedAttributeValue.setSequence(0);
    predefinedAllowedAttributeValue.setStoreId(attribute.getStoreId());
    predefinedAllowedAttributeValue.setAttribute(attribute);
    return predefinedAllowedAttributeValue;
  }

  public static Attribute convertMasterAttributeRequestToAttribute(MasterAttributeRequest request,
      String storeId) {
    Attribute attribute = new Attribute();
    BeanUtils.copyProperties(request, attribute, "allowedAttributeValues",
        "predefinedAllowedAttributeValues", Constants.ATTRIBUTE_TYPE, "sortType", Constants.VALUE_TYPES,
        "attributeImageUrl");
    if (StringUtils.isEmpty(attribute.getStoreId())) {
      attribute.setStoreId(storeId);
    }
    setAttributeTypeAndSortType(attribute, request);
    if(attribute.isSizeAttribute()) {
      ValidationUtil.checkParameter(
          AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()),
          ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_SIZE_ATTRIBUTE_ERROR_CODE.getMessage(),
          ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_SIZE_ATTRIBUTE.getMessage());
      attribute.setValueTypeAttribute(true);
      ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getAttributeImageUrl()),
          ErrorMessage.ATTRIBUTE_IMAGE_URL_MUST_NOT_BE_EMPTY);
      attribute.setAttributeImageUrl(request.getAttributeImageUrl());
    }
    validateDSExtractionTrue(attribute);
    validateMultiLanguage(attribute);
    if (AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType())) {
      setAllowedAttribute(attribute, request, storeId);
    }
    if (AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())
        || AttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType())) {
      setPredefinedAllowedAttribute(attribute, request, storeId);
    }
    return attribute;
  }

public static void validateDSExtractionTrue(Attribute attribute) {
  if (attribute.isDsExtraction()) {
    ValidationUtil.checkParameter(
        !AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()),
        ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_DS_EXTRACTION_ERROR_CODE.getMessage(),
        ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_DS_EXTRACTION.getMessage());
    ValidationUtil.checkParameter(StringUtils.isNotEmpty(attribute.getDsAttributeName()),
        ErrorMessage.DS_ATTRIBUTE_NAME_CANNOT_BE_EMPTY_ERROR_CODE.getMessage(),
        ErrorMessage.DS_ATTRIBUTE_NAME_CANNOT_BE_EMPTY.getMessage());
  }
}

public static void validateMultiLanguage(Attribute attribute) {
  if (attribute.isMultiLanguage()) {
    ValidationUtil.checkParameter(
        AttributeType.PREDEFINED_ATTRIBUTE.equals(attribute.getAttributeType())
            || AttributeType.PREDEFINED_MULTIVALUE.equals(attribute.getAttributeType()),
        ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_MULTI_LANGUAGE_ERROR_CODE.getMessage(),
        ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_MULTI_LANGUAGE);
  }
}

  public static Attribute convertMasterAttributeRequestToAttributeExcludingAttributeValues(
      MasterAttributeRequest request, String storeId) {
    Attribute attribute = new Attribute();
    BeanUtils.copyProperties(request, attribute, "allowedAttributeValues",
        "predefinedAllowedAttributeValues", "dimensionMapping", Constants.ATTRIBUTE_TYPE, "sortType",
        Constants.VALUE_TYPES);
    setAttributeTypeAndSortType(attribute, request);
    if(attribute.isSizeAttribute()) {
      ValidationUtil.checkParameter(
          AttributeType.DEFINING_ATTRIBUTE.equals(attribute.getAttributeType()),
          ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_SIZE_ATTRIBUTE_ERROR_CODE.getMessage(),
          ErrorMessage.ATTRIBUTE_TYPE_CANNOT_BE_SIZE_ATTRIBUTE.getMessage());
      ValidationUtil.checkParameter(StringUtils.isNotBlank(request.getAttributeImageUrl()),
          ErrorMessage.ATTRIBUTE_IMAGE_URL_MUST_NOT_BE_EMPTY.getMessage());
    }
    validateDSExtractionTrue(attribute);
    validateMultiLanguage(attribute);
    if (StringUtils.isEmpty(attribute.getStoreId())) {
      attribute.setStoreId(storeId);
    }
    return attribute;
  }

  public static MasterAttributeResponse convertAttributeRequestToMasterAttributeResponse(Attribute attribute)
    throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    MasterAttributeResponse masterAttributeResponse = new MasterAttributeResponse();
    BeanUtils.copyProperties(attribute, masterAttributeResponse, "allowedAttributeValues",
        "predefinedAllowedAttributeValues", Constants.ATTRIBUTE_TYPE, "sortType");
    if (Objects.nonNull(attribute.getAttributeType())) {
      masterAttributeResponse.setAttributeType(attribute.getAttributeType().toString());
    }
    if (Objects.nonNull(attribute.getSortType())) {
      masterAttributeResponse.setSortType(AttributeSortTypeRequest.valueOf(attribute.getSortType().toString()));
    }
    if (StringUtils.isNotBlank(attribute.getValueTypes())) {
      masterAttributeResponse.setValueTypes(
        mapper.readValue(attribute.getValueTypes(), new TypeReference<List<String>>() {
        }));
    }
    return masterAttributeResponse;
  }

  public static MasterAttributeUpdateDTO convertMasterAttributeUpdateRequestToMasterAttributeUpdateDTO(
      MasterAttributeUpdateRequest masterAttributeUpdateRequest){
    MasterAttributeUpdateDTO masterAttributeUpdateDTO = new MasterAttributeUpdateDTO();
    masterAttributeUpdateDTO.setUpdatedBy(masterAttributeUpdateRequest.getUpdatedBy());
    masterAttributeUpdateDTO.setUpdatedDate(masterAttributeUpdateRequest.getUpdatedDate());
    masterAttributeUpdateDTO.setSortType(
        AttributeSortType.valueOf(masterAttributeUpdateRequest.getSortType().toString()));
    for (AttributeValueUpdateRequest attributeValueUpdateRequest :
        masterAttributeUpdateRequest.getAttributeValues()) {
      AttributeValueUpdateDTO attributeValueUpdateDTO = new AttributeValueUpdateDTO();
      BeanUtils.copyProperties(attributeValueUpdateRequest, attributeValueUpdateDTO);
      masterAttributeUpdateDTO.getAttributeValues().add(attributeValueUpdateDTO);
    }
    for (AttributeValueUpdateRequest attributeValueUpdateRequest :
        masterAttributeUpdateRequest.getAddedAttributeValues()) {
      AttributeValueUpdateDTO attributeValueUpdateDTO = new AttributeValueUpdateDTO();
      BeanUtils.copyProperties(attributeValueUpdateRequest, attributeValueUpdateDTO);
      masterAttributeUpdateDTO.getAddedAttributeValues().add(attributeValueUpdateDTO);
    }
    for (AttributeValueUpdateRequest attributeValueUpdateRequest :
        masterAttributeUpdateRequest.getDeletedAttributeValues()) {
      AttributeValueUpdateDTO attributeValueUpdateDTO = new AttributeValueUpdateDTO();
      BeanUtils.copyProperties(attributeValueUpdateRequest, attributeValueUpdateDTO);
      masterAttributeUpdateDTO.getDeletedAttributeValues().add(attributeValueUpdateDTO);
    }
    return masterAttributeUpdateDTO;
  }

  public static AttributeValueUpdateDTO convertMasterAttributeAddRequestToAttributeValueUpdateDTO(
      MasterAttributeAddRequest masterAttributeAddRequest){
    AttributeValueUpdateDTO attributeValueUpdateDTO = new AttributeValueUpdateDTO();
    attributeValueUpdateDTO.setValue(masterAttributeAddRequest.getValue().trim());
    attributeValueUpdateDTO.setSequence(masterAttributeAddRequest.getSequence());
    return attributeValueUpdateDTO;
  }

  public static List<MasterAttributeResponse> toMasterAttributeResponsesFromAttributes(
      List<Attribute> attributeResponses) throws Exception {
    List<MasterAttributeResponse> masterAttributeResponses =
        new ArrayList<MasterAttributeResponse>();
    if (CollectionUtils.isNotEmpty(attributeResponses)) {
      for (Attribute attribute : attributeResponses) {
        masterAttributeResponses.add(toMasterAttributeResponseFromAttribute(attribute));
      }
    }
    return masterAttributeResponses;
  }

  public static MasterAttributeResponse toMasterAttributeResponseFromAttribute(
      Attribute attribute) throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    MasterAttributeResponse response = new MasterAttributeResponse();
    BeanUtils.copyProperties(attribute, response, "allowedAttributeValues",
        "predefinedAllowedAttributeValues", "categoryAttributes", Constants.ATTRIBUTE_TYPE, "sortType");
    if (Objects.nonNull(attribute.getAttributeType())) {
      response.setAttributeType(attribute.getAttributeType().name());
    }
    if (Objects.nonNull(attribute.getSortType())) {
      response.setSortType(AttributeSortTypeRequest.valueOf(attribute.getSortType().name()));
    }
    if (StringUtils.isNotBlank(attribute.getValueTypes())) {
      response.setValueTypes(
        mapper.readValue(attribute.getValueTypes(), new TypeReference<List<String>>() {
        }));
    }
    return response;
  }

  public static CategoryInfoUpdateDTO toCategoryInfoUpdateDTO(
      CategoryInfoUpdateRequest categoryInfoUpdateRequest){
    CategoryInfoUpdateDTO categoryInfoUpdateDTO = new CategoryInfoUpdateDTO();
    BeanUtils.copyProperties(categoryInfoUpdateRequest, categoryInfoUpdateDTO);
    return categoryInfoUpdateDTO;
  }

  public static CategoryMappingsUpdateDTO toCategoryMappingsUpdateDTO(
      CategoryMappingsUpdateRequest categoryMappingsUpdateRequest) {
    CategoryMappingsUpdateDTO categoryMappingsUpdateDTO = new CategoryMappingsUpdateDTO();
    BeanUtils.copyProperties(categoryMappingsUpdateRequest, categoryMappingsUpdateDTO);
    categoryMappingsUpdateDTO.setAddedAttributes(
        categoryMappingsUpdateRequest.getAddedAttributes().stream()
            .map(ConverterUtil::toCategoryAttributeUpdateDTO).collect(Collectors.toList()));
    categoryMappingsUpdateDTO.setDeletedAttributes(
        categoryMappingsUpdateRequest.getDeletedAttributes().stream()
            .map(ConverterUtil::toCategoryAttributeUpdateDTO).collect(Collectors.toList()));
    categoryMappingsUpdateDTO.setAddedKeywords(
        categoryMappingsUpdateRequest.getAddedKeywords().stream().map(ConverterUtil::toCategoryKeywordsUpdateDTO)
            .collect(Collectors.toList()));
    categoryMappingsUpdateDTO.setDeletedKeywords(
        categoryMappingsUpdateRequest.getDeletedKeywords().stream().map(ConverterUtil::toCategoryKeywordsUpdateDTO)
            .collect(Collectors.toList()));
    categoryMappingsUpdateDTO.setAddedMasterCategoryIds(
        categoryMappingsUpdateRequest.getAddedMasterCategoryIds());
    categoryMappingsUpdateDTO.setDeletedMasterCategoryIds(
        categoryMappingsUpdateRequest.getDeletedMasterCategoryIds());
    WholesaleMappingDTO wholesaleMappingDTO = new WholesaleMappingDTO();
    if(Objects.nonNull(categoryMappingsUpdateRequest.getWholesaleMapping())){
      BeanUtils.copyProperties(categoryMappingsUpdateRequest.getWholesaleMapping(), wholesaleMappingDTO);
      wholesaleMappingDTO.setConfigurationType(StringUtils.upperCase(categoryMappingsUpdateRequest
          .getWholesaleMapping().getConfigurationType()));
    }
    categoryMappingsUpdateDTO.setWholesaleMapping(wholesaleMappingDTO);
    return categoryMappingsUpdateDTO;
  }

  private static CategoryAttributeUpdateDTO toCategoryAttributeUpdateDTO(
      CategoryAttributeUpdateRequest categoryAttributeUpdateRequest) {
    CategoryAttributeUpdateDTO categoryAttributeUpdateDTO = new CategoryAttributeUpdateDTO();
    BeanUtils.copyProperties(categoryAttributeUpdateRequest, categoryAttributeUpdateDTO);
    return categoryAttributeUpdateDTO;
  }

  public static CategoryDetailDTO toCategoryDetailDTO(CategoryDetailRequest categoryDetailRequest) {
    CategoryDetailDTO categoryDetailDTO =
        CategoryDetailDTO.builder().catalogId(categoryDetailRequest.getCatalogId())
            .categoryInfoDetail(
                toCategoryInfoUpdateDTO(categoryDetailRequest.getCategoryInfoDetail()))
            .categoryMappingsDetail(
                toCategoryMappingsUpdateDTO(categoryDetailRequest.getCategoryMappingsDetail()))
            .build();
    BeanUtils.copyProperties(categoryDetailRequest, categoryDetailDTO);
    return categoryDetailDTO;
  }

  public static List<CategoryDTO> toCategoryDTOList(List<CategoryServiceDTO> categoryServiceDTOS) {
    List<CategoryDTO> categoryDTOS = new ArrayList<>();
    categoryServiceDTOS.forEach(categoryServiceDTO -> categoryDTOS.add(toCategoryDTO(categoryServiceDTO)));
    return categoryDTOS;
  }

  private static CategoryDTO toCategoryDTO(CategoryServiceDTO categoryServiceDTO) {
    CategoryDTO categoryDTO = new CategoryDTO();
    BeanUtils.copyProperties(categoryServiceDTO.getCategory(), categoryDTO);
    CatalogResponse catalogResponse = new CatalogResponse();
    catalogResponse.setCatalogType(categoryServiceDTO.getCategory().getCatalog().getCatalogType().name());
    BeanUtils.copyProperties(categoryServiceDTO.getCategory().getCatalog(), catalogResponse);
    categoryDTO.setChildCount(categoryServiceDTO.getChildCount());
    if (Objects.nonNull(categoryServiceDTO.getCategory().getParentCategory())) {
      categoryDTO.setParentCategoryId(categoryServiceDTO.getCategory().getParentCategory().getId());
    }
    categoryDTO.setCatalog(catalogResponse);
    return categoryDTO;
  }

  public static BrandWip generateCreateBrandWipRequestToBrandWip(CreateBrandWipRequest request, String businessPartnerCode,
      String businessPartnerName) {
    BrandWip brandWip = new BrandWip();
    BeanUtils.copyProperties(request, brandWip, "description", "brandName");
    brandWip.setBrandName(StringEscapeUtils.unescapeHtml3(request.getBrandName()));
    brandWip.setBrandDescription(request.getBrandDescription().getBytes());
    brandWip.setBusinessPartnerCode(businessPartnerCode);
    brandWip.setBusinessPartnerName(businessPartnerName);
    brandWip.setProtectedBrand(request.isProtectedBrand());
    brandWip.setSkuCreationAllowedForAllSellers(request.isSkuCreationAllowedForAllSellers());
    return brandWip;
  }

  public static CreateBrandWipResponse generateCreateBrandWipResponse(String brandCode) {
    CreateBrandWipResponse createBrandResponse = new CreateBrandWipResponse();
    createBrandResponse.setBrandRequestCode(brandCode);
    return createBrandResponse;
  }

  public static void generateProductItemsFromRequest(Product product, ProductRequest request) {
    if (CollectionUtils.isNotEmpty(request.getProductItems())) {
      for (ProductItemRequest productItemRequest : request.getProductItems()) {
        ProductItem productItem = new ProductItem();
        productItem.setGeneratedItemName(productItemRequest.getGeneratedItemName());
        if (CollectionUtils.isNotEmpty(productItemRequest.getProductItemAttributeValues())) {
          productItem.getProductItemAttributeValues()
              .add(ConverterUtil.convertToProductItemAttributeValue(
                  product.getStoreId(), productItemRequest.getProductItemAttributeValues().get(0)));
        }
        product.getProductItems().add(productItem);
      }
    }
  }

  public static ProductItem convertRequestToProductItems(String storeId,
      ProductItemRequest productItemRequest, boolean addAllAttributes, boolean creation) {
    ProductItem productItem = new ProductItem();
    BeanUtils.copyProperties(productItemRequest, productItem, "productItemAttributeValues", "images");
    productItem.setMarkForDelete(productItemRequest.isMarkForDelete());
    if(StringUtils.isEmpty(productItem.getStoreId())) {
      productItem.setStoreId(storeId);
    }
    if (!CollectionUtils.isEmpty(productItemRequest.getProductItemAttributeValues())) {
        for (ProductItemAttributeValueRequest request : productItemRequest.getProductItemAttributeValues()) {
          if (addAllAttributes || (request.getAttribute().isScreeningMandatory() && request.getAttribute()
              .isVariantCreatingUI())) {
            productItem.getProductItemAttributeValues().add(convertToProductItemAttributeValue(storeId, request));
        }
      }
    }
    if (CollectionUtils.isNotEmpty(productItemRequest.getImages())) {
      Collections.sort(productItemRequest.getImages(), Comparator.comparing(Image::isMainImages).reversed());
      List<String> uniqueImages = new ArrayList<>();
      productItem.setProductItemImages(productItemRequest.getImages().stream()
          .filter(image -> !uniqueImages.contains(image.getLocationPath()))
          .map(productItemImageRequest -> ConverterUtil.convertToProductItemImage(
              storeId, productItemImageRequest, uniqueImages, creation))
          .collect(Collectors.toList()));
    }
    return productItem;
  }

  private static ProductItemAttributeValue convertToProductItemAttributeValue(
      String storeId, ProductItemAttributeValueRequest request){
    ProductItemAttributeValue attributeValue = new ProductItemAttributeValue();
    BeanUtils.copyProperties(request, attributeValue);
    Attribute attribute = new Attribute();
    BeanUtils.copyProperties(request.getAttribute(), attribute);
    attributeValue.setAttribute(attribute);
    if(StringUtils.isEmpty(attributeValue.getStoreId())) {
      attributeValue.setStoreId(storeId);
    }
    return attributeValue;
  }

  private static ProductItemImage convertToProductItemImage(
      String storeId, Image image, List<String> uniqueImages, boolean creation) {
    ProductItemImage productItemImage = new ProductItemImage();
    if (creation) {
      BeanUtils.copyProperties(image, productItemImage, Constants.ID);
    } else {
      BeanUtils.copyProperties(image, productItemImage);
    }
    if (StringUtils.isEmpty(productItemImage.getStoreId())) {
      productItemImage.setStoreId(storeId);
    }
    uniqueImages.add(productItemImage.getLocationPath());
    return productItemImage;
  }

  public static Image updateImageLocation(Image image, String productCode, String imageSourceDirectory,
      String fullImageSourceDirectory, Set<String> uploadedImageLocations, AtomicInteger imageUploadCounter) throws Exception {
    if (StringUtils.isBlank(image.getLocationPath())) {
      log.error("Product creation failed for product because of empty image location path. Product Code : {} ",
          productCode);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.EMPTY_IMAGE_LOCATION_PATH.getMessage());
    }
    return fileStorageService.updateLocationForImages(image, productCode, imageSourceDirectory,
        fullImageSourceDirectory, uploadedImageLocations, imageUploadCounter);
  }

  public static ProductResponse createBasicProductResponse(Product product){
    ProductResponse productResponse = new ProductResponse();
    productResponse.setId(product.getId());
    productResponse.setVersion(product.getVersion());
    productResponse.setProductCode(product.getProductCode());
    productResponse.setActivated(product.isActivated());
    productResponse.setViewable(product.isViewable());
    productResponse.setUpdatedBy(product.getUpdatedBy());
    productResponse.setReviewPending(product.isReviewPending());
    productResponse.setDescription(product.getDescription());
    return productResponse;
  }

  //Reusing the other converter util to prevent code duplications and multiple changes in future
  public static List<ProductItemImage> filterProductItemImages(boolean originalImages,
      List<ProductItemImage> productItemImages) {
    if (!originalImages) {
      productItemImages =
          productItemImages.stream().filter(com.gdn.x.productcategorybase.util.ConverterUtil::filterProcessedItemImages)
              .collect(Collectors.toList());
    }
    return productItemImages;
  }

  public static ProductDetailResponse setCategoryNamesProductDetailResponse(List<Category> categoryList,
      ProductDetailResponse productDetailResponse) {
    List<String> categoryNames =
        Optional.ofNullable(categoryList).orElse(new ArrayList<>()).stream().map(category -> category.getName())
            .collect(Collectors.toList());
    List<String> categoryNamesEnglish = Optional.ofNullable(categoryList).orElse(new ArrayList<>()).stream()
        .map(category -> category.getNameEnglish()).collect(Collectors.toList());
    List<String> categoryCodes = Optional.ofNullable(categoryList).orElse(new ArrayList<>()).stream()
        .map(Category::getCategoryCode).collect(Collectors.toList());
    Collections.reverse(categoryNames);
    Collections.reverse(categoryNamesEnglish);
    Collections.reverse(categoryCodes);
    productDetailResponse.setCategories(categoryNames);
    productDetailResponse.setCategoriesEnglish(categoryNamesEnglish);
    productDetailResponse.setCategoryCodes(categoryCodes);
    return productDetailResponse;
  }

  public static CategoryKeywordsUpdateListDTO toCategoryKeywordsUpdateListDTO(
      CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList, String storeId) {
    CategoryKeywordsUpdateListDTO categoryKeywordsUpdateListDTO = new CategoryKeywordsUpdateListDTO();
    BeanUtils.copyProperties(categoryKeywordUpdateRequestList, categoryKeywordsUpdateListDTO,"storeId");
    categoryKeywordsUpdateListDTO.setAddedRestrictedKeywords(
        categoryKeywordUpdateRequestList.getAddedKeywords().stream().map(ConverterUtil::toCategoryKeywordsUpdateDTO)
            .collect(Collectors.toList()));
    categoryKeywordsUpdateListDTO.setDeletedRestrictedKeywords(
        categoryKeywordUpdateRequestList.getDeletedKeywords().stream().map(ConverterUtil::toCategoryKeywordsUpdateDTO)
            .collect(Collectors.toList()));
    categoryKeywordsUpdateListDTO.setStoreId(storeId);
    return categoryKeywordsUpdateListDTO;
  }

  private static CategoryKeywordsUpdateDTO toCategoryKeywordsUpdateDTO(
      CategoryKeywordsUpdateRequest categoryKeywordsUpdateRequest) {
    CategoryKeywordsUpdateDTO categoryKeywordsUpdateDTO = new CategoryKeywordsUpdateDTO();
    BeanUtils.copyProperties(categoryKeywordsUpdateRequest, categoryKeywordsUpdateDTO);
    categoryKeywordsUpdateDTO.setExclusionList(categoryKeywordsUpdateDTO.getExclusionList());
    return categoryKeywordsUpdateDTO;
  }

  public static RestrictedKeywordsUpdateDTO toRestrictedKeywordsUpdateDTO(
          RestrictedKeywordsUpdateRequest restrictedKeywordsUpdateRequest) {
    RestrictedKeywordsUpdateDTO restrictedKeywordsUpdateDTO = new RestrictedKeywordsUpdateDTO();
    BeanUtils.copyProperties(restrictedKeywordsUpdateRequest, restrictedKeywordsUpdateDTO);
    return restrictedKeywordsUpdateDTO;
  }

  public static WholesaleMappingDTO toCategoryWholesaleDTO(WholesaleMappingRequest wholesaleMapping) {
    WholesaleMappingDTO wholesaleMappingDTO = new WholesaleMappingDTO();
    BeanUtils.copyProperties(wholesaleMapping, wholesaleMappingDTO);
    wholesaleMappingDTO.setConfigurationType(StringUtils.upperCase(wholesaleMapping.getConfigurationType()));
    return wholesaleMappingDTO;
  }

  public static List<LookupResponse> toLookupResponseList(List<Lookup> lookupList) {
    List<LookupResponse> lookupResponseList = new ArrayList<>();
    lookupList.forEach(lookup -> lookupResponseList.add(toLookupResponse(lookup)));
    return lookupResponseList;
  }

  private static LookupResponse toLookupResponse(Lookup lookup) {
    LookupResponse lookupResponse = new LookupResponse();
    BeanUtils.copyProperties(lookup, lookupResponse);
    return lookupResponse;
  }

  public static void convertToProductItemResponse(List<ProductItemResponse> productItemResponses,
      List<ProductItem> productItems) {
    for (ProductItem productItem : productItems) {
      ProductItemResponse response = new ProductItemResponse();
      BeanUtils.copyProperties(productItem, response, "product", "productItemAttributeValues", "productItemImages");
      List<ProductItemAttributeValueResponse> productItemAttributeValueResponses = new ArrayList<>();
      for (ProductItemAttributeValue productItemAttributeValue : productItem.getProductItemAttributeValues()) {
        ProductItemAttributeValueResponse productItemAttributeValueResponse = new ProductItemAttributeValueResponse();
        BeanUtils.copyProperties(productItemAttributeValue, productItemAttributeValueResponse, "attribute");
        AttributeResponse attributeResponse = new AttributeResponse();
        BeanUtils.copyProperties(productItemAttributeValue.getAttribute(), attributeResponse, "allowedAttributeValues",
            "predefinedAllowedAttributeValues");
        productItemAttributeValueResponse.setAttributeResponse(attributeResponse);
        productItemAttributeValueResponses.add(productItemAttributeValueResponse);
      }
      response.setProductItemAttributeValueResponses(productItemAttributeValueResponses);
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        response.getImages().add(convertProductItemImageToResponse(productItemImage));
      }
      productItemResponses.add(response);
    }
  }

  public static void convertProductItemRequestToProductItem(List<ProductItemRequest> request,
      List<ProductItem> productItems) {
    if (CollectionUtils.isNotEmpty(request)) {
      for (ProductItemRequest productItemRequest : request) {
        ProductItem productItem = new ProductItem();
        BeanUtils.copyProperties(productItemRequest, productItem, "productItemAttributeValues");
        if (CollectionUtils.isNotEmpty(productItemRequest.getProductItemAttributeValues())) {
          List<ProductItemAttributeValue> productItemAttributeValues = new ArrayList<>();
          for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemRequest
              .getProductItemAttributeValues()) {
            ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
            BeanUtils.copyProperties(productItemAttributeValueRequest, productItemAttributeValue, "attribute");
            Attribute attribute = new Attribute();
            BeanUtils
                .copyProperties(productItemAttributeValueRequest.getAttribute(), attribute, "allowedAttributeValues");
            List<AllowedAttributeValue> allowedAttributeValues = new ArrayList<AllowedAttributeValue>();
            if (CollectionUtils
                .isNotEmpty(productItemAttributeValueRequest.getAttribute().getAllowedAttributeValues())) {
              for (AllowedAttributeValueRequest allowedAttributeValueRequest : productItemAttributeValueRequest
                  .getAttribute().getAllowedAttributeValues()) {
                AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
                BeanUtils.copyProperties(allowedAttributeValueRequest, allowedAttributeValue);
                allowedAttributeValues.add(allowedAttributeValue);
              }
            }
            attribute.setAllowedAttributeValues(allowedAttributeValues);
            productItemAttributeValue.setAttribute(attribute);
            productItemAttributeValues.add(productItemAttributeValue);
          }
          productItem.setProductItemAttributeValues(productItemAttributeValues);
        }
        for (Image image : productItemRequest.getImages()) {
          ProductItemImage productItemImage = new ProductItemImage();
          BeanUtils.copyProperties(image, productItemImage);
          productItem.getProductItemImages().add(productItemImage);
        }
        productItems.add(productItem);
      }
    }
  }

  public static ProductDetailResponse convertProductToProductDetailResponseForImages(Product product) {
    ProductDetailResponse response = new ProductDetailResponse();
    BeanUtils.copyProperties(product, response);
    response.setLongDescription(product.getDescription());
    Set<Image> productImageResponses = new HashSet<>();
    for (ProductItem productItem : product.getProductItems()) {
      List<ProductItemImage> nonActiveImages =
          productItem.getProductItemImages().stream().filter(productItemImage -> !productItemImage.isMarkForDelete())
              .collect(Collectors.toList());
      List<ProductItemImage> productItemImages =
          nonActiveImages.stream().filter(productItemImage -> filterProcessedItemImagesForRevision(productItemImage))
              .collect(Collectors.toList());
      nonActiveImages.removeAll(productItemImages);
      for (ProductItemImage productItemImage : nonActiveImages) {
        productImageResponses.add(convertProductItemImageToResponse(productItemImage));
      }
    }
    response.setImages(new ArrayList<>(productImageResponses));
    return response;
  }

  public static boolean filterProcessedItemImagesForRevision(ProductItemImage productItemImage) {
    if (productItemImage.isEdited() || productItemImage.isActive()) {
      return true;
    }
    if (Objects.isNull(productItemImage.getOriginalImage())) {
      return true;
    } else {
      return productItemImage.getOriginalImage();
    }
  }

  public static void setProductItemDetails(ProductRequest productRequest, Product product,
      Map<String, Attribute> attributeMap) {
    for (ProductItemRequest productItemRequest : productRequest.getProductItems()) {
      ProductItem productItem = new ProductItem();
      BeanUtils.copyProperties(productItemRequest, productItem, new String[] {"productItemAttributeValues",
          "productItemImages"});
      productItem.setStoreId(GdnMandatoryParameterUtil.getStoreId());
      productItem.setProduct(product);
      List<Image> source;
      if (CollectionUtils.isEmpty(productItemRequest.getImages())) {
        source = productRequest.getImages();
      } else {
        source = productItemRequest.getImages();
      }
      for (Image image : source) {
        ProductItemImage productItemImage = new ProductItemImage();
        BeanUtils.copyProperties(image, productItemImage);
        productItemImage.setStoreId(GdnMandatoryParameterUtil.getStoreId());
        productItemImage.setProductItem(productItem);
        productItem.getProductItemImages().add(productItemImage);
      }
      productItem.setProductItemAttributeValues(new ArrayList<>());
      if (CollectionUtils.isNotEmpty(productItemRequest.getProductItemAttributeValues())) {
        for (ProductItemAttributeValueRequest productItemAttributeValueRequest : productItemRequest
            .getProductItemAttributeValues()) {
          ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
          productItemAttributeValue.setProductItem(productItem);
          productItemAttributeValue.setValue(productItemAttributeValueRequest.getValue());
          productItemAttributeValue
              .setAttribute(attributeMap.get(productItemAttributeValueRequest.getAttribute().getAttributeCode()));
          productItem.getProductItemAttributeValues().add(productItemAttributeValue);
        }
      }
      product.getProductItems().add(productItem);
    }
  }

  public static void setProductDetailsFromProductRequest(String storeId, Product product, ProductRequest request,
      Map<AttributeAndValueByTypeRequest, Object> attributeAndAttributeValues,
      boolean productSuitabilityFeatureEnabled, boolean creation) {
    List<ProductAttributeRequest> productAttributes = request.getProductAttributes();
    if (productSuitabilityFeatureEnabled) {
      productAttributes = productAttributes.stream()
          .filter(productAttributeRequest -> !productAttributeRequest.getAttribute().isHideForSeller())
          .collect(Collectors.toList());
    }
    for (ProductAttributeRequest productAttributeRequest : productAttributes) {
      product.getProductAttributes().add(
          convertRequestToProductAttribute(storeId, product, productAttributeRequest,
              attributeAndAttributeValues, creation));
    }
    int pCounter = 0;
    for (Image productImageRequest : request.getImages()) {
      product.getProductImages()
          .add(ConverterUtil.convertRequestToProductImage(storeId, product, productImageRequest));
      if (productImageRequest.isMainImages()) {
        pCounter++;
      }
    }
    if (pCounter == 0) {
      for (ProductImage image : product.getProductImages()) {
        if (Objects.nonNull(image.getSequence()) && image.getSequence() == 0 && !image.isMainImages()) {
          image.setMainImages(true);
        }
      }
    }
  }

  private static ProductAttribute convertRequestToProductAttribute(String storeId, Product product,
      ProductAttributeRequest request,
      Map<AttributeAndValueByTypeRequest, Object> attributesAndValuesMap, boolean creation) {
    ProductAttribute productAttribute = new ProductAttribute();
    BeanUtils.copyProperties(request, productAttribute, "attribute", "productAttributeValues");
    productAttribute.setAttribute((Attribute) attributesAndValuesMap.get(
        new AttributeAndValueByTypeRequest(request.getAttribute().getId(),
            AttributeAndValueByTypeRequest.type.ATTRIBUTE)));
    productAttribute.setProduct(product);
    for (ProductAttributeValueRequest productAttributeValueRequest : request
        .getProductAttributeValues()) {
      ProductAttributeValue productAttributeValue = new ProductAttributeValue();
      if (creation) {
        BeanUtils.copyProperties(productAttributeValueRequest, productAttributeValue, Constants.ID);
      } else {
        BeanUtils.copyProperties(productAttributeValueRequest, productAttributeValue);
      }
      productAttributeValue.setDescriptiveAttributeValueType(com.gdn.x.productcategorybase.DescriptiveAttributeValueType
          .valueOf(productAttributeValueRequest.getDescriptiveAttributeValueType().toString()));

      if (productAttributeValueRequest.getDescriptiveAttributeValueType().toString()
          .equals(com.gdn.x.productcategorybase.DescriptiveAttributeValueType.NONE.toString())) {
        AllowedAttributeValue allowedAttributeValue = (AllowedAttributeValue) attributesAndValuesMap.get(
            new AttributeAndValueByTypeRequest(productAttributeValueRequest.getAllowedAttributeValue().getId(),
                AttributeAndValueByTypeRequest.type.ALLOWED_ATTRIBUTE_VALUE));
        productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
      }
      else {
        String id = null;
        if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())) {
          id = productAttributeValueRequest.getPredefinedAllowedAttributeValue().getId();
        }

        if (Objects.nonNull(id)) {
          PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
              (PredefinedAllowedAttributeValue) attributesAndValuesMap.get(new AttributeAndValueByTypeRequest(
                  productAttributeValueRequest.getPredefinedAllowedAttributeValue().getId(),
                  AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_ID));
          productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
        }
        else {
          if (Objects.nonNull(productAttributeValueRequest.getPredefinedAllowedAttributeValue())
              && StringUtils.isNotEmpty(
              productAttributeValueRequest.getPredefinedAllowedAttributeValue().getPredefinedAllowedAttributeCode())) {
            PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
                (PredefinedAllowedAttributeValue) attributesAndValuesMap.get(new AttributeAndValueByTypeRequest(
                    productAttributeValueRequest.getPredefinedAllowedAttributeValue()
                        .getPredefinedAllowedAttributeCode(),
                    AttributeAndValueByTypeRequest.type.PREDEFINED_ATTRIBUTE_VALUE_BY_CODE));
            productAttributeValue.setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
          } else {
            productAttributeValue
                .setDescriptiveAttributeValue(productAttributeValueRequest.getDescriptiveAttributeValue());
          }
        }
      }
      productAttributeValue.setProductAttribute(productAttribute);
      if (StringUtils.isEmpty(productAttributeValue.getStoreId())) {
        productAttributeValue.setStoreId(storeId);
      }
      productAttribute.getProductAttributeValues().add(productAttributeValue);
    }
    if (StringUtils.isEmpty(productAttribute.getStoreId())) {
      productAttribute.setStoreId(storeId);
    }
    return productAttribute;
  }

  public static ProductAndAttributeDetailResponse convertProductToProductAndAttributeDetailResponse(Product product) {
    ProductAndAttributeDetailResponse response = new ProductAndAttributeDetailResponse();
    BeanUtils.copyProperties(product, response);
    List<ProductAttributeResponse> productAttributeResponses =
        getProductAttributeResponses(product.getProductAttributes());
    response.setProductAttributeResponses(productAttributeResponses);
    return response;
  }

  public static List<ProductAttributeResponse> getProductAttributeResponses(List<ProductAttribute> productAttributes) {
    List<ProductAttributeResponse> productAttributeResponses = new ArrayList<>();
    for (ProductAttribute productAttribute : productAttributes) {
      productAttributeResponses.add(convertProductAttributeToResponse(productAttribute));
    }
    return productAttributeResponses;
  }

  public static ProductBrandUpdateDTO getProductBrandUpdateDTO(ProductBrandUpdateRequest productBrandUpdateRequest) {
    ProductBrandUpdateDTO productBrandUpdateDTO = new ProductBrandUpdateDTO();
    BeanUtils.copyProperties(productBrandUpdateRequest, productBrandUpdateDTO);
    return productBrandUpdateDTO;
  }

  public static ProductBrandUpdateResponse getProductBrandUpdateResponse(
      ProductBrandUpdateResponseDTO productBrandUpdateResponseDTO) {
    ProductBrandUpdateResponse productBrandUpdateResponse = new ProductBrandUpdateResponse();
    BeanUtils.copyProperties(productBrandUpdateResponseDTO, productBrandUpdateResponse);
    return productBrandUpdateResponse;
  }


  private static Map<String, String> removeValueTypeFromAttributeMap(Map<String, String> attributeMap,
      String sizeChartValueTypeDelimiter) {
    if (MapUtils.isNotEmpty(attributeMap)) {
      for (Map.Entry<String, String> attributeValueTypeEntry : attributeMap.entrySet()) {
        attributeValueTypeEntry.setValue(
            splitAndReturnAttributeValue(attributeValueTypeEntry.getValue(), sizeChartValueTypeDelimiter));
      }
    }
    return attributeMap;
  }

  private static String splitAndReturnAttributeValue(String attributeValue, String sizeChartValueTypeDelimiter) {
    String[] tokens = attributeValue.split(sizeChartValueTypeDelimiter);
    if (tokens.length == Constants.TWO) {
      return tokens[1];
    } else {
      return attributeValue;
    }
  }

  public static void setAttributeDetailResponse(boolean allowedAttributeValuesTrim, Attribute attribute,
      AttributeResponse response, boolean valueTypeAdditionForDefiningAttributes, boolean concatenateValueWithValueType,
      String sizeChartValueTypeDelimiter, boolean sortValues) {
    BeanUtils.copyProperties(attribute, response, "allowedAttributeValues", "predefinedAllowedAttributeValues");
    response.setAttributeType(attribute.getAttributeType().toString());
    List<AllowedAttributeValueResponse> allowedAttributeValueResponses = new ArrayList<>();
    for (AllowedAttributeValue allowedAttributeValue : attribute.getAllowedAttributeValues()) {
      if (!allowedAttributeValue.isMarkForDelete()) {
        AllowedAttributeValueResponse allowedAttributeValueResponse = new AllowedAttributeValueResponse();
        if (allowedAttributeValuesTrim) {
          BeanUtils.copyProperties(allowedAttributeValue, allowedAttributeValueResponse, "createdBy", "createdDate",
              "version", "storeId", "updatedDate", "updatedBy");
        } else {
          BeanUtils.copyProperties(allowedAttributeValue, allowedAttributeValueResponse);
        }
        if (valueTypeAdditionForDefiningAttributes && concatenateValueWithValueType && StringUtils.isNotBlank(
            allowedAttributeValueResponse.getValue()) && StringUtils.isNotBlank(
            allowedAttributeValueResponse.getValueType())) {
          allowedAttributeValueResponse.setValue(
              allowedAttributeValueResponse.getValueType().concat(sizeChartValueTypeDelimiter)
                  .concat(allowedAttributeValueResponse.getValue()));
        }
        allowedAttributeValueResponses.add(allowedAttributeValueResponse);
      }
    }
    if (sortValues) {
      allowedAttributeValueResponses = allowedAttributeValueResponses.stream()
          .sorted(Comparator.nullsLast(Comparator.comparingInt(AllowedAttributeValueResponse::getSequence)))
          .collect(Collectors.toList());
    }
    response.setAllowedAttributeValues(allowedAttributeValueResponses);

    List<PredefinedAllowedAttributeValueResponse> predefinedAllowedAttributeValueResponses = new ArrayList<>();
    for (PredefinedAllowedAttributeValue predefinedAllowedAttributeValue : attribute.getPredefinedAllowedAttributeValues()) {
      if (!predefinedAllowedAttributeValue.isMarkForDelete()) {
        PredefinedAllowedAttributeValueResponse predefinedAllowedAttributeValueResponse =
            new PredefinedAllowedAttributeValueResponse();
        BeanUtils.copyProperties(predefinedAllowedAttributeValue, predefinedAllowedAttributeValueResponse);
        predefinedAllowedAttributeValueResponses.add(predefinedAllowedAttributeValueResponse);
      }
    }
    response.setPredefinedAllowedAttributeValues(predefinedAllowedAttributeValueResponses);
  }


  public static void setVideoDTO(Product product, ProductDetailResponse productDetailResponse) {
    if (StringUtils.isNotEmpty(product.getVideo())) {
      ObjectMapper mapper = new ObjectMapper();
      try {
        productDetailResponse.setVideoDTO(mapper.readValue(product.getVideo(), VideoDTO.class));
      } catch (Exception e) {
        log.error("Error while converting video add edit request to object for product Code : {} ",
          product.getProductCode(), e);
      }
    }
  }

  public static String convertVideoAddEditRequestToDTO(VideoAddEditRequest videoAddEditRequest) {
    try {
      ObjectMapper mapper = new ObjectMapper();
      VideoDTO videoDTO = new VideoDTO();
      BeanUtils.copyProperties(videoAddEditRequest, videoDTO);
      videoDTO.setSourceUrl(videoAddEditRequest.getVideoUrl());
      return mapper.writeValueAsString(videoDTO);
    } catch (Exception e) {
      log.error("Failed to convert VideoAddEditRequest {} to JSON DTO", videoAddEditRequest);
      return StringUtils.EMPTY;
    }
  }
}
