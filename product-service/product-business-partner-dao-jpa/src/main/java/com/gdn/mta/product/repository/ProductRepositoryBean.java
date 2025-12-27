package com.gdn.mta.product.repository;


import com.esotericsoftware.minlog.Log;
import com.gda.mta.product.dto.EditProductResponse;
import com.gda.mta.product.dto.ProductDetailEditDTO;
import com.gda.mta.product.dto.ProductItemDistributionInfoRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.request.GdnRestListRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.product.entity.ProductItemLevel3;
import com.gdn.mta.product.entity.ProductLevel3Attribute;
import com.gdn.mta.product.util.GdnBaseLookup;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.product.valueobject.SimpleMasterProductUpdateRequestDTO;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.helper.RequestHelper;
import com.gdn.partners.pbp.outbound.product.ProductOutbound;
import com.gdn.partners.pbp.outbound.product.feign.PCBFeign;
import com.gdn.x.product.rest.web.model.response.ProductL3Response;
import com.gdn.x.productcategorybase.AttributeType;
import com.gdn.x.productcategorybase.DescriptiveAttributeValueType;
import com.gdn.x.productcategorybase.dto.ActivateImageRequest;
import com.gdn.x.productcategorybase.dto.ActivateImageResponse;
import com.gdn.x.productcategorybase.dto.CatalogType;
import com.gdn.x.productcategorybase.dto.CategorySummaryResponse;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusResponse;
import com.gdn.x.productcategorybase.dto.Image;
import com.gdn.x.productcategorybase.dto.ProductActivateImageRequest;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.AddProductAttributesRequest;
import com.gdn.x.productcategorybase.dto.request.AttributeRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryCodeAndKeywordListRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryRequest;
import com.gdn.x.productcategorybase.dto.request.EditProductDetailRequest;
import com.gdn.x.productcategorybase.dto.request.ProductAttributeRequest;
import com.gdn.x.productcategorybase.dto.request.ProductBrandUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCategoryRequest;
import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemAttributeValueRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemRequest;
import com.gdn.x.productcategorybase.dto.request.ProductItemUomInfoDTO;
import com.gdn.x.productcategorybase.dto.request.ProductRequest;
import com.gdn.x.productcategorybase.dto.request.ReplaceProductImagesRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.SimpleStringListRequest;
import com.gdn.x.productcategorybase.dto.response.AttributeHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CatalogResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryRestrictedKeywordResponse;
import com.gdn.x.productcategorybase.dto.response.EditProductItemAndImageResponse;
import com.gdn.x.productcategorybase.dto.response.GeneratedProductImagesPathResponse;
import com.gdn.x.productcategorybase.dto.response.MapResponse;
import com.gdn.x.productcategorybase.dto.response.NewlySavedItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAndItemImageRequest;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductBrandUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCategoryResponse;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import com.gdn.x.productcategorybase.dto.response.ProductDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemAttributeValueResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProductItemResponse;
import com.gdn.x.productcategorybase.dto.response.ProductPredictionCategoryMappingResponse;
import com.gdn.x.productcategorybase.dto.response.ProductResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleBooleanResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import com.gdn.x.productcategorybase.dto.response.SimpleStringMapResponse;
import com.gdn.x.productcategorybase.dto.response.SingleObjectResponse;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.GdnBaseEntity;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.gdn.mta.product.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Repository
@Slf4j
public class ProductRepositoryBean implements ProductRepository {

  private static final Logger LOGGER = LoggerFactory.getLogger(ProductRepositoryBean.class);

  @Autowired
  private PCBFeign pcbFeign;

  @Autowired
  private ProductOutbound productOutbound;

  @Value("${add.delete.variants.switch}")
  private boolean addDeleteVariantSwitch;

  @Value("${family.color.attribute.code}")
  private String familyColorAttributeCode;

  @Value("${combined.edit.flow.enabled}")
  private boolean combinedEditFlowEnabled;

  @Value("${handle.add.delete.variant.mismatch}")
  private boolean handleAddDeleteVariantMismatch;

  @Value("${delete.all.items.for.deleted.attribute.enabled}")
  private boolean deleteAllItemsForDeletedAttributeEnabled;

  @Value("${delete.items.from.pcb.for.missing.variants.enabled}")
  private boolean deleteItemsFromPcbForMissingVariants;

  @Value("${validate.duplicate.defining.product.attribute.value}")
  private boolean validateDuplicateDefiningProductAttributeValue;

  @Value("${size.chart.value.type.delimiter}")
  private String sizeChartValueTypeDelimiter;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Override
  public void activate(Product product) throws Exception {
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    request.setActivated(true);
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    setProductCategoryRequest(product, request);
    setProductAttribute(product, request);
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemRequest.getImages().add(image);
      }
      request.getProductItems().add(productItemRequest);
    }
    setProductImages(product, request);
    GdnBaseRestResponse response = pcbFeign.activateProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
    }
  }

  @Override
  public void delete(ProductDetailResponse product) throws Exception {
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    request.setMarkForDelete(true);
    BeanUtils
        .copyProperties(product, request, "productCategoryResponses", "productAttributeResponses",
            "productItemResponses", "images");
    for (ProductCategoryResponse productCategory : product.getProductCategoryResponses()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      productCategoryRequest.setMarkForDelete(true);
      request.getProductCategories().add(productCategoryRequest);
    }
    for (ProductAttributeResponse productAttribute : product.getProductAttributeResponses()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest);
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      productAttributeRequest.setMarkForDelete(true);
      request.getProductAttributes().add(productAttributeRequest);
    }
    for (ProductItemResponse productItem : product.getProductItemResponses()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest, "images");
      for ( Image productItemImage : productItem.getImages()) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemRequest.getImages().add(image);
      }
      productItemRequest.setMarkForDelete(true);
      request.getProductItems().add(productItemRequest);
    }
    for (Image productImage : product.getImages()) {
      Image image = new Image();
      BeanUtils.copyProperties(productImage, image);
      request.getImages().add(image);
    }
    try{
      GdnBaseRestResponse response =
          pcbFeign.discardProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
              GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
              GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
              request);

      if (!response.isSuccess()) {
        throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
      }
    } catch(Exception e){
      Log.error(e.getMessage() + e.getClass().getName(), e);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, e.getMessage());
    }
  }

  @Override
  public ProductDetailResponse findProductDetailByProductCode(String productCode, boolean inAllProducts)
      throws Exception {
    return pcbFeign.getProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), inAllProducts,
        productCode, true).getValue();
  }

  @Override
  public ProductResponse findProductBasicDetailByProductCode(String productCode) throws Exception {
    return pcbFeign.getProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        productCode, false).getValue();
  }

  @Override
  public ProductDetailResponse findProductDetailByProductCode(String productCode) throws ApplicationRuntimeException {
    GdnRestSingleResponse<ProductDetailResponse> response =
        pcbFeign.getProductDetailByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
            productCode, true);
    if (!response.isSuccess()) {
      LOGGER.error("Error while getting productDetailResponse for : {}", productCode);
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public Page<Product> findByStoreId(String storeId, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductSummary(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize());
    List<Product> products = new ArrayList<Product>();
    for (ProductResponse productResponse : response.getContent()) {
      Product product = new Product();
      BeanUtils.copyProperties(productResponse, product);
      products.add(product);
    }
    Page<Product> page = new PageImpl<Product>(products, pageable, response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<Product> findByStoreIdAndName(String storeId, String name, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductByName(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), name);
    List<Product> products = new ArrayList<Product>();
    for (ProductResponse productResponse : response.getContent()) {
      Product product = new Product();
      BeanUtils.copyProperties(productResponse, product);
      products.add(product);
    }
    Page<Product> page = new PageImpl<Product>(products, pageable, response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<Product> findByStoreIdAndNameAndViewableAndActivated(String storeId, String name, boolean viewable,
      boolean activated, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductByNameAndViewableAndActivated(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), name, viewable, activated);
    List<Product> products = new ArrayList<Product>();
    for (ProductResponse productResponse : response.getContent()) {
      Product product = new Product();
      BeanUtils.copyProperties(productResponse, product);
      products.add(product);
    }
    Page<Product> page = new PageImpl<Product>(products, pageable, response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<Product> findByStoreIdAndProductCode(String storeId, String productCode, Pageable pageable)
      throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), productCode);
    Page<Product> page = new PageImpl<>(getProductsFromResponses(response), pageable,
        response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<Product> findByStoreIdAndViewable(String storeId, boolean viewable, Pageable pageable) throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductByViewable(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), viewable);
    List<Product> products = new ArrayList<Product>();
    for (ProductResponse productResponse : response.getContent()) {
      Product product = new Product();
      BeanUtils.copyProperties(productResponse, product);
      products.add(product);
    }
    Page<Product> page = new PageImpl<Product>(products, pageable, response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Page<Product> findByStoreIdAndViewableAndActivated(String storeId, boolean viewable, boolean activated,
      Pageable pageable) throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductByViewableAndActivated(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageNumber(), pageable.getPageSize(), viewable, activated);
    List<Product> products = new ArrayList<Product>();
    for (ProductResponse productResponse : response.getContent()) {
      Product product = new Product();
      BeanUtils.copyProperties(productResponse, product);
      products.add(product);
    }
    Page<Product> page = new PageImpl<Product>(products, pageable, response.getPageMetaData().getTotalRecords());
    return page;
  }

  @Override public List<ProductItemResponse> findProductItemsByProductId(String productId)
      throws Exception {
    return pcbFeign.getProductItemWithAttributeValues(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productId)
        .getContent();
  }

  @Override
  public Product findOne(String id) throws Exception {
    Product product = null;
    ProductDetailResponse response =
        pcbFeign.getProductDetailWithOriginalImages(GdnMandatoryRequestParameterUtil.getStoreId(),
                GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
                GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), id, true)
            .getValue();
    return getProduct(product, response);
  }

  @Override
  public ProductDetailResponse findProductResponse(String id) throws Exception {
    return pcbFeign
        .getProductDetailWithOriginalImages(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            id, true).getValue();
  }

  private Product getProduct(Product product, ProductDetailResponse response) {
    if (response != null) {
      product = new Product();
    }
    BeanUtils.copyProperties(response, product);
    if (response.getProductItemResponses() != null) {
      for (ProductItemResponse productItemResponse : response.getProductItemResponses()) {
        ProductItem productItem = new ProductItem();
        BeanUtils.copyProperties(productItemResponse, productItem);
        for (Image productItemImageResponse : productItemResponse.getImages()) {
          ProductItemImage productItemImage = new ProductItemImage();
          BeanUtils.copyProperties(productItemImageResponse, productItemImage);
          productItem.getProductItemImages().add(productItemImage);
        }
        if (CollectionUtils.isNotEmpty(productItemResponse.getProductItemAttributeValueResponses())) {
          for (ProductItemAttributeValueResponse productItemAttributeValueResponse : productItemResponse.getProductItemAttributeValueResponses()) {
            ProductItemAttributeValue productItemAttributeValue = new ProductItemAttributeValue();
            productItemAttributeValue.setValue(productItemAttributeValueResponse.getValue());
            Attribute attribute = new Attribute();
            BeanUtils.copyProperties(productItemAttributeValueResponse.getAttributeResponse(), attribute);
            productItemAttributeValue.setAttribute(attribute);
            productItem.getProductItemAttributeValues().add(productItemAttributeValue);
          }
        }
        product.getProductItems().add(productItem);
      }
    }
    if (response.getProductAttributeResponses() != null) {
      for (ProductAttributeResponse productAttributeResponse : response
          .getProductAttributeResponses()) {
        ProductAttribute productAttribute = new ProductAttribute();
        Attribute attribute = new Attribute();
        BeanUtils.copyProperties(productAttributeResponse, productAttribute);
        BeanUtils.copyProperties(productAttributeResponse.getAttribute(), attribute);
        attribute.setAttributeType(
            AttributeType.valueOf(productAttributeResponse.getAttribute().getAttributeType()));
        productAttribute.setAttribute(attribute);

        List<ProductAttributeValueResponse> productAttributeValueResponses =
            productAttributeResponse.getProductAttributeValues();
        productAttribute.setProductAttributeValues(new ArrayList<ProductAttributeValue>());
        if (CollectionUtils.isNotEmpty(productAttributeValueResponses)) {
          for (ProductAttributeValueResponse productAttributeValueResponse : productAttributeValueResponses) {
            ProductAttributeValue productAttributeValue = new ProductAttributeValue();
            BeanUtils.copyProperties(productAttributeValueResponse, productAttributeValue, "allowedAttributeValue", "predefinedAllowedAttributeValue", "descriptiveAttributeValueType");
            productAttributeValue.setDescriptiveAttributeValue(
                productAttributeValueResponse.getDescriptiveAttributeValue());
            AllowedAttributeValue allowedAttributeValue = new AllowedAttributeValue();
            if (productAttributeValueResponse.getAllowedAttributeValue() != null) {
              BeanUtils.copyProperties(productAttributeValueResponse.getAllowedAttributeValue(),
                  allowedAttributeValue);
            }
            PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
                new PredefinedAllowedAttributeValue();
            if (productAttributeValueResponse.getPredefinedAllowedAttributeValue() != null) {
              BeanUtils.copyProperties(
                  productAttributeValueResponse.getPredefinedAllowedAttributeValue(),
                  predefinedAllowedAttributeValue);
            }
            if (productAttributeValueResponse.getDescriptiveAttributeValueType() != null) {
              productAttributeValue.setDescriptiveAttributeValueType(DescriptiveAttributeValueType
                  .valueOf(
                      productAttributeValueResponse.getDescriptiveAttributeValueType().toString()));
            }
            productAttributeValue.setAllowedAttributeValue(allowedAttributeValue);
            productAttributeValue
                .setPredefinedAllowedAttributeValue(predefinedAllowedAttributeValue);
            productAttributeValue.setMarkForDelete(productAttributeValueResponse.isMarkForDelete());
            productAttribute.getProductAttributeValues().add(productAttributeValue);
          }
        }

        product.getProductAttributes().add(productAttribute);
      }
    }
    if (response.getProductCategoryResponses() != null) {
      for (ProductCategoryResponse productCategoryResponse : response.getProductCategoryResponses()) {
        ProductCategory productCategory = new ProductCategory();
        Category category = new Category();
        BeanUtils.copyProperties(productCategoryResponse, productCategory);
        BeanUtils.copyProperties(productCategoryResponse.getCategory(), category);
        productCategory.setCategory(category);
        product.getProductCategories().add(productCategory);
      }
    }
    if (response.getImages() != null) {
      for (Image productImageResponse : response.getImages()) {
        ProductImage productImage = new ProductImage();
        BeanUtils.copyProperties(productImageResponse, productImage);
        product.getProductImages().add(productImage);
      }
    }
    return product;
  }

  @Override
  @Deprecated
  public void save(Product product, String businessPartnerCode) throws Exception {
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    request.setCreatedMerchant(businessPartnerCode);
    setProductCategoryRequest(product, request);
    setProductAttribute(product, request);
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemRequest.getImages().add(image);
      }
      request.getProductItems().add(productItemRequest);
    }
    setProductImages(product, request);
    GdnBaseRestResponse response =
        pcbFeign.createNewProductWithSpecificationDetailGeneratedBySystem(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
    }
  }

  @Override
  public void updateForMerge(Product product) throws Exception {
    ProductRequest request = new ProductRequest();
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    request.setProductItems(new ArrayList<ProductItemRequest>());
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setImages(new ArrayList<Image>());
    setProductCategoryRequest(product, request);
    setProductAttribute(product, request);
    for (ProductItem productItem : product.getProductItems()) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
        Image image = new Image();
        BeanUtils.copyProperties(productItemImage, image);
        productItemRequest.getImages().add(image);
      }
      request.getProductItems().add(productItemRequest);
    }
    setProductImages(product, request);
  }

  @Override
  public SimpleMasterProductUpdateResponse updateSimpleMasterProduct(SimpleMasterProductUpdateRequestDTO simpleMasterProductUpdateRequestDTO)
  throws Exception{
    SimpleMasterProductUpdateRequest simpleMasterProductUpdateRequest = new SimpleMasterProductUpdateRequest();
    BeanUtils.copyProperties(simpleMasterProductUpdateRequestDTO, simpleMasterProductUpdateRequest);
    GdnRestSingleResponse<SimpleMasterProductUpdateResponse> response =
        pcbFeign.updateSimpleMasterData(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            simpleMasterProductUpdateRequest);
    if (!response.isSuccess()) {
      LOGGER.error("failed to update product item in PCB database, productCode : {}, errorMsg: {}",
              simpleMasterProductUpdateRequestDTO.getProductCode(), response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public List<NewlySavedItemResponse> update(Product product, Boolean isPristineCategory, boolean onlyVatChanged,
      boolean scoreUpdated, boolean resetExtractedAttributeValue, List<ProductItemLevel3> newlyAddedItems,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap, List<String> deletedItems,
      boolean ignoreSaveItemCall, boolean ignoreSalesCategoryPublish, List<String> deletedAttributeCodes,
      ProductL3Response saveProductDataResponse, EditProductResponse editProductResponse)
      throws ApplicationException {
    ProductRequest productRequest =
        prepareProductRequestForPCBUpdate(product, deletedItems, isPristineCategory, scoreUpdated, newlyAddedItems,
            productItemAttributeValueRequestMap, null, deletedAttributeCodes, saveProductDataResponse,
            editProductResponse);
    if(Objects.nonNull(productRequest)){
    GdnRestListResponse<NewlySavedItemResponse> newlySavedItemResponse =
        pcbFeign.updateProductItem(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), true, onlyVatChanged,
          resetExtractedAttributeValue, ignoreSalesCategoryPublish, productRequest);
      if (!newlySavedItemResponse.isSuccess()) {
        LOGGER
            .error("failed to update product item in PCB database, productCode : {}, errorMsg: {}",
                productRequest.getProductCode(), newlySavedItemResponse.getErrorMessage());
        throw new ApplicationException(ErrorCategory.DATA_ACCESS, newlySavedItemResponse.getErrorMessage());
      }
      if (!ignoreSaveItemCall) {
        validateProductItems(productRequest);
      }

      if (!ignoreSaveItemCall) {
        GdnBaseRestResponse response = pcbFeign.saveProductItem(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productRequest);
        if (!response.isSuccess()) {
          LOGGER.error("failed to save product item in PCB database, productCode : {}, errorMsg: "
              + "{}", productRequest.getProductCode(),
              response.getErrorMessage());
          throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
        }
      }
      return newlySavedItemResponse.getContent();
    }
    else {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS,
          "Produk sudah diupdate user lain. Silakan refresh halaman.");
    }
  }

  private void getNewlyAddedItemRequests(List<ProductItemLevel3> newlyAddedItems,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap, ProductRequest request,
      List<Image> newItemCommonImages) throws ApplicationRuntimeException {
    AttributeResponse familyColor = null;
    for (ProductItemLevel3 productItemLevel3 : newlyAddedItems) {
      ProductItemRequest productItemRequest = new ProductItemRequest();
      productItemRequest.setGeneratedItemName(productItemLevel3.getItemName());
      productItemRequest.setUpcCode(productItemLevel3.getUpcCode());
      productItemRequest.setActivated(true);
      productItemRequest.setViewable(true);
      productItemRequest.setDangerousGoodsLevel(0);
      productItemRequest.setContentChanged(true);
      productItemRequest.getImages().addAll(newItemCommonImages);
      productItemRequest.setAttributesMap(productItemLevel3.getItemAttributesMap());
      productItemRequest.setProductItemAttributeValues(new ArrayList<>());
      familyColor = getFamilyColor(familyColor, productItemLevel3, productItemRequest);
      for (Map.Entry<String, String> entry : productItemLevel3.getItemAttributesMap().entrySet()) {
        ProductItemAttributeValueRequest productItemAttributeValueRequest =
            productItemAttributeValueRequestMap.get(entry.getValue() + entry.getKey());
        productItemRequest.getProductItemAttributeValues().add(productItemAttributeValueRequest);
      }
      request.getNewlyAddedProductItems().add(productItemRequest);
    }
  }

  @Override
  public ProductRequest prepareProductRequestForPCBUpdate(Product product, List<String> deletedItems,
      Boolean isPristineCategory, boolean scoreUpdated, List<ProductItemLevel3> newlyAddedItems,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap,
      ProductDetailEditDTO productDetailEditDTO, List<String> deletedAttributeCodes,
      ProductL3Response savedProductDataResponse, EditProductResponse editProductResponse)
      throws ApplicationRuntimeException {
    String productCode = product.getProductCode();
    LOGGER.info("Processing product update in PCB database, product Code : {}", productCode);
    ProductRequest request = new ProductRequest();
    request.setProductCategories(new ArrayList<ProductCategoryRequest>());
    request.setProductAttributes(new ArrayList<ProductAttributeRequest>());
    request.setProductItems(new ArrayList<ProductItemRequest>());
    BeanUtils.copyProperties(product, request, "productCategories", "productAttributes", "productItems");
    ProductDetailResponse savedProduct =
        pcbFeign.getProductDetailWithOriginalImages(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            product.getId(), true).getValue();
    Map<String, String> extraDeletedItemIdAndItemCode =  deleteItemsForDeletedAttribute(deletedItems, deletedAttributeCodes, savedProduct, savedProductDataResponse);
    editProductResponse.setExtraDeletedItems(extraDeletedItemIdAndItemCode);
    Optional.ofNullable(productDetailEditDTO).ifPresent(
      dto -> dto.setEligibleForPCBUpdate(savedProduct.getVersion() <= product.getVersion()));
    // TODO New Category Add
    ProductRequest productRequest = prepareRequestForPCB(product, deletedItems, isPristineCategory, scoreUpdated, newlyAddedItems,
        productItemAttributeValueRequestMap, request, savedProduct);
    if (Objects.nonNull(productRequest)) {
      RequestHelper.validateProductAttributeRequest(productRequest, sizeChartValueTypeDelimiter,
          validateDuplicateDefiningProductAttributeValue);
      productRequest.setDistributionInfoRequest(editProductResponse.getDistributionInfoRequest());
      RequestHelper.setItemDistributionInfo(ranchIntegrationEnabled, productRequest, editProductResponse);
    }
    RequestHelper.setVideoAddEditRequestForPCB(productRequest, editProductResponse);
    return productRequest;
  }

  private ProductRequest prepareRequestForPCB(Product product, List<String> deletedItems, Boolean isPristineCategory,
      boolean scoreUpdated, List<ProductItemLevel3> newlyAddedItems,
      Map<String, ProductItemAttributeValueRequest> productItemAttributeValueRequestMap, ProductRequest request,
      ProductDetailResponse savedProduct) {
    if (savedProduct.getVersion() <= product.getVersion()) {
      request.setReviewPending(product.isReviewPending());
      setProductCategoryRequest(product, request);
      setProductAttribute(product, request);
      List<Image> newItemCommonImages = new ArrayList<>();
      setNewItemCommonImages(product, newItemCommonImages);
      deleteProductItemAndItemImages(product, deletedItems, request);
      getNewlyAddedItemRequests(newlyAddedItems, productItemAttributeValueRequestMap, request, newItemCommonImages);
      if(CollectionUtils.isNotEmpty(newlyAddedItems) && handleAddDeleteVariantMismatch){
        processPCBRequestUpdateToHandleMissingVariants(product, newlyAddedItems, request);
      }
      setProductImages(product, request);
      request.setPristineCategory(isPristineCategory);
      request.setScoreUpdated(scoreUpdated);
      if(isCombinedEditAndAddDeleteSwitchEnabled()){
        validateProductItems(request);
      }
      return request;
    }
    return null;
  }

  private boolean isCombinedEditAndAddDeleteSwitchEnabled() {
    return combinedEditFlowEnabled && addDeleteVariantSwitch;
  }

  private static void setProductImages(Product product, ProductRequest request) {
    for (ProductImage productImage : product.getProductImages()) {
      Image image = new Image();
      BeanUtils.copyProperties(productImage, image);
      request.getImages().add(image);
    }
  }

  private static void deleteProductItemAndItemImages(Product product, List<String> deletedItems,
      ProductRequest request) {
    for (ProductItem productItem : product.getProductItems()) {
      boolean itemDeleted = deletedItems.contains(productItem.getSkuCode());
      ProductItemRequest productItemRequest = new ProductItemRequest();
      BeanUtils.copyProperties(productItem, productItemRequest);
      if (itemDeleted) {
        productItemRequest.setMarkForDelete(true);
      }
      deleteItemImages(productItem, itemDeleted, productItemRequest);
      request.getProductItems().add(productItemRequest);
    }
  }

  private static void deleteItemImages(ProductItem productItem, boolean itemDeleted,
      ProductItemRequest productItemRequest) {
    for (ProductItemImage productItemImage : productItem.getProductItemImages()) {
      Image image = new Image();
      BeanUtils.copyProperties(productItemImage, image);
      if (itemDeleted) {
        image.setMarkForDelete(true);
      }
      productItemRequest.getImages().add(image);
    }
  }

  private static void setNewItemCommonImages(Product product, List<Image> newItemCommonImages) {
    for (ProductImage productImage : product.getProductImages()) {
      if (!productImage.isMarkForDelete() && productImage.isCommonImage()) {
        Image productItemImage = new Image();
        BeanUtils.copyProperties(productImage, productItemImage);
        newItemCommonImages.add(productItemImage);
      }
    }
  }

  private static void setProductAttribute(Product product, ProductRequest request) {
    for (ProductAttribute productAttribute : product.getProductAttributes()) {
      ProductAttributeRequest productAttributeRequest = new ProductAttributeRequest();
      AttributeRequest attributeRequest = new AttributeRequest();
      BeanUtils.copyProperties(productAttribute, productAttributeRequest, "attribute", "productAttributeValues");
      BeanUtils.copyProperties(productAttribute.getAttribute(), attributeRequest);
      productAttributeRequest.setAttribute(attributeRequest);
      productAttributeRequest.setProductAttributeValues(
          Optional.of(productAttribute.getProductAttributeValues()).map(RequestHelper::toProductAttributeValueRequest)
              .orElse(null));
      request.getProductAttributes().add(productAttributeRequest);
    }
  }


  private static void setProductCategoryRequest(Product product, ProductRequest request) {
    for (ProductCategory productCategory : product.getProductCategories()) {
      ProductCategoryRequest productCategoryRequest = new ProductCategoryRequest();
      CategoryRequest categoryRequest = new CategoryRequest();
      BeanUtils.copyProperties(productCategory, productCategoryRequest);
      BeanUtils.copyProperties(productCategory.getCategory(), categoryRequest);
      productCategoryRequest.setCategory(categoryRequest);
      request.getProductCategories().add(productCategoryRequest);
    }
  }

  private Map<String, String> deleteItemsForDeletedAttribute(List<String> deletedItems,
      List<String> deletedAttributeCodes, ProductDetailResponse savedProduct,
      ProductL3Response savedProductDataResponse) {
    Map<String, String> updatedDeletedItems = new HashMap<>();

    if (deleteAllItemsForDeletedAttributeEnabled) {
      Map<String, String> extraDeletedItemIdAndItemCode = Optional.ofNullable(savedProduct.getProductItemResponses()).orElse(new HashSet<>()).stream()
          .filter(productItemResponse -> productItemResponse.getProductItemAttributeValueResponses().stream().anyMatch(
              productItemAttributeValueResponse -> isItemAttributeDeleted(deletedItems, deletedAttributeCodes,
                  productItemResponse, productItemAttributeValueResponse)))
          .collect(Collectors.toMap(ProductItemResponse::getId, ProductItemResponse::getSkuCode, (v1, v2) -> v2));
      updatedDeletedItems.putAll(extraDeletedItemIdAndItemCode);

      log.info("Deleting extra items containing deleted attribute code for productCode : {} , extra items : {} ",
          savedProduct.getProductCode(), updatedDeletedItems);
      deletedItems.addAll(updatedDeletedItems.values());
      if (deleteItemsFromPcbForMissingVariants && Objects.nonNull(savedProductDataResponse) && MapUtils.isNotEmpty(
          savedProductDataResponse.getItemSkuItemCodeMap())) {
        deleteItemsForDeletedAttributeForBulkCreatedProducts(deletedItems, savedProduct, savedProductDataResponse, updatedDeletedItems);
      }
    }

    return updatedDeletedItems;
  }

  private void deleteItemsForDeletedAttributeForBulkCreatedProducts(List<String> deletedItems,
      ProductDetailResponse savedProduct, ProductL3Response savedProductDataResponse, Map<String, String> updatedDeletedItems) {
    List<ProductItemResponse> itemsFromPcb =
        savedProduct.getProductItemResponses().stream().filter(Predicate.not(ProductItemResponse::isMarkForDelete))
            .collect(Collectors.toList());
    List<String> itemCodesFromXproduct = new ArrayList<>();
    Optional<Map<String, String>> itemSkuItemCodeMapOptional =
        Optional.ofNullable(savedProductDataResponse.getItemSkuItemCodeMap());
    if (itemSkuItemCodeMapOptional.isPresent()) {
      Map<String, String> itemSkuItemCodeMap = itemSkuItemCodeMapOptional.get();
      itemCodesFromXproduct.addAll(itemSkuItemCodeMap.values());
    }
    Set<String> missingVariants = new HashSet<>();
    for (ProductItemResponse productItemResponse : itemsFromPcb) {
      if (!itemCodesFromXproduct.contains(productItemResponse.getSkuCode())) {
        log.info("Deleting missing variants from PCB with itemCode : {} ", productItemResponse.getSkuCode());
        missingVariants.add(productItemResponse.getSkuCode());
        updatedDeletedItems.put(productItemResponse.getId(), productItemResponse.getSkuCode());
      }
    }
    deletedItems.addAll(missingVariants);
  }

  private static boolean isItemAttributeDeleted(List<String> deletedItems, List<String> deletedAttributeCodes,
      ProductItemResponse productItemResponse, ProductItemAttributeValueResponse productItemAttributeValueResponse) {
    return deletedAttributeCodes.contains(productItemAttributeValueResponse.getAttributeResponse().getAttributeCode())
        && !deletedItems.contains(productItemResponse.getSkuCode());
  }

  private static void processPCBRequestUpdateToHandleMissingVariants(Product product,
    List<ProductItemLevel3> newlyAddedItems, ProductRequest request) {
    //  map of saved product item from PCB with product IDs to defining variant attributes
    // "id" -> ["UA-M000044-L,WA-2000060-Small"]

    Map<String, String> savedItemXAttributeCodeAttributeValue = getSavedItemXAttributeCodeAttributeValue(product);

    Set<String> duplicateVariantsWithSameDefiningAttributes =
      getDuplicateVariantsWithSameDefiningAttributes(newlyAddedItems, savedItemXAttributeCodeAttributeValue);

    // IF duplicate variants with same defining attributes are found , we will delete them and
    // create new product Item with same attributes , this will help with resolving data mismatch
    // in x-product
    for (ProductItemRequest productItemRequest : request.getProductItems()) {
      if (duplicateVariantsWithSameDefiningAttributes.contains(productItemRequest.getId())) {
        productItemRequest.setMarkForDelete(true);
      }
    }
  }

  private static Set<String> getDuplicateVariantsWithSameDefiningAttributes(List<ProductItemLevel3> newlyAddedItems,
    Map<String, String> savedItemXAttributeCodeAttributeValue) {
    // list of newly added product item attribute values
    List<String> newlyAddedAttributeCodeAttributeValue =
      newlyAddedItems.stream().map(ProductItemLevel3::getItemAttributesMap).map(
        map -> map.entrySet().stream().map(entry -> Optional.ofNullable(entry.getKey())
            .orElse(org.apache.commons.lang3.StringUtils.EMPTY).concat(Constants.DASH_DELIMITER)
            .concat(Optional.ofNullable(entry.getValue())
              .orElse(org.apache.commons.lang3.StringUtils.EMPTY))).sorted(Comparator.naturalOrder())
          .collect(Collectors.joining(Constants.COMMA))).collect(Collectors.toList());


    // Finding duplicate variants with same defining attributes
      return savedItemXAttributeCodeAttributeValue.entrySet().stream()
        .filter(entry -> newlyAddedAttributeCodeAttributeValue.contains(entry.getValue()))
        .map(Map.Entry::getKey).collect(Collectors.toSet());
  }

  private static Map<String, String> getSavedItemXAttributeCodeAttributeValue(Product product) {
    return product.getProductItems().stream().filter(Objects::nonNull).collect(Collectors.toMap(GdnBaseEntity::getId,
        productItem -> Optional.ofNullable(productItem.getProductItemAttributeValues()).orElse(new ArrayList<>())
            .stream().filter(productItemAttributeValue -> Optional.ofNullable(productItemAttributeValue.getAttribute())
                .map(Attribute::isVariantCreation).orElse(false))
            //only considering variant creating attributes and concatenating AttributeCode and
            // Value to maintain uniqueness
            .map(productItemAttributeValue -> Optional.ofNullable(productItemAttributeValue.getAttribute())
                .map(Attribute::getAttributeCode).orElse(org.apache.commons.lang3.StringUtils.EMPTY)
                .concat(Constants.DASH_DELIMITER).concat(productItemAttributeValue.getValue()))
            .sorted(Comparator.naturalOrder()).collect(Collectors.joining(Constants.COMMA))));
    // create a comma separated list [ATR123-L,ATR-456-Green]
  }


  private AttributeResponse getFamilyColor(AttributeResponse familyColor, ProductItemLevel3 productItemLevel3,
      ProductItemRequest productItemRequest) throws ApplicationRuntimeException {
    for (ProductLevel3Attribute itemAttribute : productItemLevel3.getItemAttributes()) {
      if (familyColorAttributeCode.equals(itemAttribute.getAttributeCode())) {
        if (Objects.isNull(familyColor)) {
          GdnRestListResponse<AttributeResponse> response =
              pcbFeign.getAttributeDetailByAttributeCode(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
                  Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME,
                  familyColorAttributeCode);
          if (!response.isSuccess() || CollectionUtils.isEmpty(response.getContent())) {
            LOGGER.error("failed to fetch family color attribute with attributeCode = {}", familyColorAttributeCode);
            throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
          }
            familyColor = response.getContent().get(0);
        }
        ProductItemAttributeValueRequest productItemAttributeValueRequest = new ProductItemAttributeValueRequest();
        AttributeRequest attributeRequest = new AttributeRequest();
        BeanUtils.copyProperties(familyColor, attributeRequest);
        attributeRequest.setAttributeType(com.gdn.x.productcategorybase.dto.AttributeType.PREDEFINED_ATTRIBUTE);
        productItemAttributeValueRequest.setAttribute(attributeRequest);
        productItemAttributeValueRequest.setValue(itemAttribute.getValues().get(0));
        productItemRequest.getProductItemAttributeValues().add(productItemAttributeValueRequest);
      }
    }
    return familyColor;
  }

  private void validateProductItems(ProductRequest request) {
    ProductDetailResponse product =
        pcbFeign.getProductDetailWithOriginalImages(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            request.getId(), true).getValue();
    List<ProductItemRequest> validProductItems = new ArrayList<ProductItemRequest>();
    for (ProductItemResponse productItemResponse : product.getProductItemResponses()) {
      boolean equal = false;
      for (ProductItemRequest productItemRequest : request.getProductItems()) {
        if (productItemResponse.getId().equals(productItemRequest.getId())
            && productItemResponse.getGeneratedItemName().equals(productItemRequest.getGeneratedItemName())) {
          validProductItems.add(productItemRequest);
          equal = true;
          break;
        }
      }
      if (!equal) {
        ProductItemRequest productItemRequest = new ProductItemRequest();
        BeanUtils.copyProperties(productItemResponse, productItemRequest, "images");
        for (Image imageItemResponse : productItemResponse.getImages()) {
          Image imageItemRequest = new Image();
          BeanUtils.copyProperties(imageItemResponse, imageItemRequest);
          productItemRequest.getImages().add(imageItemRequest);
        }
        validProductItems.add(productItemRequest);
      }
    }
    request.setProductItems(validProductItems);
  }

  @Override
  public void updateViewable(String productCode, boolean viewable) throws Exception {
    GdnBaseRestResponse response = this.pcbFeign.updateProductViewable(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productCode,
        viewable);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public ActivateImageResponse updateProductImageName(ActivateImageRequest request)
      throws Exception {
    GdnRestSingleResponse<ActivateImageResponse> response =
        this.pcbFeign.updateProductImageName(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public ActivateImageResponse updateProductImagesName(ProductActivateImageRequest request)
      throws Exception {
    GdnRestSingleResponse<ActivateImageResponse> response =
        pcbFeign.updateProductImagesName(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
            request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void create(ProductRequest request) throws Exception {
    GdnBaseRestResponse response = this.pcbFeign.createNewProductWithSpecificationDetailGeneratedBySystem(
        GdnMandatoryRequestParameterUtil.getStoreId(), GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(), GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public Map<String, String> createProduct(ProductRequest request, boolean computeCommonImage) throws Exception {
    GdnRestSingleResponse<MapResponse<String, String>> response =
        pcbFeign.createProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            computeCommonImage, request);
    if (!response.isSuccess()) {
      if (ErrorCategory.VALIDATION.getCode().equals(response.getErrorCode())) {
        throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
            "[" + response.getErrorCode() + "] " + response.getErrorMessage());
      }
      throw new ApplicationException(ErrorCategory.UNSPECIFIED,
          "[" + response.getErrorCode() + "] " + response.getErrorMessage());
    }
    return response.getValue().getMap();
  }

  @Override
  public void updateActivated(String productCode, boolean activated) throws Exception {
    GdnBaseRestResponse response = this.pcbFeign.updateProductActivated(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productCode,
        activated);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public List<ConfigurationStatusResponse> getConfigurationStatus(List<ConfigurationStatusRequest> requests) throws Exception {
    GdnRestListResponse<ConfigurationStatusResponse> response =
        this.pcbFeign.getReviewConfiguration(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), requests);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    return response.getContent();
  }

  @Override
  public ProductDetailResponse findDetailById(String productId) throws Exception {
    GdnRestSingleResponse<ProductDetailResponse> response =
        this.pcbFeign.getProductDetailWithOriginalImages(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), productId,
            true);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
    ProductDetailResponse product = response.getValue();
    return product;
  }

  @Override
  public Page<Product> findByStoreIdAndProductCodeExactMatch(String storeId, String productCode,
      Pageable pageable) throws Exception {
    GdnRestListResponse<ProductResponse> response =
        pcbFeign.getProductByProductCodeExactMatch(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            pageable.getPageSize(), pageable.getPageNumber(), productCode);
    Page<Product> page = new PageImpl<>(getProductsFromResponses(response), pageable, response
        .getPageMetaData().getTotalRecords());
    return page;
  }

  @Override
  public Integer getProductCountByViewable(boolean viewable) throws Exception {
    int productCount = 0;
    GdnRestSingleResponse<SingleObjectResponse<Long>> response =
        pcbFeign.getProductCountByViewable(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), viewable);
    if (response != null) {
      SingleObjectResponse<Long> singleObjectResponse = response.getValue();
      if (singleObjectResponse != null) {
        Long value = singleObjectResponse.getValue();
        if (value != null) {
          productCount = value.intValue();
        }
      }
    }
    return productCount;
  }

  private List<Product> getProductsFromResponses(GdnRestListResponse<ProductResponse> response) {
    List<Product> productList = new ArrayList<>();
    for (ProductResponse productResponse : response.getContent()) {
      Product product = new Product();
      BeanUtils.copyProperties(productResponse, product);
      productList.add(product);
    }
    return productList;
  }

  @Override
  public void updateProductContent(ProductRequest request) throws Exception {
    GdnBaseRestResponse response = this.pcbFeign.updateProductContent(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
        request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public void updateProductImage(ProductRequest request) throws Exception {
    GdnBaseRestResponse response = this.pcbFeign.updateProductImage(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), request);
    if (!response.isSuccess()) {
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, "[" + response.getErrorCode()
          + "] " + response.getErrorMessage());
    }
  }

  @Override
  public GdnRestListResponse<ProductDetailResponse> getProductDetailsByProductCodes(
      String requestId, String username, List<String> productCodeList) throws Exception {
    GdnRestListResponse<ProductDetailResponse> response =
        pcbFeign.getAllProductDetailListByProductCodes(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, productCodeList, false, true);
    if (!response.isSuccess()) {
      LOGGER.error(
          "error invoking getProductDetailsByProductCodes from product category base service. "
              + "requestId: {}, username: {}, producCodeList: {}",
          requestId, username, productCodeList);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public GdnRestListResponse<ProductDetailResponse> getAllProductDetailsByProductCodes(String requestId,
      String username, List<String> productCodeList) throws Exception {
    GdnRestListResponse<ProductDetailResponse> response = pcbFeign
        .getAllProductDetailListByProductCodes(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, requestId, username, productCodeList, true, true);
    if (!response.isSuccess()) {
      LOGGER.error("error invoking getAllProductDetailListByProductCodes from product category base service. "
          + "requestId: {}, username: {}, producCodeList: {}", requestId, username, productCodeList);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response;
  }

  @Override
  public void activateAndUpdateImageName(ActivateImageRequest request) throws Exception {
    String requestId =
        StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID.randomUUID()
            .toString() : GdnMandatoryRequestParameterUtil.getRequestId();
    String username =
        StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ? GdnBaseLookup.DEFAULT_USERNAME
            : GdnMandatoryRequestParameterUtil.getUsername();
    GdnBaseRestResponse response = pcbFeign.activateAndUpdateImageName(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        username, request);
    if (!response.isSuccess()) {
      LOGGER.error("error invoking activateAndUpdateImageName from product category base service. "
          + "requestId: {}, username: {}, request: {}", requestId, username, request);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public ActivateImageResponse isProductImagesActivated(String productCode) throws Exception {
    String requestId =
        StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ? UUID.randomUUID()
            .toString() : GdnMandatoryRequestParameterUtil.getRequestId();
    String username =
        StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ?
          GdnBaseLookup.DEFAULT_USERNAME
            : GdnMandatoryRequestParameterUtil.getUsername();
    GdnRestSingleResponse<ActivateImageResponse> response =
        pcbFeign.isProductImagesActivated(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, productCode);
    if (!response.isSuccess()) {
      LOGGER.error("error invoking isProductImagesActivated from product category base service. "
          + "requestId: {}, username: {}, productCode: {}", requestId, username, productCode);
      throw new ApplicationException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public void updateRejectedProduct(ProductRequest productRequest) throws Exception {
    GdnBaseRestResponse response = pcbFeign.updateRejectedProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        productRequest);
    if (!response.isSuccess()) {
      LOGGER.error("failed to update product item in PCB database, productCode : {}, errorMsg: {}",
          productRequest.getProductCode(), response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.DATA_ACCESS, response.getErrorMessage());
    }
  }

  @Override
  public List<ProductItemDetailResponse> getProductItemByListOfProductCode(List<String> productCodes, Boolean isOnlyExternal,
      boolean active)
      throws Exception {
    String requestId = StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ?
        UUID.randomUUID().toString() :
        GdnMandatoryRequestParameterUtil.getRequestId();
    GdnRestListRequest listRequest = new GdnRestListRequest();
    listRequest.setRequestId(requestId);
    ProductCodesRequest productCodesRequest = new ProductCodesRequest();
    productCodesRequest.setProductCodes(productCodes);
    return productOutbound.getProductItemByListOfProductCode(productCodesRequest, false, isOnlyExternal, active);
  }

  @Override
  public void clearMasterProductCache(String productCode, String productId) throws Exception {
    String requestId = StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ?
        UUID.randomUUID().toString() :
        GdnMandatoryRequestParameterUtil.getRequestId();
    String username = StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ?
        GdnBaseLookup.DEFAULT_USERNAME :
        GdnMandatoryRequestParameterUtil.getUsername();
    GdnBaseRestResponse response = pcbFeign.clearProductCache(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
        username, productId, productCode);
    if(!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public void clearMasterProductCacheSync(String productId, String productCode) throws Exception {
    String requestId = StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getRequestId()) ?
        UUID.randomUUID().toString() :
        GdnMandatoryRequestParameterUtil.getRequestId();
    String username = StringUtils.isEmpty(GdnMandatoryRequestParameterUtil.getUsername()) ?
        GdnBaseLookup.DEFAULT_USERNAME :
        GdnMandatoryRequestParameterUtil.getUsername();
    GdnBaseRestResponse response =
        pcbFeign.clearProductCacheSyncByProductIdAndProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, productId, productCode);
    if(!response.isSuccess()) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
  }

  @Override
  public GeneratedProductImagesPathResponse replaceProductImages(String requestId, String username,
      ReplaceProductImagesRequest productImageRequest) throws Exception {
    GdnRestSingleResponse<GeneratedProductImagesPathResponse> response =
        pcbFeign.replaceProductImages(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, productImageRequest);
    return response.getValue();
  }

  @Override
  public GdnRestSingleResponse<CategorySummaryResponse> movePrdCategoryByPrdCode(String requestId, String username,
      String productCode, String categoryCode) throws Exception {
    GdnRestSingleResponse<CategorySummaryResponse> response =
        pcbFeign.movePrdCategoryByPrdCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, productCode, categoryCode);
    this.checkResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<ProductAttributeResponse> addProductAttributesByProductCode(
      String requestId, String username, AddProductAttributesRequest request) throws Exception {
    GdnRestListResponse<ProductAttributeResponse> response =
        pcbFeign.addProductAttributesByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(), requestId,
            username, request);
    this.checkResponse(response);
    return response;
  }

  @Override
  public GdnRestListResponse<CategoryResponse> filterCategoryHierarchyByCategoryCode(
      String requestId, String username, String categoryCode) throws Exception {
    GdnRestListResponse<CategoryResponse> response =
        pcbFeign.filterCategoryHierarchyByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            categoryCode);
    this.checkResponse(response);
    return response;
  }

  @Override
  public List<CatalogResponse> getCatalogByType(CatalogType catalogType, Pageable pageable)
    throws Exception {
    GdnRestListResponse<CatalogResponse> response =
      pcbFeign.getCatalogByType(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(),
        GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), pageable.getPageNumber(),
        pageable.getPageSize(), catalogType);
    this.checkResponse(response);
    return response.getContent();
  }

  @Override
  public List<ProductPredictionCategoryMappingResponse> getPredictionListByCategoryCode(String categoryCode) throws Exception {
    GdnRestListResponse<ProductPredictionCategoryMappingResponse> response = pcbFeign
        .getPredictionListByCategoryCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            categoryCode);
    this.checkResponse(response);
    return response.getContent();
  }

  @Override
  public boolean takeDownProductBasedOnBrand(ProductBrandValidationRequest productBrandValidationRequest)
      throws Exception {
    GdnRestSingleResponse<SimpleBooleanResponse> response = pcbFeign
        .takeDownProductBasedOnBrand(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productBrandValidationRequest);
    this.checkResponse(response);
    return response.getValue().getResult();
  }

  @Override
  public void updateProductAndItemImages(ProductAndItemImageRequest productAndItemImageRequest)
      throws Exception {
    GdnBaseRestResponse response =
        pcbFeign.updateProductAndItemImagesByProductCode(GdnMandatoryRequestParameterUtil.getStoreId(),
            GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(), false,
            productAndItemImageRequest);
    this.checkResponse(response);
  }

  @Override
  public void discardProduct(ProductRequest productRequest) throws Exception {
    GdnBaseRestResponse response = pcbFeign.discardProduct(GdnMandatoryRequestParameterUtil.getStoreId(),
        GdnMandatoryRequestParameterUtil.getChannelId(), GdnMandatoryRequestParameterUtil.getClientId(),
        GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
        productRequest);
    checkResponse(response);
  }

  private void checkResponse(GdnBaseRestResponse response) throws Exception {
    if(!response.isSuccess()){
      log.error("Error when getting response from PCB : {} ", response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE, response.getErrorMessage());
    }
  }

  @Override
  public CategoryRestrictedKeywordResponse getCategoryRestrictedKeywordDetail(String categoryRestrictedId)
      throws Exception {
    GdnRestSingleResponse<CategoryRestrictedKeywordResponse> categoryRestrictedKeywordDetail = pcbFeign
        .getCategoryRestrictedKeywordDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), categoryRestrictedId);
    this.checkResponse(categoryRestrictedKeywordDetail);
    return categoryRestrictedKeywordDetail.getValue();
  }

  @Override
  public List<AttributeHistoryResponse> autoFillProductAttribute(String storeId, String productCode) throws Exception {
    GdnRestListResponse<AttributeHistoryResponse> response =
        pcbFeign.autoFillProductAttribute(storeId, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(), GdnMandatoryRequestParameterUtil.getUsername(),
            productCode);
    checkResponse(response);
    return response.getContent();
  }

  @Override
  public List<CategoryRestrictedKeywordResponse> getCategoryRestrictedKeywordDetailList(String categoryCode,
      List<String> keywordIds) throws Exception {
    GdnRestListResponse<CategoryRestrictedKeywordResponse> categoryRestrictedKeywordDetailList =
        pcbFeign.getCategoryRestrictedKeywordByCategoryCodeAndKeywordIds(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(),
            new CategoryCodeAndKeywordListRequest(categoryCode, keywordIds));
    this.checkResponse(categoryRestrictedKeywordDetailList);
    return categoryRestrictedKeywordDetailList.getContent();
  }

  @Override
  public EditProductItemAndImageResponse updateProductMasterDataAndImagesAndUpcCode(
    String productCode, boolean ignoreSalesCategoryPublish,
    EditProductDetailRequest editProductDetailRequest) throws Exception {
    GdnRestSingleResponse<EditProductItemAndImageResponse> response =
      pcbFeign.updateProductMasterDataAndImagesAndUpcCode(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), productCode, ignoreSalesCategoryPublish,
        editProductDetailRequest);
    this.checkResponse(response);
    return response.getValue();
  }

  @Override
  public ProductBrandUpdateResponse updateProductBrandData(ProductBrandUpdateRequest productBrandUpdateRequest)
      throws Exception {
    GdnRestSingleResponse<ProductBrandUpdateResponse> brandUpdateResponse =
        pcbFeign.updateProductBrandData(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), productBrandUpdateRequest);
    this.checkResponse(brandUpdateResponse);
    return brandUpdateResponse.getValue();
  }

  @Override
  public SimpleStringMapResponse getSkuCodesByProductItemIds(List<String> productItemIds)
      throws Exception {
    GdnRestSingleResponse<SimpleStringMapResponse> skuCodesByProductItemIds =
        pcbFeign.getSkuCodesByProductItemIds(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), new SimpleStringListRequest(productItemIds));
    this.checkResponse(skuCodesByProductItemIds);
    return skuCodesByProductItemIds.getValue();
  }

  @Override
  public Page<ProductCodeResponse> getProductsByBrandName(String brandName, Pageable pageable)
      throws Exception {
    GdnRestListResponse<ProductCodeResponse> response =
        pcbFeign.getProductsByBrandName(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), brandName, pageable.getPageNumber(),
            pageable.getPageSize());
    if (!response.isSuccess()) {
      log.error("Error when getting response from PCB : {} ", response.getErrorMessage());
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          response.getErrorMessage());
    }
    return new PageImpl<ProductCodeResponse>(
        Optional.ofNullable(response.getContent()).orElse(List.of()), pageable,
        response.getPageMetaData().getTotalRecords());
  }

}
