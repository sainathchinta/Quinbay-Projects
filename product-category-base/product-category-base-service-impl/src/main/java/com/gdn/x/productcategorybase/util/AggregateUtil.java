package com.gdn.x.productcategorybase.util;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.domain.event.model.AggregateImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AggregateProductItemDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CatalogDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ImageDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.PredefinedAllowedAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeValueDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductCategoryDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;
import com.gdn.x.productcategorybase.domain.event.model.ProductItemDomainEventModel;
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
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import lombok.extern.slf4j.Slf4j;
import com.gdn.common.util.BeanUtils;

@Slf4j
public class AggregateUtil {

  /**
   * @param product
   * @return ProductDomainEventModel from Product
   */
  public static ProductDomainEventModel toProductDomainEventModel(Product product) {
    try {
      return Optional
          .ofNullable(product)
          .map(AggregateUtil::createProductDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create ProductDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ProductDomainEventModel createProductDomainEventModel(Product product) {
    ProductDomainEventModel productEvent = new ProductDomainEventModel();

    BeanUtils.copyProperties(product, productEvent, "productCategories","productAttributes","productImages","productItems");
    productEvent.setTimestamp(System.currentTimeMillis());
    productEvent.setNewProduct(false);
    productEvent.setProductCategories(toListProductCategoryDomainEventModel(product.getProductCategories()));
    productEvent.setProductAttributes(toListProductAttributeDomainEventModel(product.getProductAttributes()));
    productEvent.setImages(toListImageDomainEventModel(product.getProductImages()));
    productEvent.setProductItems(toListProductItemDomainEventModel(product.getProductItems()));

    return productEvent;
  }

  private static List<ProductCategoryDomainEventModel> toListProductCategoryDomainEventModel(List<ProductCategory> productCategories) {
    return productCategories.stream()
        .map(AggregateUtil::toProductCategoryDomainEventModel)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static List<ProductAttributeDomainEventModel> toListProductAttributeDomainEventModel(List<ProductAttribute> productAttributes) {
    return productAttributes.stream()
        .map(AggregateUtil::toProductAttributeDomainEventModel)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static List<ImageDomainEventModel> toListImageDomainEventModel(List<ProductImage> productImages) {
    return productImages.stream()
        .map(AggregateUtil::toImageDomainEventModel)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static List<ProductItemDomainEventModel> toListProductItemDomainEventModel(List<ProductItem> productItems) {
    return productItems.stream()
        .map(AggregateUtil::toProductItemDomainEventModel)
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  /**
   * @param productCategories
   * @return AggregateProductCategoryDomainEventModel from ProductCategory
   */
  public static AggregateProductCategoryDomainEventModel toAggregateProductCategoryDomainEventModel(
      List<ProductCategory> productCategories){
    try {
      return AggregateProductCategoryDomainEventModel
          .builder()
          .timestamp(System.currentTimeMillis())
          .productCode(toProductCodeFromProductCategories(productCategories))
          .productCategories(toListProductCategoryDomainEventModel(productCategories))
          .build();
    } catch (Exception e){
      log.error("Error while create AggregateProductCategoryDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static String toProductCodeFromProductCategories(List<ProductCategory> productCategories) {
    return Optional
        .ofNullable(productCategories)
        .map(vals -> vals.stream().findFirst().orElse(null))
        .map(ProductCategory::getProduct)
        .map(Product::getProductCode)
        .orElse(null);
  }

  private static ProductCategoryDomainEventModel toProductCategoryDomainEventModel(ProductCategory productCategory) {
    try {
      return Optional
          .ofNullable(productCategory)
          .map(AggregateUtil::createProductCategoryDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create ProductCategoryDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ProductCategoryDomainEventModel createProductCategoryDomainEventModel(ProductCategory productCategory) {
    ProductCategoryDomainEventModel result = new ProductCategoryDomainEventModel();

    result.setCategory(toCategoryDomainEventModel(productCategory.getCategory()));

    return result;
  }

  private static CategoryDomainEventModel toCategoryDomainEventModel(Category category){
    try {
      return Optional
          .ofNullable(category)
          .map(AggregateUtil::createCategoryDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create CategoryDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static CategoryDomainEventModel createCategoryDomainEventModel(Category category){
    CategoryDomainEventModel result = new CategoryDomainEventModel();

    result.setId(category.getId());
    result.setName(category.getName());
    result.setCategoryCode(category.getCategoryCode());
    result.setSequence(category.getSequence());
    result.setDescription(category.getDescription());
    result.setDisplay(category.isDisplay());
    result.setLogisticAdjustment(category.getLogisticAdjustment());
    result.setWarranty(category.isWarranty());
    result.setNeedIdentity(category.isNeedIdentity());
    result.setActivated(category.isActivated());
    result.setViewable(category.isViewable());
    result.setCatalog(toCatalogDomainEventModel(category.getCatalog()));
    result.setParentCategoryId(toParentCategoryId(category));

    return result;
  }

  private static String toParentCategoryId(Category category) {
    return Optional
        .ofNullable(category)
        .map(Category::getParentCategory)
        .map(Category::getId)
        .orElse(null);
  }

  private static CatalogDomainEventModel toCatalogDomainEventModel(Catalog catalog){
    try {
      return Optional
          .ofNullable(catalog)
          .map(AggregateUtil::createCatalogDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create CatalogDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static CatalogDomainEventModel createCatalogDomainEventModel(Catalog catalog){
    CatalogDomainEventModel result = new CatalogDomainEventModel();

    result.setName(catalog.getName());
    result.setCatalogCode(catalog.getCatalogCode());
    result.setCatalogType(toCatalogType(catalog));

    return result;
  }

  private static String toCatalogType(Catalog catalog){
    return Optional
        .ofNullable(catalog)
        .map(Catalog::getCatalogType)
        .map(ctgType -> ctgType.toString())
        .orElse(null);
  }

  /**
   * @param productAttributes
   * @return AggregateProductAttributeDomainEventModel from ProductAttribute
   */
  public static AggregateProductAttributeDomainEventModel toAggregateProductAttributeDomainEventModel(
      List<ProductAttribute> productAttributes) {
    try {
      return AggregateProductAttributeDomainEventModel
          .builder()
          .timestamp(System.currentTimeMillis())
          .productCode(toProductCodeFromProductAttributes(productAttributes))
          .productAttributes(toListProductAttributeDomainEventModel(productAttributes))
          .build();
    } catch (Exception e){
      log.error("Error while create AggregateProductAttributeDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static String toProductCodeFromProductAttributes(List<ProductAttribute> productAttributes) {
    return Optional
        .ofNullable(productAttributes)
        .map(vals -> vals.stream().findFirst().orElse(null))
        .map(ProductAttribute::getProduct)
        .map(Product::getProductCode)
        .orElse(null);
  }

  private static ProductAttributeDomainEventModel toProductAttributeDomainEventModel(ProductAttribute productAttribute) {
    try {
      return Optional
          .ofNullable(productAttribute)
          .map(AggregateUtil::createProductAttributeDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create ProductAttributeDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ProductAttributeDomainEventModel createProductAttributeDomainEventModel(ProductAttribute productAttribute) {
    ProductAttributeDomainEventModel result = new ProductAttributeDomainEventModel();

    result.setAttribute(toAttributeDomainEventModel(productAttribute.getAttribute()));
    result.setProductAttributeName(productAttribute.getProductAttributeName());
    result.setOwnByProductItem(productAttribute.isOwnByProductItem());
    result.setSequence(productAttribute.getSequence());
    result.setProductAttributeValues(toProductAttributeValueDomainEventModels(productAttribute.getProductAttributeValues()));

    return result;
  }

  private static AttributeDomainEventModel toAttributeDomainEventModel(Attribute attribute){
    try {
      return Optional
          .ofNullable(attribute)
          .map(AggregateUtil::createAttributeDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create AttributeDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static AttributeDomainEventModel createAttributeDomainEventModel(Attribute attribute){
    AttributeDomainEventModel result = new AttributeDomainEventModel();

    result.setName(attribute.getName());
    result.setAttributeCode(attribute.getAttributeCode());
    result.setAttributeType(toAttributeType(attribute));
    result.setDescription(attribute.getDescription());
    result.setSkuValue(attribute.isSkuValue());

    return result;
  }

  private static String toAttributeType(Attribute attribute) {
    return Optional.ofNullable(attribute)
                   .map(Attribute::getAttributeType)
                   .map(atrType -> atrType.toString())
                   .orElse(null);
  }

  private static List<ProductAttributeValueDomainEventModel> toProductAttributeValueDomainEventModels(
      List<ProductAttributeValue> productAttributeValues){
    try {
      return Optional
          .ofNullable(productAttributeValues)
          .map(AggregateUtil::createProductAttributeValueDomainEventModels)
          .orElse(Collections.emptyList());
    } catch (Exception e){
      log.error("Error while create ProductAttributeValueDomainEventModels with error message {}", e);
      return Collections.emptyList();
    }
  }

  private static List<ProductAttributeValueDomainEventModel> createProductAttributeValueDomainEventModels(
      List<ProductAttributeValue> productAttributeValues){
    return productAttributeValues
        .stream()
        .map(productAttributeValue -> toProductAttributeValueDomainEventModel(productAttributeValue))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static ProductAttributeValueDomainEventModel toProductAttributeValueDomainEventModel(
      ProductAttributeValue productAttributeValue){
    try {
      return Optional
          .ofNullable(productAttributeValue)
          .map(AggregateUtil::createProductAttributeValueDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create ProductAttributeValueDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ProductAttributeValueDomainEventModel createProductAttributeValueDomainEventModel(
      ProductAttributeValue productAttributeValue){
    ProductAttributeValueDomainEventModel result = new ProductAttributeValueDomainEventModel();

    result.setAllowedAttributeValue(toAllowedAttributeValueDomainEventModel(productAttributeValue.getAllowedAttributeValue()));
    result.setDescriptiveAttributeValue(productAttributeValue.getDescriptiveAttributeValue());
    result.setDescriptiveAttributeValueType(toDescriptiveAttributeValueType(productAttributeValue));
    result.setPredefinedAllowedAttributeValue(toPredefinedAllowedAttributeValueDomainEventModel(productAttributeValue.getPredefinedAllowedAttributeValue()));

    return result;
  }

  private static String toDescriptiveAttributeValueType(ProductAttributeValue productAttributeValue) {
    return Optional
        .ofNullable(productAttributeValue)
        .map(ProductAttributeValue::getDescriptiveAttributeValueType)
        .map(descAtrValType -> descAtrValType.toString())
        .orElse(null);
  }

  private static AllowedAttributeValueDomainEventModel toAllowedAttributeValueDomainEventModel(
      AllowedAttributeValue attributeValue){
    try {
      return Optional
          .ofNullable(attributeValue)
          .map(AggregateUtil::createAllowedAttributeValueDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create toAllowedAttributeValueDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static AllowedAttributeValueDomainEventModel createAllowedAttributeValueDomainEventModel(
      AllowedAttributeValue attributeValue){
    AllowedAttributeValueDomainEventModel result = new AllowedAttributeValueDomainEventModel();

    result.setAllowedAttributeCode(attributeValue.getAllowedAttributeCode());
    result.setValue(attributeValue.getValue());
    result.setSequence(attributeValue.getSequence());

    return result;
  }

  private static PredefinedAllowedAttributeValueDomainEventModel toPredefinedAllowedAttributeValueDomainEventModel(
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue){
    try {
      return Optional
          .ofNullable(predefinedAllowedAttributeValue)
          .map(AggregateUtil::createPredefinedAllowedAttributeValueDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create PredefinedAllowedAttributeValueDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static PredefinedAllowedAttributeValueDomainEventModel createPredefinedAllowedAttributeValueDomainEventModel(
      PredefinedAllowedAttributeValue predefinedAllowedAttributeValue){
    PredefinedAllowedAttributeValueDomainEventModel result = new PredefinedAllowedAttributeValueDomainEventModel();

    result.setPredefinedAllowedAttributeCode(predefinedAllowedAttributeValue.getPredefinedAllowedAttributeCode());
    result.setValue(predefinedAllowedAttributeValue.getValue());
    result.setSequence(predefinedAllowedAttributeValue.getSequence());

    return result;
  }

  /**
   * @param productImages
   * @return AggregateImageDomainEventModel from ProductImage
   */
  public static AggregateImageDomainEventModel toAggregateImageDomainEventModel(List<ProductImage> productImages) {
    try {
      return AggregateImageDomainEventModel
          .builder()
          .timestamp(System.currentTimeMillis())
          .productCode(toProductCodeFromProductImages(productImages))
          .images(toListImageDomainEventModel(productImages))
          .build();
    } catch (Exception e){
      log.error("Error while create AggregateImageDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static String toProductCodeFromProductImages(List<ProductImage> productImages) {
    return Optional
        .ofNullable(productImages)
        .map(vals -> vals.stream().findFirst().orElse(null))
        .map(ProductImage::getProduct)
        .map(Product::getProductCode)
        .orElse(null);
  }

  private static ImageDomainEventModel toImageDomainEventModel(ProductImage productImage) {
    try {
      return Optional
          .ofNullable(productImage)
          .map(AggregateUtil::createImageDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create ImageDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ImageDomainEventModel createImageDomainEventModel(ProductImage productImage) {
    ImageDomainEventModel result = new ImageDomainEventModel();

    result.setMainImage(productImage.isMainImages());
    result.setLocationPath(productImage.getLocationPath());
    result.setSequence(productImage.getSequence());

    return result;
  }

  /**
   * @param productItems
   * @return AggregateProductItemDomainEventModel from ProductItem
   */
  public static AggregateProductItemDomainEventModel toAggregateProductItemDomainEventModel(List<ProductItem> productItems) {
    try {
      return AggregateProductItemDomainEventModel
          .builder()
          .timestamp(System.currentTimeMillis())
          .productCode(toProductCodeFromProductItems(productItems))
          .productItems(toListProductItemDomainEventModel(productItems))
          .build();
    } catch (Exception e){
      log.error("Error while create AggregateProductItemDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static String toProductCodeFromProductItems(List<ProductItem> productItems) {
    return Optional
        .ofNullable(productItems)
        .map(vals -> vals.stream().findFirst().orElse(null))
        .map(ProductItem::getProduct)
        .map(Product::getProductCode)
        .orElse(null);
  }

  private static ProductItemDomainEventModel toProductItemDomainEventModel(ProductItem productItem) {
    try {
      return Optional
          .ofNullable(productItem)
          .map(AggregateUtil::createProductItemDomainEventModel)
          .orElse(null);
    } catch (Exception e){
      log.error("Error while create ProductItemDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ProductItemDomainEventModel createProductItemDomainEventModel(ProductItem productItem) {
    ProductItemDomainEventModel result = new ProductItemDomainEventModel();

    result.setGeneratedItemName(productItem.getGeneratedItemName());
    result.setUpcCode(productItem.getUpcCode());
    result.setSkuCode(productItem.getSkuCode());
    result.setActivated(productItem.isActivated());
    result.setViewable(productItem.isViewable());
    result.setImages(toImageDomainEventModels(productItem.getProductItemImages()));
    result.setDangerousGoodsLevel(productItem.getDangerousGoodsLevel());

    return result;
  }

  private static List<ImageDomainEventModel> toImageDomainEventModels(List<ProductItemImage> productItemImages){
    try {
      return Optional
          .ofNullable(productItemImages)
          .map(AggregateUtil::createImageDomainEventModels)
          .orElse(Collections.emptyList());
    } catch (Exception e) {
      log.error("Error while create ImageDomainEventModels with error message {}", e);
      return Collections.emptyList();
    }
  }

  private static List<ImageDomainEventModel> createImageDomainEventModels(List<ProductItemImage> productItemImages){
    return productItemImages
        .stream()
        .map(productItemImage -> toImageDomainEventModel(productItemImage))
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
  }

  private static ImageDomainEventModel toImageDomainEventModel(ProductItemImage productItemImage){
    try {
      return Optional
          .ofNullable(productItemImage)
          .map(AggregateUtil::createImageDomainEventModel)
          .orElse(null);
    } catch (Exception e) {
      log.error("Error while create ImageDomainEventModel with error message {}", e);
      return null;
    }
  }

  private static ImageDomainEventModel createImageDomainEventModel(ProductItemImage productItemImage){
    ImageDomainEventModel result = new ImageDomainEventModel();

    result.setMainImage(productItemImage.isMainImages());
    result.setLocationPath(productItemImage.getLocationPath());
    result.setSequence(productItemImage.getSequence());

    return result;
  }

}
