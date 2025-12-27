package com.gdn.x.productcategorybase.service.impl;

import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.time.DateUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductArchivalService;
import com.gdn.x.productcategorybase.service.ProductAttributeService;
import com.gdn.x.productcategorybase.service.ProductAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductDeletionService;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;
import com.gdn.x.productcategorybase.service.ProductItemAttributeValueService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductService;
import com.gdn.x.productcategorybase.service.SystemParameterService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductDeletionWrapperServiceImpl implements ProductDeletionWrapperService {

  @Autowired
  private ProductService productService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ProductArchivalService productArchivalService;

  @Autowired
  private ProductCategoryService productCategoryService;

  @Autowired
  private ImageService imageService;

  @Autowired
  private ProductItemService productItemService;

  @Autowired
  private ProductAttributeService productAttributeService;

  @Autowired
  private ProductAttributeValueService productAttributeValueService;

  @Autowired
  private ProductItemAttributeValueService productItemAttributeValueService;

  @Autowired
  private ProductDeletionService productDeletionService;

  @Override
  public void publishProductForDeletion(String storeId) {
    int batchSize = Integer.parseInt(
        systemParameterService.findByStoreIdAndVariable(storeId, Constants.BATCH_SIZE_TO_PUBLISH_REJECTED_PRODUCTS)
            .getValue());
    if (batchSize != 0) {
      int daysThreshold = Integer.parseInt(
          systemParameterService.findByStoreIdAndVariable(storeId, Constants.DAYS_THRESHOLD_FOR_PRODUCT_DELETION)
              .getValue());
      Date dateFilter = DateUtils.addDays(new Date(), -daysThreshold);
      List<String> productCodesToBeDeleted =
          productService.getListOfProductCodesEligibleForDeletion(storeId, dateFilter, batchSize);
      if (CollectionUtils.isNotEmpty(productCodesToBeDeleted)) {
        log.info("Product to be deleted : {} ", productCodesToBeDeleted);
        productService.updatePickedForDeletionFlagByProductCode(storeId, productCodesToBeDeleted, true);
        domainEventPublisherService.publishProductToBeDeleted(storeId, productCodesToBeDeleted);
        log.info("Published products to be delete");
      }
    }
  }

  @Override
  public void archiveAndDeleteProductData(String storeId, String productCode) {
    try {
      log.info("Archiving and deleting product : {} ", productCode);
      productArchivalService.copyProductDetailsToArchiveTablesAndHardDeleteProduct(
          getProductDataFromCache(storeId, productCode));
      domainEventPublisherService.publishDeletedProduct(storeId, productCode);
    } catch (Exception e) {
      log.error("Deleting product failed. productCode : {} ", productCode, e);
      productService.updatePickedForDeletionFlagByProductCode(storeId, Arrays.asList(productCode), false);
    }
  }

  @Override
  public Product getProductDetails(String storeId, String productCode) {
    Product product = productService.getProductByStoreIdAndProductCode(storeId, productCode);
    if (Objects.isNull(product)) {
      return null;
    }
    return setProductDataFromCache(storeId, product);
  }

  private Product getProductDataFromCache(String storeId, String productCode) {
    Product product = productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    return setProductDataFromCache(storeId, product);
  }

  private Product setProductDataFromCache(String storeId, Product product) {
    List<ProductItem> productItems =
        productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, product.getId());
    List<ProductImage> productImages =
        imageService.getProductImagesByStoreIdAndProductIdCached(storeId, product.getId());
    List<ProductCategory> productCategories =
        productCategoryService.getProductCategoriesByStoreIdAndProductIdCached(storeId, product.getId());
    List<ProductAttribute> productAttributes =
        productAttributeService.getProductAttributesByStoreIdAndProductId(storeId, product.getId());
    List<ProductAttributeValue> productAttributeValues;
    List<ProductItemAttributeValue> productItemAttributeValues;
    List<ProductItemImage> productItemImages;

    if (CollectionUtils.isNotEmpty(productAttributes)) {
      Set<String> productAttributeIds =
          productAttributes.stream().map(ProductAttribute::getId).collect(Collectors.toSet());
      productAttributeValues =
          productAttributeValueService.getProductAttributeValuesByStoreIdAndProductAttributeIds(storeId,
              productAttributeIds);
      if (CollectionUtils.isNotEmpty(productAttributeValues)) {
        Map<String, List<ProductAttributeValue>> productAttributeValuesMap = productAttributeValues.stream()
            .collect(Collectors.groupingBy(ProductAttributeValue::getProductAttributeId));
        productAttributes.forEach(productAttribute -> productAttribute.setProductAttributeValues(
            productAttributeValuesMap.get(productAttribute.getId())));
      }
    }

    if (CollectionUtils.isNotEmpty(productItems)) {
      List<String> productItemIds = productItems.stream().map(ProductItem::getId).collect(Collectors.toList());
      productItemAttributeValues =
          productItemAttributeValueService.getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(storeId,
              productItemIds);
      productItemImages = imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(storeId, productItemIds);

      if (CollectionUtils.isNotEmpty(productItemAttributeValues)) {
        Map<String, List<ProductItemAttributeValue>> productItemAttributeValuesMap = productItemAttributeValues.stream()
            .collect(Collectors.groupingBy(ProductItemAttributeValue::getProductItemId));
        productItems.forEach(productItem -> productItem.setProductItemAttributeValues(
            productItemAttributeValuesMap.get(productItem.getId())));
      }

      if (CollectionUtils.isNotEmpty(productItemImages)) {
        Map<String, List<ProductItemImage>> productItemImagesMap =
            productItemImages.stream().collect(Collectors.groupingBy(ProductItemImage::getProductItemId));
        productItems.forEach(
            productItem -> productItem.setProductItemImages(productItemImagesMap.get(productItem.getId())));
      }
    }

    product.setProductItems(productItems);
    product.setProductImages(productImages);
    product.setProductCategories(productCategories);
    product.setProductAttributes(productAttributes);

    return product;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void hardDeleteProductAndClearCache(String storeId, Product product) throws Exception {
    productDeletionService.deleteProductData(product);
    productDeletionService.hardDeleteProductAttributeExtracted(product.getProductCode());
    productService.evictAllProductDetailCache(product.getStoreId(), product);
  }
}
