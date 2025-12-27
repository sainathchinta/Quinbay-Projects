package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.ProductPublishUpdateDTO;
import com.gdn.x.productcategorybase.dto.request.ProductItemUpcCodeUpdateRequest;
import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductItemUomInfoService;
import com.gdn.x.productcategorybase.service.ProductService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.dto.request.UPCCodeSearchRequest;
import com.gdn.x.productcategorybase.dto.response.ItemImageResponse;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.service.CategoryService;
import com.gdn.x.productcategorybase.service.ImageService;
import com.gdn.x.productcategorybase.service.ProductCategoryService;
import com.gdn.x.productcategorybase.service.ProductItemService;
import com.gdn.x.productcategorybase.service.ProductItemServiceWrapper;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductItemServiceWrapperImpl implements ProductItemServiceWrapper{

  @Autowired
  private ProductItemService productItemService;

  @Autowired
  private CategoryService categoryService;

  @Autowired
  private ProductCategoryService productCategoryService;

  @Autowired
  @Lazy
  private ImageService imageService;

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private ProductItemUomInfoService productItemUomInfoService;

  @Value("${ranch.integration.enabled}")
  private boolean ranchIntegrationEnabled;

  @Value("${distribution.seller.list}")
  private Set<String> distributionSellerList;

  @Override
  public void setProductItemsWithProductItemImagesCached(String storeId, Product product, boolean includeMarkForDelete) {
    setProductItemsCached(storeId, product, includeMarkForDelete);
    setProductItemImages(storeId, product, includeMarkForDelete);
  }

  @Override
  public void setProductItemsWithProductItemImagesCachedWithoutFilteringMainImages(String storeId, Product product,
      boolean includeMarkForDelete) {
    setProductItemsCached(storeId, product, includeMarkForDelete);
    setProductItemImagesWithoutFilteringMainImages(storeId, product, includeMarkForDelete);
  }

  @Override
  public void setCompleteProductItemDetailsCached(String storeId, Product product, boolean includeMarkForDelete) {
    setProductItemsCached(storeId, product, includeMarkForDelete);
    setProductItemAttributeValues(storeId, product);
    setProductItemImages(storeId, product, includeMarkForDelete);
    if (ranchIntegrationEnabled && Optional.ofNullable(distributionSellerList).orElse(new HashSet<>())
        .contains(product.getCreatedMerchant())) {
      List<ProductItemUomInfo> productItemUomInfoList =
          productItemUomInfoService.findByStoreIdAndProductCodeAndMarkForDeleteFalse(storeId, product.getProductCode());
      Map<String, ProductItemUomInfo> productItemUomInfoMap =
          Optional.ofNullable(productItemUomInfoList).orElse(new ArrayList<>()).stream()
              .collect(Collectors.toMap(ProductItemUomInfo::getSkuCode, Function.identity()));
      for (ProductItem productItem : product.getProductItems()) {
        productItem.setProductItemUomInfo(productItemUomInfoMap.get(productItem.getSkuCode()));
      }
    }
  }

  @Override
  public void setProductItemsWithProductItemAttributeValuesAndAttributeCached(
      String storeId, Product product, boolean includeMarkForDelete) {
    setProductItemsCached(storeId, product, includeMarkForDelete);
    setProductItemAttributeValues(storeId, product);
  }

  @Override
  public void setProductItemsCached(String storeId, Product product, boolean includeMarkForDelete) {
    List<ProductItem> productItems = productItemService.getProductItemsByStoreIdAndProductIdCached(storeId, product.getId());
    if (!includeMarkForDelete) {
      productItems.removeIf(ProductItem::isMarkForDelete);
    }
    productItems.forEach(productItem -> productItem.setProduct(product));
    product.setProductItems(productItems);
  }

  @Override
  public Page<ProductItem> findByUPCCodeAndCategoryIds(
      String storeId, UPCCodeSearchRequest upcCodeSearchRequest, boolean isOnlyExternal, Integer page, Integer size) {
    log.info("Fetching items for upc code {} , category codes :  {} and store ID  : {}",
        upcCodeSearchRequest.getUpcCode(), upcCodeSearchRequest.getCategoryCodes(), storeId);
    List<String> categoryIds =
        categoryService.findIdsByStoreIdAndCategoryCodes(storeId, upcCodeSearchRequest.getCategoryCodes());
    upcCodeSearchRequest.setCategoryIds(categoryIds);
    List<String> productItemIds = productItemService.getProductItemsIdsByUpcCodeAndCategoryIds(
        upcCodeSearchRequest.getUpcCode(), isOnlyExternal, categoryIds);

    if (CollectionUtils.isEmpty(productItemIds)) {
      return new PageImpl<>(new ArrayList<>());
    }
    Pageable pageable = PageRequest.of(page, size);
    Page<ProductItem> productItems =
        productItemService.getProductItemsByStoreIdAndIds(storeId, productItemIds, pageable);
    for (ProductItem productItem : productItems) {
      productItemService.setProductCached(storeId, productItem);
      Hibernate.initialize(productItem.getProductItemImages());
    }
    return productItems;
  }

  private void setProductItemAttributeValues(String storeId, Product product) {
    Map<String, ProductItem> productItemIdProductItemMap = new HashMap<>();
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setProductItemAttributeValues(new ArrayList<>());
      productItemIdProductItemMap.put(productItem.getId(), productItem);
    }
    List<ProductItemAttributeValue> productItemAttributeValues =
        productItemService.getProductItemAttributeValuesWithAttributesCached(storeId, product);
    for (ProductItemAttributeValue productItemAttributeValue : productItemAttributeValues) {
      if (productItemIdProductItemMap.containsKey(productItemAttributeValue.getProductItemId())) {
        ProductItem productItem = productItemIdProductItemMap.get(productItemAttributeValue.getProductItemId());
        productItemAttributeValue.setProductItem(productItem);
        productItem.getProductItemAttributeValues().add(productItemAttributeValue);
      }
    }
  }

  private void setProductItemImages(String storeId, Product product, boolean includeMarkForDelete) {
    Map<String, ProductItem> productItemIdProductItemMap = new HashMap<>();
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setProductItemImages(new ArrayList<>());
      productItemIdProductItemMap.put(productItem.getId(), productItem);
    }
    List<ProductItemImage> productItemImages = productItemService.getProductItemImagesCached(storeId, product);
    for (ProductItemImage productItemImage : productItemImages) {
      if (productItemIdProductItemMap.containsKey(productItemImage.getProductItemId())) {
        ProductItem productItem = productItemIdProductItemMap.get(productItemImage.getProductItemId());
        productItemImage.setProductItem(productItem);
        productItem.getProductItemImages().add(productItemImage);
      }
    }
    for (ProductItem productItem : product.getProductItems()) {
      if (!includeMarkForDelete) {
        productItemService.removeDeletedProductItemImages(productItem);
      }
    }
  }

  @Override
  public void setProductItemImagesWithoutFilteringMainImages(String storeId, Product product,
    boolean includeMarkForDelete) {
    Map<String, ProductItem> productItemIdProductItemMap = new HashMap<>();
    for (ProductItem productItem : product.getProductItems()) {
      productItem.setProductItemImages(new ArrayList<>());
      productItemIdProductItemMap.put(productItem.getId(), productItem);
    }
    List<ProductItemImage> productItemImages =
        productItemService.getProductItemImagesCached(storeId, product);
    if (!includeMarkForDelete) {
      productItemImages.removeIf(ProductItemImage::isMarkForDelete);
      productItemImages.removeIf(
          productItemImage -> !productItemIdProductItemMap.keySet().contains(productItemImage.getProductItemId()));
    }
    for (ProductItemImage productItemImage : productItemImages) {
      ProductItem productItem = productItemIdProductItemMap.get(productItemImage.getProductItemId());
      productItemImage.setProductItem(productItem);
      productItem.getProductItemImages().add(productItemImage);
    }
    for (ProductItem productItem : product.getProductItems()) {
      if (!includeMarkForDelete) {
        productItemService.removeDeletedProductItemImagesWithoutFilteringMainImages(productItem);
      }
    }
  }

  @Override
  public List<ItemImageResponse> findProductItemImagesByItemCodes(String storeId, List<String> itemCodes, boolean removeOriginalImages,
    Boolean fetchImageResponse) {
    List<ProductItem> productItems = productItemService.findItemsBySkuCodesAndMarkForDeleteFalse(storeId, itemCodes);
    if(Objects.isNull(fetchImageResponse) || Boolean.TRUE.equals(fetchImageResponse)) {
      List<ProductItemImage> productItemImages =
        imageService.getDetachedProductItemImagesByStoreIdAndProductItemIds(storeId,
          productItems.stream().map(ProductItem::getId).distinct().collect(Collectors.toList()));
      Map<String, List<ProductItemImage>> productItemIdImagesMap = productItemImages.stream()
        .collect(Collectors.groupingBy(ProductItemImage::getProductItemId));
      for (ProductItem productItem : productItems) {
        productItem.setProductItemImages(
          productItemIdImagesMap.getOrDefault(productItem.getId(), new ArrayList<>()));
        productItemService.removeDeletedProductItemImages(productItem);
      }
      return ConverterUtil.convertProductItemToItemImageResponse(productItems, removeOriginalImages);
    }
    return ConverterUtil.convertProductItemToBasicItemDetailResponse(productItems);
  }

  @Override
  @Transactional(propagation = Propagation.REQUIRES_NEW)
  public void updateProductItemUpcCodeAndEvictCache(String storeId,
    List<ProductItemUpcCodeUpdateRequest> request, String productCode) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId),
      ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(productCode),
      ErrorMessage.PRODUCT_CODE_MUST_NOT_BE_BLANK.getMessage());
    List<ProductItem> updatedProductItems = productItemService.updateProductItemUpcCode(storeId, request, productCode);
    Product product =
      productService.getProductByStoreIdAndProductCodeCached(storeId, productCode);
    if (Objects.nonNull(product)) {
      productService.evictProductCache(storeId, product);
      applicationCacheServiceBean.evictProductItemsCacheByStoreIdAndProductId(storeId,
        product.getId());
      product.setProductItems(updatedProductItems);
      domainEventPublisherService.publishProductForEdit(new ProductPublishUpdateDTO(product, false,
          updatedProductItems.stream().map(ProductItem::getSkuCode).collect(Collectors.toSet())), null, false, false, false, true, false);
    }
  }


}

