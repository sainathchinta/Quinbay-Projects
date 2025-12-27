package com.gdn.x.productcategorybase.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.commons.collections.CollectionUtils;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuCleanupStatusEventModel;
import com.gdn.x.productcategorybase.domain.event.model.TerminatedSellerSkuImageCleanupEventModel;
import com.gdn.x.productcategorybase.dto.TerminatedSellerSkuCleanupStatusDTO;
import com.gdn.x.productcategorybase.entity.Product;
import com.gdn.x.productcategorybase.entity.ProductAttribute;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductCategory;
import com.gdn.x.productcategorybase.entity.ProductImage;
import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductItemImage;
import com.gdn.x.productcategorybase.repository.ProductAttributeExtractedRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductAttributeValueRepository;
import com.gdn.x.productcategorybase.repository.ProductCategoryRepository;
import com.gdn.x.productcategorybase.repository.ProductImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemAttributeRepository;
import com.gdn.x.productcategorybase.repository.ProductItemImageRepository;
import com.gdn.x.productcategorybase.repository.ProductItemRepository;
import com.gdn.x.productcategorybase.repository.ProductRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.ProductDeletionService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@Transactional(readOnly = true)
public class ProductDeletionServiceImpl implements ProductDeletionService {

  @Autowired
  private ProductRepository productRepository;

  @Autowired
  private ProductItemRepository productItemRepository;

  @Autowired
  private ProductAttributeRepository productAttributeRepository;

  @Autowired
  private ProductAttributeValueRepository productAttributeValueRepository;

  @Autowired
  private ProductItemAttributeRepository productItemAttributeRepository;

  @Autowired
  private ProductCategoryRepository productCategoryRepository;

  @Autowired
  private ProductImageRepository productImageRepository;

  @Autowired
  private ProductItemImageRepository productItemImageRepository;

  @Autowired
  private ProductAttributeExtractedRepository productAttributeExtractedRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Value("${terminated.seller.sku.picked.for.deletion.threshold.in.minutes}")
  private long terminatedSellerSkuPickedForDeletionThresholdInMinutes;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void deleteProductData(Product product) throws Exception {
    String productId = product.getId();
    List<String> productImageIds =
        Optional.ofNullable(product.getProductImages()).orElseGet(ArrayList::new).stream().map(ProductImage::getId)
            .collect(Collectors.toList());
    List<String> productCategoryIds =
        Optional.ofNullable(product.getProductCategories()).orElseGet(ArrayList::new).stream()
            .map(ProductCategory::getId).collect(Collectors.toList());
    List<String> productAttributeIds =
        Optional.ofNullable(product.getProductAttributes()).orElseGet(ArrayList::new).stream()
            .map(ProductAttribute::getId).collect(Collectors.toList());
    List<String> productAttributeValueIds =
        Optional.ofNullable(product.getProductAttributes()).orElseGet(ArrayList::new).stream()
            .filter(productAttribute -> CollectionUtils.isNotEmpty(productAttribute.getProductAttributeValues()))
            .map(ProductAttribute::getProductAttributeValues).flatMap(List::stream).map(ProductAttributeValue::getId)
            .collect(Collectors.toList());
    List<String> productItemIds =
        Optional.ofNullable(product.getProductItems()).orElseGet(ArrayList::new).stream().map(ProductItem::getId)
            .collect(Collectors.toList());
    List<String> productItemImageIds = Optional.ofNullable(product.getProductItems()).orElseGet(ArrayList::new).stream()
        .filter(productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemImages()))
        .map(ProductItem::getProductItemImages).flatMap(List::stream).map(ProductItemImage::getId)
        .collect(Collectors.toList());
    List<String> productItemAttributeIds =
        Optional.ofNullable(product.getProductItems()).orElseGet(ArrayList::new).stream()
            .filter(productItem -> CollectionUtils.isNotEmpty(productItem.getProductItemAttributeValues()))
            .map(ProductItem::getProductItemAttributeValues).flatMap(List::stream).map(ProductItemAttributeValue::getId)
            .collect(Collectors.toList());

    if (CollectionUtils.isNotEmpty(productCategoryIds)) {
      productCategoryRepository.deleteByIds(productCategoryIds);
    }

    if (CollectionUtils.isNotEmpty(productImageIds)) {
      productImageRepository.deleteByIds(productImageIds);
    }

    if (CollectionUtils.isNotEmpty(productAttributeValueIds)) {
      productAttributeValueRepository.deleteByIds(productAttributeValueIds);
    }

    if (CollectionUtils.isNotEmpty(productAttributeIds)) {
      productAttributeRepository.deleteByIds(productAttributeIds);
    }

    if (CollectionUtils.isNotEmpty(productItemAttributeIds)) {
      productItemAttributeRepository.deleteByIds(productItemAttributeIds);
    }

    if (CollectionUtils.isNotEmpty(productItemImageIds)) {
      productItemImageRepository.deleteByIds(productItemImageIds);
    }

    if (CollectionUtils.isNotEmpty(productItemIds)) {
      productItemRepository.deleteByIds(productItemIds);
    }

    productRepository.deleteByIds(Arrays.asList(productId));
  }

  @Override
  public void hardDeleteProductAttributeExtracted(String productCode) {
    productAttributeExtractedRepository.deleteByProductCode(productCode);
  }

  @Override
  public boolean pickedForDeletion(Product product) {
    if (!product.isPickedForDeletion()) {
      return false;
    }

    Date updatedDate = Optional.ofNullable(product.getUpdatedDate()).orElse(new Date());
    long timeDifferenceMillis = new Date().getTime() - updatedDate.getTime();
    long minutesDifference = timeDifferenceMillis / (Constants.SECONDS * Constants.MILLI_SECONDS);
    return minutesDifference < terminatedSellerSkuPickedForDeletionThresholdInMinutes;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public void updatePickedForDeletionFlag(Product product, boolean pickedForDeletion) {
    product.setPickedForDeletion(true);
    productRepository.save(product);
  }

  @Override
  public boolean isAllowedToDeleteImage(List<ProductItem> productItems) {
    if (CollectionUtils.isEmpty(productItems)) {
      return false;
    }

    boolean hasItemWithSourceItemCode = productItems.stream()
        .anyMatch(item -> Objects.nonNull(item.getSourceItemCode()));

    if (!hasItemWithSourceItemCode) {
      ProductItem anyItemWithCurrentItemAsSource = productItemRepository.findFirstBySourceItemCodeIn(
          productItems.stream().map(ProductItem::getSkuCode).collect(Collectors.toList()));
      return  Objects.isNull(anyItemWithCurrentItemAsSource);
    }
    return false;
  }

  @Override
  public void publishEventToUpdateStatusAndDeleteImage(TerminatedSellerSkuCleanupStatusDTO statusDTO) {
    TerminatedSellerSkuCleanupStatusEventModel statusEventModel =
        new TerminatedSellerSkuCleanupStatusEventModel();
    BeanUtils.copyProperties(statusDTO, statusEventModel);
    domainEventPublisherService.publishTerminatedSellerSkuCleanupStatusEvent(statusEventModel);

    if (statusDTO.isPublishImageDeletionEvent()) {
      TerminatedSellerSkuImageCleanupEventModel imageCleanupEventModel =
          new TerminatedSellerSkuImageCleanupEventModel();
      BeanUtils.copyProperties(statusDTO, imageCleanupEventModel);
      domainEventPublisherService.publishTerminatedSellerSkuImageCleanupEvent(imageCleanupEventModel);
    }
  }

}
