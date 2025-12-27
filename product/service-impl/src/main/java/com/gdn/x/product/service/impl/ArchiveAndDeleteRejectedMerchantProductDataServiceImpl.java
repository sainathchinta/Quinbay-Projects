package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.gdn.x.product.service.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemArchive;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemPickupPointArchive;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.entity.ProductArchive;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.service.api.ArchiveAndDeleteRejectedMerchantProductDataService;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.CacheItemHelperService;
import com.gdn.x.product.service.api.ItemArchiveService;
import com.gdn.x.product.service.api.ItemPickupPointArchiveService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductArchiveService;
import com.gdn.x.product.service.api.ProductCacheableService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ArchiveAndDeleteRejectedMerchantProductDataServiceImpl
    implements ArchiveAndDeleteRejectedMerchantProductDataService {

  @Autowired
  @Lazy
  private ProductService productService;

  @Autowired
  private ProductCacheableService productCacheableService;

  @Autowired
  private CacheItemHelperService cacheItemHelperService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ProductArchiveService productArchiveService;

  @Autowired
  private ItemArchiveService itemArchiveService;

  @Autowired
  private ItemPickupPointArchiveService itemPickupPointArchiveService;

  @Autowired
  @Lazy
  private ItemService itemService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  @Lazy
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Value("${delete.l3.terminated.seller.switch}")
  private boolean deleteL3ForTerminatedSellers;

  @Override
  public void archiveAndDeleteRejectedMerchantProductData(ProductCodeDomainEventModel productCodeDomainEventModel)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCodeDomainEventModel.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productCodeDomainEventModel.getProductCode()),
        ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_BLANK);
    try {
      archiveAndDeleteAndCacheEvictProductData(productCodeDomainEventModel);
    } catch (Exception e) {
      log.error(
          "Error while deleting rejected merchant product. event : {} , productCodeDomainEventModel : {} , Error - ",
          ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT, productCodeDomainEventModel, e);
      ProductRetryEventPublish productRetryEventPublish =
          ProductRetryEventPublish.builder().topicName(ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT)
              .identifier(objectMapper.writeValueAsString(productCodeDomainEventModel))
              .retryPublishStatus(RetryPublishStatus.PENDING).clearCache(Boolean.FALSE).retryCount(0).build();
      productRetryEventPublishService.insertToRetryPublish(productRetryEventPublish);
    }
  }

  @Override
  public void archiveAndDeleteRejectedMerchantProductDataRetry(ProductCodeDomainEventModel productCodeDomainEventModel)
      throws Exception {
    archiveAndDeleteAndCacheEvictProductData(productCodeDomainEventModel);
  }

  private void archiveAndDeleteAndCacheEvictProductData(ProductCodeDomainEventModel productCodeDomainEventModel) {
    List<Product> products = productService.findByStoreIdAndProductCode(productCodeDomainEventModel.getStoreId(),
        productCodeDomainEventModel.getProductCode());
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(products),
        String.format(ErrorMessages.PRODUCT_NOT_FOUND_ERROR, productCodeDomainEventModel.getProductCode()));
    List<Product> productsToDelete = new ArrayList<>();
    List<Item> itemsToDelete = new ArrayList<>();
    List<ItemPickupPoint> itemPickupPointsToDelete = new ArrayList<>();
    Set<String> productSkusToDelete = new HashSet<>();
    for (Product product : products) {
      productSkusToDelete.add(product.getProductSku());
      Product productToDelete =
          productCacheableService.findProductByStoreIdAndProductSku(product.getStoreId(), product.getProductSku());
      productsToDelete.add(productToDelete);
      itemsToDelete =
          cacheItemHelperService.findCacheableByStoreIdAndProductSku(product.getStoreId(), product.getProductSku());
      if (CollectionUtils.isNotEmpty(itemsToDelete)) {
        itemPickupPointsToDelete =
            itemPickupPointService.findByStoreIdAndProductSku(product.getStoreId(), product.getProductSku());
      }
    }
    addItemPickupPointToArchiveAndDeleteAndEvictCache(itemsToDelete, itemPickupPointsToDelete, productSkusToDelete);
    addItemToItemArchiveAndDeleteAndEvictCache(itemsToDelete, productSkusToDelete);
    addProductToProductArchiveAndDeleteAndEvictCache(productsToDelete, productSkusToDelete);
    evictCacheProductAndItemAndItemPickupPoint(itemsToDelete, itemPickupPointsToDelete, productsToDelete);
    if (deleteL3ForTerminatedSellers) {
      for (Product product : products) {
        log.info("Publishing event:  {}, for deleting the l3 solr document of the product-sku : {} ",
            ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku());
        ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
        productAndItemEventModel.setProductSku(product.getProductSku());
        productAndItemEventModel.setRejected(true);
        productAndItemEventModel.setMerchantCode(product.getMerchantCode());
        kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), productAndItemEventModel);
      }
    }
  }

  private void addItemPickupPointToArchiveAndDeleteAndEvictCache(List<Item> itemsToDelete,
      List<ItemPickupPoint> itemPickupPointsToDelete, Set<String> productSkusToDelete) {
    if (CollectionUtils.isNotEmpty(itemPickupPointsToDelete)) {
      List<ItemPickupPointArchive> itemPickupPointsToArchive = new ArrayList<>();
      itemPickupPointsToDelete.stream().forEach(itemPickupPoint -> {
        ItemPickupPointArchive itemPickupPointToArchive = new ItemPickupPointArchive();
        BeanUtils.copyProperties(itemPickupPoint, itemPickupPointToArchive, "id", "version");
        itemPickupPointsToArchive.add(itemPickupPointToArchive);
      });
      itemPickupPointArchiveService.addItemPickupPointsToItemPickupPointArchive(itemPickupPointsToArchive);
      itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(itemPickupPointsToDelete.get(0).getStoreId(),
          productSkusToDelete);
    }
  }

  private void addItemToItemArchiveAndDeleteAndEvictCache(List<Item> itemsToDelete, Set<String> productSkusToDelete) {
    if (CollectionUtils.isNotEmpty(itemsToDelete)) {
      List<ItemArchive> itemsToArchive = new ArrayList<>();
      itemsToDelete.stream().forEach(item -> {
        ItemArchive itemToArchive = new ItemArchive();
        BeanUtils.copyProperties(item, itemToArchive, "id", "version");
        itemsToArchive.add(itemToArchive);
      });
      itemArchiveService.addItemsToItemArchive(itemsToArchive);
      itemService.deleteItemByStoreIdAndProductSkus(itemsToDelete.get(0).getStoreId(), productSkusToDelete);
    }
  }

  private void addProductToProductArchiveAndDeleteAndEvictCache(List<Product> productsToDelete,
      Set<String> productSkusToDelete) {
    if (CollectionUtils.isNotEmpty(productsToDelete)) {
      List<ProductArchive> productsToArchive = new ArrayList<>();
      productsToDelete.stream().forEach(product -> {
        ProductArchive productToArchive = new ProductArchive();
        BeanUtils.copyProperties(product, productToArchive, "id", "version");
        productsToArchive.add(productToArchive);
      });
      productArchiveService.addProductsToProductArchive(productsToArchive);
      productService.deleteProductByStoreIdAndProductSkus(productsToDelete.get(0).getStoreId(), productSkusToDelete);
    }
  }

  private void evictCacheProductAndItemAndItemPickupPoint(List<Item> itemsToDelete,
      List<ItemPickupPoint> itemPickupPointsToDelete, List<Product> productsToDelete) {
    if (CollectionUtils.isNotEmpty(itemPickupPointsToDelete)) {
      cacheEvictHelperService.evictItemPickupPointCache(itemPickupPointsToDelete.get(0).getStoreId(), itemsToDelete,
          itemPickupPointsToDelete);
    }
    if (CollectionUtils.isNotEmpty(itemsToDelete)) {
      itemsToDelete.stream().forEach(item -> cacheEvictHelperService.evictItemData(item.getStoreId(), item));
    }
    productsToDelete.stream()
        .forEach(product -> cacheEvictHelperService.evictProductData(product.getStoreId(), product));
  }
}
