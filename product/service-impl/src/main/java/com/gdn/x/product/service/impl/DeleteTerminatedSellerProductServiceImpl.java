package com.gdn.x.product.service.impl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import com.gdn.x.product.domain.event.model.ItemEventModel;
import com.gdn.x.product.service.config.KafkaPublisher;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductEventModel;
import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductStatusEventModel;
import com.gdn.x.product.domain.event.model.ProductAndItemEventModel;
import com.gdn.x.product.enums.DeleteTerminatedSellerProductStatus;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Product;
import com.gdn.x.product.model.vo.ProductCollectionsVo;
import com.gdn.x.product.service.api.CacheEvictHelperService;
import com.gdn.x.product.service.api.DeleteTerminatedSellerProductService;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class DeleteTerminatedSellerProductServiceImpl implements DeleteTerminatedSellerProductService {

  @Autowired
  private ProductService productService;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private CacheEvictHelperService cacheEvictHelperService;

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${delete.terminated.seller.product.x.product.service.name}")
  private String deleteTerminatedSellerProductXProductServiceName;

  @Value("${delete.terminated.seller.picked.for.deletion.threshold.in.minutes}")
  private long deleteTerminatedSellerPickedForDeletionThresholdInMinutes;


  @Override
  public void deleteTerminatedSellerProductData(
      DeleteTerminatedSellerProductEventModel deleteTerminatedSellerProductEventModel) {
    try {
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(deleteTerminatedSellerProductEventModel.getProductCode()),
          ErrorMessages.PRODUCT_CODE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(deleteTerminatedSellerProductEventModel.getSellerCode()),
          ErrorMessages.MERCHANT_CODE_MUST_NOT_BE_BLANK);
      deleteProductAndItemAndItemPickupPoint(deleteTerminatedSellerProductEventModel.getProductCode(),
          deleteTerminatedSellerProductEventModel.getSellerCode());
      publishDeleteTerminatedSellerProductEvent(deleteTerminatedSellerProductEventModel,
          DeleteTerminatedSellerProductStatus.SUCCESS);
    } catch (Exception e) {
      log.error("Error while deleting terminatedSellerProduct. deleteTerminatedSellerProductEventModel : {} ",
          deleteTerminatedSellerProductEventModel, e);
      publishDeleteTerminatedSellerProductEvent(deleteTerminatedSellerProductEventModel,
          DeleteTerminatedSellerProductStatus.FAILED);
    }
  }


  private void deleteProductAndItemAndItemPickupPoint(String productCode, String sellerCode) {
    Product product = productService.findByProductCodeAndSellerCode(productCode, sellerCode);
    if (isEligibleToDeleteTheProduct(product)) {
      deleteProductAndItemAndItemPickupPoint(productCode, sellerCode, product);
    }
  }

  private boolean isEligibleToDeleteTheProduct(Product product) {
    if (Objects.nonNull(product)) {
      if (product.isPickedForDeletion()) {
        return convertTimeDifferenceToMinutes(product.getUpdatedDate())
            > deleteTerminatedSellerPickedForDeletionThresholdInMinutes;
      } else {
        return true;
      }
    } else {
      return false;
    }
  }

  private long convertTimeDifferenceToMinutes(Date pickedForDeletionTimeStamp) {
    pickedForDeletionTimeStamp = Optional.ofNullable(pickedForDeletionTimeStamp).orElse(new Date());
    Date currentTimeStamp = new Date();
    long differenceInMs = currentTimeStamp.getTime() - pickedForDeletionTimeStamp.getTime();
    return differenceInMs / (60 * 1000);
  }

  private void deleteProductAndItemAndItemPickupPoint(String productCode, String sellerCode, Product product) {
    boolean pickedForDeletionUpdated = false;
    boolean productDeleted = false;
    ProductCollectionsVo deletedData = new ProductCollectionsVo();
    try {
      updatePickedForDeletionInProduct(product, true);
      pickedForDeletionUpdated = true;

      //Delete from all collection
      deletedData =
          deleteProductDataFromAllCollectionsAndClearCache(product);
      productDeleted = true;

      //clear cache
      evictCacheProductAndItemAndItemPickupPoint(product.getStoreId(), deletedData);

      //delete from solr
      deleteProductFromSolr(product, deletedData);

    } catch (Exception ex) {
      log.error("Error while deleting terminatedSellerProduct. productCode : {} , sellerCode : {} ", productCode,
          sellerCode, ex);

      //if there is any error and we have update pickedForDeletionFlag the we need to revert it if product is not deleted. It will helpful when doing retry
      if (pickedForDeletionUpdated && !productDeleted) {
        updatePickedForDeletionInProduct(product, false);
      }

      //re throw the error only when product is not deleted. If product is deleted means data from all collection is deleted. Otherwise we can remove it from solr as well
      if (productDeleted) {
        deleteProductFromSolr(product, deletedData);
      } else {
        throw ex;
      }
    }
  }

  private ProductCollectionsVo deleteProductDataFromAllCollectionsAndClearCache(
      Product product) {

    //delete item pickup points
    List<ItemPickupPoint> itemPickupPointsDeleted =
        itemPickupPointService.deleteItemPickupPointByStoreIdAndProductSkus(product.getStoreId(),
            Set.of(product.getProductSku()));

    //delete items
    List<Item> itemsDeleted =
        itemService.deleteItemByStoreIdAndProductSkus(product.getStoreId(), Set.of(product.getProductSku()));

    //delete product
    List<Product> productsDeleted =
        productService.deleteProductByStoreIdAndProductSkus(product.getStoreId(), Set.of(product.getProductSku()));


    return ProductCollectionsVo.builder().itemPickupPoints(itemPickupPointsDeleted).items(itemsDeleted)
        .products(productsDeleted).build();
  }

  private void evictCacheProductAndItemAndItemPickupPoint(String storeId, ProductCollectionsVo productCollectionsVo) {

    // clear item pickup point cache
    if (CollectionUtils.isNotEmpty(productCollectionsVo.getItemPickupPoints())) {
      cacheEvictHelperService.evictItemPickupPointCache(storeId, productCollectionsVo.getItems(),
          productCollectionsVo.getItemPickupPoints());
    }

    //clear item cache
    if (CollectionUtils.isNotEmpty(productCollectionsVo.getItems())) {
      for (Item item : productCollectionsVo.getItems()) {
        cacheEvictHelperService.evictItemData(storeId, item);
      }
    }

    // clear product cache
    if (CollectionUtils.isNotEmpty(productCollectionsVo.getProducts())) {
      for (Product product : productCollectionsVo.getProducts()) {
        cacheEvictHelperService.evictProductData(storeId, product);
      }
    }

  }

  private Product updatePickedForDeletionInProduct(Product product, boolean pickedForDeletion) {
    product.setPickedForDeletion(pickedForDeletion);
    return productService.saveProductWithoutUpdatingSolr(product, Collections.EMPTY_LIST, StringUtils.EMPTY);
  }

  private void deleteProductFromSolr(Product product, ProductCollectionsVo productCollectionsVo) {
    ProductAndItemEventModel productAndItemEventModel = new ProductAndItemEventModel();
    productAndItemEventModel.setProductSku(product.getProductSku());
    productAndItemEventModel.setRejected(true);
    productAndItemEventModel.setMerchantCode(product.getMerchantCode());
    productAndItemEventModel.setItems(generateItemEventModel(productCollectionsVo));
    log.info("Publishing event topic : {} , payload : {} ", ProductDomainEventName.UPDATE_TO_SOLR,
        productAndItemEventModel);
    kafkaPublisher.send(ProductDomainEventName.UPDATE_TO_SOLR, product.getProductSku(), productAndItemEventModel);
  }


  private List<ItemEventModel> generateItemEventModel(ProductCollectionsVo productCollectionsVo) {
    List<ItemEventModel> itemEventModels = new ArrayList<>();
    for (Item item : productCollectionsVo.getItems()) {
      ItemEventModel itemEventModel = new ItemEventModel();
      BeanUtils.copyProperties(item, itemEventModel);
      itemEventModels.add(itemEventModel);
    }
    return itemEventModels;
  }

  private void publishDeleteTerminatedSellerProductEvent(
      DeleteTerminatedSellerProductEventModel deleteTerminatedSellerProductEventModel,
      DeleteTerminatedSellerProductStatus deleteTerminatedSellerProductStatus) {
    DeleteTerminatedSellerProductStatusEventModel deleteTerminatedSellerProductStatusEventModel =
        DeleteTerminatedSellerProductStatusEventModel.builder()
            .productCode(deleteTerminatedSellerProductEventModel.getProductCode())
            .sellerCode(deleteTerminatedSellerProductEventModel.getSellerCode())
            .service(deleteTerminatedSellerProductXProductServiceName)
            .result(deleteTerminatedSellerProductStatus.name()).build();
    log.info("Publishing event topic : {} , payload : {} ",
        kafkaTopicProperties.getDeleteTerminatedSellerProductStatusEvent(),
        deleteTerminatedSellerProductStatusEventModel);
    kafkaPublisher.send(kafkaTopicProperties.getDeleteTerminatedSellerProductStatusEvent(),
        deleteTerminatedSellerProductEventModel.getProductCode(), deleteTerminatedSellerProductStatusEventModel);
  }

}
