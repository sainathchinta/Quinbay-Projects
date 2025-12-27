package com.gdn.partners.pbp.service.productlevel3;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ItemPickupPointDto;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.ProductItemWholesalePrice;
import com.gdn.mta.product.repository.ProductItemWholesalePriceCustomRepository;
import com.gdn.mta.product.repository.ProductItemWholesalePriceRepository;
import com.gdn.mta.product.service.ErrorMessages;
import com.google.common.collect.Lists;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductItemWholesalePriceServiceBean implements ProductItemWholesalePriceService {

  @Autowired
  private ProductItemWholesalePriceRepository productItemWholesalePriceRepository;

  @Autowired
  private ProductItemWholesalePriceCustomRepository productItemWholesalePriceCustomRepository;

  @Value("${product.item.wholesale.price.fetch.batch.size}")
  private int itemWholesalePriceBatchSize;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void saveWholesalePrice(List<ProductItemWholesalePrice> productItemWholesalePrices) {
    productItemWholesalePriceRepository.saveAll(productItemWholesalePrices);
  }

  @Override
  public void saveWholesalePriceNew(List<ProductItemWholesalePrice> productItemWholesalePrices) {
    productItemWholesalePriceRepository.saveAll(productItemWholesalePrices);
  }

  @Override
  public List<ProductItemWholesalePrice> findByStoreIdAndItemSkus(String storeId, List<String> itemSkus) {
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    List<List<String>> itemSkuList = Lists.partition(itemSkus, itemWholesalePriceBatchSize);
    for (List<String> itemSkuSubList : itemSkuList) {
      List<ProductItemWholesalePrice> wholesalePrices =
          productItemWholesalePriceRepository.findByStoreIdAndItemSkuIn(storeId, itemSkuSubList);
      if (CollectionUtils.isNotEmpty(wholesalePrices)) {
        productItemWholesalePrices.addAll(wholesalePrices);
      }
    }
    return productItemWholesalePrices;
  }

  @Override
  public List<ProductItemWholesalePrice> findByStoreIdAndProductItemId(String storeId, List<String> productItemIds) {
    List<ProductItemWholesalePrice> productItemWholesalePrices = new ArrayList<>();
    List<List<String>> productItemIdsSubList = Lists.partition(productItemIds, itemWholesalePriceBatchSize);
    for (List<String> productItemIdSubList : productItemIdsSubList) {
      List<ProductItemWholesalePrice> wholesalePrices =
          productItemWholesalePriceRepository.findByStoreIdAndProductItemIdIn(storeId, productItemIdSubList);
      if (CollectionUtils.isNotEmpty(wholesalePrices)) {
        productItemWholesalePrices.addAll(wholesalePrices);
      }
    }
    return productItemWholesalePrices;
  }

  @Override
  public List<ProductItemWholesalePrice> findByStoreIdAndItemSkuAndPickupPointCode(String storeId,
      List<ItemPickupPointDto> itemPickupPointDtoList) {
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPointDtoList),
        ErrorMessages.ITEM_PICKUP_POINT_LIST_MUST_NOT_BE_EMPTY);
    return Optional.ofNullable(
        productItemWholesalePriceCustomRepository.findProductItemWholesalePriceByItemSkuAndPickupPointCode(storeId,
            itemPickupPointDtoList)).orElse(new ArrayList<>());
  }

  @Override
  public ProductItemWholesalePrice findOneByItemSkuAndPickupPointCode(String itemSku, String pickupPointCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(itemSku), ErrorMessages.SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(pickupPointCode),
      ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_NULL);
    return this.productItemWholesalePriceRepository.findFirstByItemSkuAndPickupPointCode(itemSku, pickupPointCode);
  }

  @Override
  public ProductItemWholesalePrice findOneByStoreIdAndItemSkuAndPickupPointCode(String storeId, String itemSku, String pickupPointCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(itemSku), ErrorMessages.SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(pickupPointCode),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_NULL);
    return this.productItemWholesalePriceRepository.findFirstByStoreIdAndItemSkuAndPickupPointCode(storeId,itemSku, pickupPointCode);
  }

}
