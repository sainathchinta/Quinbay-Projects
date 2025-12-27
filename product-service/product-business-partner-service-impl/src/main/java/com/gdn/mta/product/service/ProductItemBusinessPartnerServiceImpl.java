package com.gdn.mta.product.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gda.mta.product.dto.DeletedProductItems;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.MapUtils;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerCustomRepository;
import com.gdn.mta.product.repository.ProductItemBusinessPartnerRepository;
import com.gdn.partners.pbp.commons.constants.Constants;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@Transactional(readOnly = true)
public class ProductItemBusinessPartnerServiceImpl implements ProductItemBusinessPartnerService {

  @Autowired
  private ProductItemBusinessPartnerRepository productItemBusinessPartnerRepository;

  @Autowired
  private ProductItemBusinessPartnerCustomRepository productItemBusinessPartnerCustomRepository;

  @Value("${delete.extra.variants.from.x-product}")
  private boolean deleteExtraVariantsFromXproduct;

  @Override
  public Map<String, String> getItemSkuAndItemCodeMappingUsingItemIds(String storeId,
      Map<String, String> itemIdAndSkuCodeMap) {
    Map<String, String> itemSkuAndItemCodeMap = new HashMap<>();
    if (MapUtils.isNotEmpty(itemIdAndSkuCodeMap)) {
      List<ProductItemBusinessPartner> productItemBusinessPartnerList =
          productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdIn(storeId, itemIdAndSkuCodeMap.keySet());
      if (CollectionUtils.isNotEmpty(productItemBusinessPartnerList)) {
        for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList) {
          itemSkuAndItemCodeMap.put(productItemBusinessPartner.getGdnProductItemSku(),
              itemIdAndSkuCodeMap.getOrDefault(productItemBusinessPartner.getProductItemId(), StringUtils.EMPTY));
        }
      }
    }
    return itemSkuAndItemCodeMap;
  }

  @Override
  public String findReviewPendingProductCodeByItemSku(String itemSku) {
    return productItemBusinessPartnerRepository.findProductCodeByItemSku(itemSku);
  }

  @Override
  public Map<String, String> getSkuCodesAndItemSkusMap(String storeId, Map<String, String> skuCodesAndProductItemIdsMap){
    Map<String, String> skuCodesAndItemSkusMap = new HashMap<>();
    if(Objects.nonNull(skuCodesAndProductItemIdsMap)) {
      List<ProductItemBusinessPartner> productItemBusinessPartnerList =
          productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndProductItemIdIn(
              storeId, new ArrayList<>(skuCodesAndProductItemIdsMap.values()));
      if (CollectionUtils.isNotEmpty(productItemBusinessPartnerList)) {
        Map<String, String> productItemIdsAndItemSkuMap = new HashMap<>();
        for (ProductItemBusinessPartner productItemBusinessPartner : productItemBusinessPartnerList) {
          productItemIdsAndItemSkuMap.put(productItemBusinessPartner.getProductItemId(),
              productItemBusinessPartner.getGdnProductItemSku());
        }
        for (String skuCode : skuCodesAndProductItemIdsMap.keySet()) {
          String productItemId = skuCodesAndProductItemIdsMap.get(skuCode);
          String itemSku = productItemIdsAndItemSkuMap.getOrDefault(productItemId, null);
          skuCodesAndItemSkusMap.put(skuCode, itemSku);
        }
      }
    }
    return skuCodesAndItemSkusMap;
  }

  @Override
  public List<ProductItemBusinessPartner> findProductItemByProductItemId(String storeId, String productItemId) {
    return productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdAndMarkForDeleteFalse(storeId, productItemId);
  }

  @Override
  public List<ProductItemBusinessPartner> findProductItemByProductItemIdIn(String storeId, List<String> productItemIds) {
    return productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdIn(storeId, productItemIds);
  }

  @Override
  public ProductItemBusinessPartner findProductItemByItemSku(String storeId, String itemSku) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), Constants.STORE_ID_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), Constants.ITEM_SKU_MUST_NOT_BE_EMPTY);
    return productItemBusinessPartnerRepository.findByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(storeId, itemSku);
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class, propagation = Propagation.REQUIRES_NEW)
  public void updateMerchantSkuByStoreIdAndItemSku(String storeId, String itemSku, String merchantSku) throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), Constants.STORE_ID_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), Constants.ITEM_SKU_MUST_NOT_BE_EMPTY);
    merchantSku = StringUtils.defaultString(merchantSku, StringUtils.EMPTY);
    productItemBusinessPartnerRepository.updateMerchantSkuByStoreIdAndItemSku(storeId, itemSku, merchantSku);
  }

  @Override
  public Page<ProductItemBusinessPartner> getProductItemBusinessPartnerForL5Listing(String storeId,
      String productBusinessPartnerId, int page, int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(productBusinessPartnerId),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_EMPTY);
    return productItemBusinessPartnerCustomRepository.findByStoreIdAndProductBusinessPartnerIdAndGdnProductItemSkuAndPickupPointIdIn(storeId,
        productBusinessPartnerId, itemPickupPointListingL3Request.getItemSku(),
        itemPickupPointListingL3Request.getPickupPointCodes(), PageRequest.of(page, size),
      itemPickupPointListingL3Request.isFbbSortRequired());
  }

  @Override
  public List<ProductItemBusinessPartner> getProductItemBusinessPartnerByItemSkuList(String storeId,
      List<String> itemSkuList) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(storeId), ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemSkuList),
        ErrorMessages.ITEM_SKU_LIST_MUST_NOT_BE_EMPTY);
    List<ProductItemBusinessPartner> productItemsBusinessPartnerList =
        productItemBusinessPartnerRepository.findByStoreIdAndMarkForDeleteFalseAndGdnProductItemSkuIn(storeId,
            itemSkuList);
    return productItemsBusinessPartnerList;
  }

  @Override
  public int getProductItemCountByItemSku(String storeId, String itemSku) {
    return productItemBusinessPartnerRepository
      .findFirst2ByStoreIdAndGdnProductItemSkuAndMarkForDeleteFalse(storeId, itemSku).size();
  }

  @Override
  public void deleteItemsPickupPointDelete(String storeId, String pickupPointCode,
    List<String> itemSku) {
    productItemBusinessPartnerRepository.updateMarkForDeleteByStoreIdAndItemSkuInAndPickupPointId(
      MDC.get(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER), storeId, itemSku,
      pickupPointCode);
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void saveAll(List<ProductItemBusinessPartner> productItemBusinessPartnerList) {
    productItemBusinessPartnerRepository.saveAll(productItemBusinessPartnerList);
  }

  @Override
  public List<ProductItemBusinessPartner> replaceNewWithExistingProductItemBusinessPartner(
      List<ProductItemBusinessPartner> existingProductItemBusinessPartners, List<ProductItemBusinessPartner>
      newProductItemBusinessPartners) {
    if (CollectionUtils.isNotEmpty(newProductItemBusinessPartners)) {
      Map<String, ProductItemBusinessPartner> idAndProductItemBusinessPartnerMap =
          Optional.ofNullable(existingProductItemBusinessPartners).orElse(new ArrayList<>())
              .stream().collect(Collectors.toMap(ProductItemBusinessPartner::getId,
                  Function.identity(), (oldValue, newValue) -> oldValue));
      return newProductItemBusinessPartners.stream().map(productItemBusinessPartner ->
          idAndProductItemBusinessPartnerMap.getOrDefault(productItemBusinessPartner.getId(),
          productItemBusinessPartner)).collect(Collectors.toList());
      }
    return new ArrayList<>();
    }

  @Override
  public List<ProductItemBusinessPartner> mergeProductItemBusinessPartnerLists(
      List<ProductItemBusinessPartner> existingUpdatedProductItemBusinessPartners,
      List<ProductItemBusinessPartner> newlyUpdatedProductItemBusinessPartners) {
    Map<String, ProductItemBusinessPartner> idAndProductItemBusinessPartnerMap =
        Optional.ofNullable(newlyUpdatedProductItemBusinessPartners).orElse(new ArrayList<>()).stream()
            .collect(Collectors.toMap(ProductItemBusinessPartner::getId,
                Function.identity(), (oldValue, newValue) -> oldValue));
    List<ProductItemBusinessPartner> filteredExistingList =
        Optional.ofNullable(existingUpdatedProductItemBusinessPartners).orElse(new ArrayList<>()).stream()
            .filter(productItemBusinessPartner -> !idAndProductItemBusinessPartnerMap.containsKey(
                productItemBusinessPartner.getId())).collect(Collectors.toList());
    List<ProductItemBusinessPartner> mergedList = new ArrayList<>(filteredExistingList);
    if (CollectionUtils.isNotEmpty(newlyUpdatedProductItemBusinessPartners)) {
      mergedList.addAll(newlyUpdatedProductItemBusinessPartners);
    }
    return mergedList;
  }

  @Override
  @Transactional(readOnly = false)
  public void save(List<ProductItemBusinessPartner> productItemBusinessPartnerList) {
    productItemBusinessPartnerRepository.saveAll(productItemBusinessPartnerList);
  }

  @Override
  public void deleteProductItemBusinessPartnerByStoreIdAndProductId(String storeId, String productId){
    productItemBusinessPartnerRepository.deleteByStoreIdAndProductId(storeId, productId);
  }

  @Override
  public void getDeletedVariantItemSkus(ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, String> extraDeletedItems) {
    if (deleteExtraVariantsFromXproduct && MapUtils.isNotEmpty(extraDeletedItems)) {
      List<ProductItemBusinessPartner> productItemBusinessPartnerList =
          productItemBusinessPartnerRepository.findByStoreIdAndProductItemIdIn(
              GdnMandatoryRequestParameterUtil.getStoreId(), extraDeletedItems.keySet());
      for (ProductItemBusinessPartner productItemBusinessPartner : Optional.ofNullable(productItemBusinessPartnerList)
          .orElse(new ArrayList<>())) {
        productVariantUpdateRequest.getDeletedProductItems().add(
            new DeletedProductItems(productItemBusinessPartner.getGdnProductItemSku(),
                extraDeletedItems.get(productItemBusinessPartner.getProductItemId())));
      }
    }
    productVariantUpdateRequest.setDeletedProductItems(
        productVariantUpdateRequest.getDeletedProductItems().stream().distinct().collect(Collectors.toList()));
  }
}