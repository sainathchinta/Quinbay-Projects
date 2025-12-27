package com.gdn.mta.product.service;

import java.util.Map;
import java.util.List;

import org.springframework.data.domain.Page;

import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductVariantUpdateRequest;
import com.gdn.mta.product.entity.ProductItemBusinessPartner;

public interface ProductItemBusinessPartnerService {

  /**
   * Find product codes when are review pending by itemSku
   *
   * @param itemSku
   * @return
   */
  String findReviewPendingProductCodeByItemSku(String itemSku);

  /**
   *
   * @param storeId
   * @param itemIdAndSkuCodeMap
   * @return
   */
  Map<String, String> getItemSkuAndItemCodeMappingUsingItemIds(String storeId,
      Map<String, String> itemIdAndSkuCodeMap);

  /**
   *
   * @param storeId
   * @param skuCodesAndProductItemIdsMap
   * @return
   */
  Map<String, String> getSkuCodesAndItemSkusMap(String storeId, Map<String, String> skuCodesAndProductItemIdsMap);

  /**
   * find product item business partner by product item id
   * @param storeId
   * @param productItemId
   * @return
   */
  List<ProductItemBusinessPartner> findProductItemByProductItemId(String storeId, String productItemId);

  /**
   * Find product item business partner by product item ids
   *
   * @param storeId
   * @param productItemIds
   * @return
   */
  List<ProductItemBusinessPartner> findProductItemByProductItemIdIn(String storeId, List<String> productItemIds);

  /**
   *
   * find product item business partner by product item sku
   * @param storeId
   * @param itemSku
   * @return
   */
  ProductItemBusinessPartner findProductItemByItemSku(String storeId, String itemSku);

  /**
   * update merchantSku but store id and ItemSku
   *
   * @param storeId
   * @param itemSku
   * @param merchantSku
   * @throws Exception
   */
  void updateMerchantSkuByStoreIdAndItemSku(String storeId, String itemSku, String merchantSku) throws Exception;

  /**
   * fetch productItemBusinessPartner for l5 listing by productSku
   * @param storeId
   * @param productBusinessPartnerId
   * @param page
   * @param size
   * @param itemPickupPointListingL3Request
   * @return
   */
  Page<ProductItemBusinessPartner> getProductItemBusinessPartnerForL5Listing(String storeId,
      String productBusinessPartnerId, int page, int size,
      ItemPickupPointListingL3Request itemPickupPointListingL3Request);

  /**
   * get product item business partner by item sku list
   * @param storeId
   * @param itemSkuList
   * @return
   */
  List<ProductItemBusinessPartner> getProductItemBusinessPartnerByItemSkuList(String storeId,
      List<String> itemSkuList);

  int getProductItemCountByItemSku(String storeId, String itemSku);

  void deleteItemsPickupPointDelete(String storeId,
    String pickupPointCode, List<String> itemSku);


  /**
   * save product item business partner list
   * @param productItemBusinessPartnerList
   */
  void saveAll(List<ProductItemBusinessPartner> productItemBusinessPartnerList);

  /**
   * replace new with existing product item business partner list
   * @param existingProductItemBusinessPartners
   * @param newProductItemBusinessPartners
   * @return
   */
  List<ProductItemBusinessPartner> replaceNewWithExistingProductItemBusinessPartner(
      List<ProductItemBusinessPartner> existingProductItemBusinessPartners, List<ProductItemBusinessPartner>
      newProductItemBusinessPartners);

  /**
   * merging both existing update and newly updated product item business partners
   * @param existingUpdatedProductItemBusinessPartners
   * @param newlyUpdatedProductItemBusinessPartners
   * @return
   */
  List<ProductItemBusinessPartner> mergeProductItemBusinessPartnerLists(
      List<ProductItemBusinessPartner> existingUpdatedProductItemBusinessPartners,
      List<ProductItemBusinessPartner> newlyUpdatedProductItemBusinessPartners);

  /**
   * save product item business partner list
   * @param productItemBusinessPartnerList
   */
  void save(List<ProductItemBusinessPartner> productItemBusinessPartnerList);

  /**
   * delete entries from product item business partner repo by storeId and productId
   * @param storeId
   * @param productId
   */
  void deleteProductItemBusinessPartnerByStoreIdAndProductId(String storeId, String productId);

  /**
   * get extra deleted variant in delete item request
   * @param productVariantUpdateRequest
   * @param extraDeletedItems
   */
  void getDeletedVariantItemSkus(ProductVariantUpdateRequest productVariantUpdateRequest,
      Map<String, String> extraDeletedItems);

}
