package com.gdn.mta.product.repository;

import java.util.List;
import java.util.Map;

import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.pbp.outbound.xProduct.feign.PromoEligibilityRequest;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;

import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.x.product.rest.web.model.dto.ItemDTO;
import com.gdn.x.product.rest.web.model.dto.ItemSummaryResponse;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequest;
import com.gdn.x.product.rest.web.model.request.ItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemViewConfigRequest;
import com.gdn.x.product.rest.web.model.request.PriceRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndItemsRequest;
import com.gdn.x.product.rest.web.model.request.ProductRequest;
import com.gdn.x.product.rest.web.model.request.UpdateItemSummaryRequest;
import com.gdn.x.product.rest.web.model.response.ActiveProductResponse;
import com.gdn.x.product.rest.web.model.response.AddProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import com.gdn.x.product.rest.web.model.response.SimpleBooleanResponse;

public interface ProductLevel3Repository {

  /**
   *  repository method to find active item by gdnSku
   *
   * @param gdnSku gdn Sku
   * @throws Exception
   */
  ProductAndItemsResponse findDetailByGdnSku(String gdnSku) throws Exception;

  /**
   *  repository method to find active or suspended item by gdnSku
   *
   * @param gdnSku gdn Sku
   * @throws Exception
   */
  ProductAndItemsResponse findProductAndItemDetailByGdnSku(String gdnSku) throws Exception;

  /**
   *  repository method to find items by applying filter as an ItemSummaryResponse
   *
   * @param businessPartnerCode merchant Code
   * @param itemName Name of the item
   * @param gdnSkus List of gdnSkus
   * @param categoryCode Category Code filter
   * @param salePrice Sale Price filter
   * @param pickupPointCode PickupPoint filter
   * @param display Displayabe flag filter
   * @param buyable Buyable flag filter
   * @param isArchived Archived flag filter
   * @param sort orderby and sortBy for sorting
   * @return Page<ItemSummaryResponse>
   * @throws Exception
   */
  Page<ItemSummaryResponse> findSummaryByFilter(String businessPartnerCode, String itemName,
      List<String> gdnSkus, String categoryCode, Double salePrice, String pickupPointCode,
      Boolean display, Boolean buyable, Pageable pageRequest, Boolean isArchived, SortOrder sort)
      throws Exception;

  /**
   *  find products by category and brand filter
   *
   * @param campaignItemSummaryRequest itemSummaryRequest
   * @param pageRequest pageRequest
   * @param sort
   * @return
   * @throws Exception
   */
  Page<ItemSummaryResponse> findSummaryByCategoryAndBrandFilter(
      CampaignItemSummaryRequest campaignItemSummaryRequest, PageRequest pageRequest, SortOrder sort)
      throws Exception;

  Page<ItemSummaryResponse> findSummaryByFilter(ItemSummaryRequest filter, Pageable pageRequest,
      SortOrder sort) throws Exception;

  void updateItemPrice(PriceRequest priceRequest, String itemSku) throws Exception;

  void updateItemViewConfig(ItemViewConfigRequest itemViewConfigRequest, String itemSku)
      throws Exception;

  /**
   * synchronizing the product
   * @param productSku
   * @throws Exception
   */
  ProductAndItemsResponse synchronizeProduct(String productSku) throws Exception;

  /**
   * unsynchronizing the product
   * @param productSku
   * @throws Exception
   */
  ProductAndItemsResponse unsynchronizeProduct(String productSku) throws Exception;

  void updateItemOff2OnActivate(String itemSku) throws Exception;

  void updateItemOff2OnDeactivate(String itemSku) throws Exception;

  void toggleArchiveItem(String itemSku, boolean doArchive) throws Exception;

  void toggleSuspensionProduct(String productSku, boolean doSuspension) throws Exception;

  ItemSummaryResponse updateSummary(String businessPartnerCode, String gdnSku, UpdateItemSummaryRequest request)
      throws Exception;

  ItemSummaryResponse findSummaryByGdnSku(String gdnSku) throws Exception;

  ItemSummaryResponse findSummaryByArchivedGdnSku(String gdnSku) throws Exception;

  ItemResponse getItem(String itemSku) throws Exception;

  ProductAndItemsResponse getProduct(String productSku) throws Exception;

  SimpleBooleanResponse checkPickupPointCodeUsed(String pickupPointCode) throws Exception;

  ProductAndItemsResponse findDetailByProductSku(String productSku) throws Exception;

  ProductAndItemsResponse findDetailByProductSkuForSuspension(String productSku) throws Exception;

  Long getProductCountByBrand(String brand) throws Exception;

  void updateResignBusinessPartnerItems(String businessPartnerCode) throws Exception;

  /**
   * get all item SKUs by item code to predict normal price and offer price for new Item creation
   *
   * @param itemCode itemCode for which need all SKUs
   * @return list of all mapped Items SKUs with price details
   * @throws Exception
   */
  List<ItemPriceResponse> getItemSkusToFetchEstimatedPrice(String itemCode) throws Exception;

  Map<String, String> getItemNameByItemSku(List<String> itemSkus) throws Exception;

  Map<String, String> getProductNameByProductSku(List<String> productSkus) throws Exception;

  /**
   * fetch product list for suspension.
   *
   * @param summaryFilterRequest
   * @param page
   * @param categoryCodes
   * @param catalogCode
   * @param isCatalogCodeRequired
   * @return
   * @throws Exception throws exception if occurs
   */
  Page<ActiveProductResponse> getAllProducts(SummaryFilterRequest summaryFilterRequest, Pageable page,
      List<String> categoryCodes, String catalogCode, boolean isCatalogCodeRequired) throws Exception;


  /**
   * fetch product list for suspension.
   *
   * @param summaryFilterRequest
   * @param page
   * @param categoryCodes
   * @param catalogCode
   * @param isCatalogCodeRequired
   * @return
   * @throws Exception throws exception if occurs
   */
  Page<ItemSummaryResponse> getSuspendedItemList(SummaryFilterRequest summaryFilterRequest, Pageable page,
      List<String> categoryCodes, String catalogCode, boolean isCatalogCodeRequired) throws Exception;

  /**
   * fetch product by product code and merchant code.
   *
   * @param productCode
   * @param merchantCode
   * @return
   * @throws Exception throws exception if occurs
   */
  List<ProductResponse> getProductsByProductCodeAndMerchantCode(String productCode, String merchantCode)
      throws Exception;

  /**
   * campaign eligibility for product sku
   *
   * @param promoEligibilityRequest
   * @return
   */
  Map<String, Boolean> campaignEligibilityForProductSku(PromoEligibilityRequest promoEligibilityRequest)
      throws ApplicationRuntimeException;
}
