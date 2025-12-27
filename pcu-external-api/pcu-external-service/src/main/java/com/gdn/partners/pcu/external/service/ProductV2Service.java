package com.gdn.partners.pcu.external.service;

import com.gda.mta.product.dto.response.ProductCountResponse;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.tuple.Pair;
import com.gdn.mta.bulk.dto.BulkProcessResponse;
import com.gdn.partners.pcu.external.client.model.OmniChannelSkuWebRequest;
import com.gdn.partners.pcu.external.client.model.ValidOmniChannelSkuWebResponse;
import com.gdn.partners.pcu.external.web.model.request.ItemPickupPointListingL3WebRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductEditInfoV2WebRequest;
import com.gdn.partners.pcu.external.web.model.request.QuickEditV2WebRequest;

import java.util.List;

import com.gdn.partners.pcu.external.web.model.request.WholesaleStatusV2Request;
import com.gdn.partners.pcu.external.web.model.response.BulkProcessStatusListingWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ConsignmentDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.EditProductWebResponse;
import com.gdn.partners.pcu.external.web.model.response.InventorySummaryWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemCodeBasicDetailWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ItemPickupPointListingL3WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductL3CountWebResponse;

import com.gdn.partners.pcu.external.web.model.request.ProductSummaryV2WebRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3ListingV2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductLevel3V2WebResponse;
import com.gdn.partners.pcu.external.web.model.response.WarehouseStockDetailsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.WholesalePromoV2Response;
import org.springframework.data.domain.Page;

import java.util.Optional;

public interface ProductV2Service {

  /**
   * @param productSku
   * @param quickEditRequests
   * @param isExternalOnly
   */
  void updateItemListing(String productSku, List<QuickEditV2WebRequest> quickEditRequests, boolean isExternalOnly);

  ProductL3CountWebResponse getL3CountsByType(String businessPartnerCode, String type);


  /**
   * Get Product L3 List by request
   *
   * @param request
   * @param page
   * @param size
   * @param onlyDefaultViewConfig
   * @return
   */
  Page<ProductLevel3ListingV2WebResponse> getProductL3List(ProductSummaryV2WebRequest request,
    Integer page, Integer size, boolean onlyDefaultViewConfig);

  /**
   * Get Primary count
   *
   * @param merchantCode
   * @return
   */
  ProductL3CountWebResponse getL3PrimaryCountsByMerchantCode(String merchantCode);

  /**
   * Get wholesale status by itemPickupPoint requests
   *
   *
   * @param storeId
   * @param requestId
   * @param wholesaleStatusV2Request
   * @return
   */
  List<WholesalePromoV2Response> getWholesaleStatusByRequest(String storeId, String requestId,
    WholesaleStatusV2Request wholesaleStatusV2Request);

  /**
   * API to fetch inventory summary for reserved stock.
   *
   * @param itemSku
   * @param isWareHouse
   * @param merchantCode
   * @return
   */
  InventorySummaryWebResponse getInventorySummary(String itemSku, boolean isWareHouse, String merchantCode,
      String pickupPointCode) throws Exception;

  /**
   * API to edit product info by external user
   *
   * @param request
   * @param businessPartnerCode
   * @param isOnlyExternal
   * @return
   */
  EditProductWebResponse editProductV2Info(ProductEditInfoV2WebRequest request, String businessPartnerCode, boolean isOnlyExternal)
    throws Exception;

  /**
   * API to fetch the detail by product sku.
   *
   * @param storeId
   * @param productSku
   * @param businessPartnerCode
   * @param isNeedCorrection
   * @return
   */
  ProductLevel3V2WebResponse fetchL3DetailsByProductSku(String storeId, String productSku,
    String businessPartnerCode, boolean isNeedCorrection) throws Exception;

  /**
   * get itemPickupPoint listing by productSku
   *
   * @param productSku
   * @param itemPickupPointListingL3WebRequest
   * @return
   */
  Page<ItemPickupPointListingL3WebResponse> getItemPickupPointListingByProductSku(int page, int size,
    String productSku, ItemPickupPointListingL3WebRequest itemPickupPointListingL3WebRequest);

  /**
   * fetch Bulk Process Listing Response
   *
   * @param storeId             10001
   * @param requestId           user
   * @param businessPartnerCode merchant code
   * @param bulkProcessType     process type
   * @param bulkProcessCodes    Optional Param bulk Process codes
   * @param estimationsNeeded   boolean True for ETA request
   * @param page                0
   * @param size                50
   * @return
   */
  Page<BulkProcessStatusListingWebResponse> fetchBulkProcessListingResponse(String storeId, String requestId, String businessPartnerCode, String bulkProcessType,
    Optional<List<String>> bulkProcessCodes, boolean estimationsNeeded, int page, int size)
    throws Exception;

  /**
   * fetch Consignment Details By Item Sku
   *
   * @param storeId             10001
   * @param businessPartnerCode merchant code
   * @param itemSku             L4 code
   * @param page                0
   * @param size                50
   * @return
   */
  Pair<List<ConsignmentDetailWebResponse>, Integer> fetchConsignmentDetailResponse(String storeId,
    String businessPartnerCode, String itemSku, int page, int size);

  /**
   * Get Product count
   * @param storeId
   * @param businessPartnerCode
   * @return
   */
  ProductCountResponse getProductCountForProductLimit(String storeId, String businessPartnerCode);

  /**
   * fetch Basic Item Details By ItemCodes
   *
   * @param storeId   10001
   * @param requestId user
   * @param username  not null
   * @param itemCode  not null
   * @param searchKey
   * @param page      0
   * @param size      10
   * @return
   */
  Page<ItemCodeBasicDetailWebResponse> fetchBasicItemDetailsByItemCodes(String storeId,
    String requestId, String username, String itemCode, String searchKey, int page, int size);

  /**
   * fetch stock details by warehouse item sku
   *
   * @param itemCodes  not null
   * @return
   */
  List<WarehouseStockDetailsWebResponse> fetchWarehouseStockStatusByItemCode(List<String> itemCodes);

  /**
   * GET bulk process by bulk process code
   * @param bulkProcessCode
   * @return
   */
  BulkProcessResponse getBulkProcessResponse(String bulkProcessCode);

  /**
   * Check omni channel sku exist
   * @param omniChannelSkuWebRequest
   * @return ValidOmniChannelSkuWebResponse
   */
  ValidOmniChannelSkuWebResponse checkOmniChannelSkuExistsInSeller(
    OmniChannelSkuWebRequest omniChannelSkuWebRequest);
}
