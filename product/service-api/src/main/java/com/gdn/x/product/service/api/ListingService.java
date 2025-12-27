package com.gdn.x.product.service.api;

import com.gdn.x.product.rest.web.model.request.HalalProductsFilterRequest;
import com.gdn.x.product.rest.web.model.response.HalalDashboardProductsResponse;
import com.gdn.x.product.rest.web.model.response.ItemL5ListingResponse;
import org.springframework.data.domain.Page;

import com.gdn.x.product.rest.web.model.request.ItemPickupPointListingRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ItemRequestV2;
import com.gdn.x.product.rest.web.model.request.ProductSummaryRequestV2;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;
import com.gdn.x.product.rest.web.model.response.ItemResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponseV2;

import java.util.List;

public interface ListingService {

  /**
   * get itemPickupPoint summary for listing
   * @param storeId
   * @param page
   * @param size
   * @param itemPickupPointSummaryRequest
   * @param fetchViewConfigByChannel
   * @return
   */
  Page<ItemResponseV2> getItemPickupPointSummary(String storeId, int page, int size,
      ItemPickupPointSummaryRequest itemPickupPointSummaryRequest, String fetchViewConfigByChannel) throws Exception;

  /**
   * get itemPickupPoint by item skus
   *
   * @param storeId
   * @param page
   * @param size
   * @param itemRequestV2
   * @param excludeDistributionPickupPoint
   * @return
   * @throws Exception
   */
  Page<ItemResponseV2> getItemPickupPointsByItemSku(String storeId, int page, int size,
      ItemRequestV2 itemRequestV2, boolean excludeDistributionPickupPoint) throws Exception;

  /**
   * get product summary form l3 colelction using filters V2
   * @param storeId
   * @param page
   * @param size
   * @param productSummaryRequestV2
   * @return
   */
  Page<ProductSummaryResponseV2> getProductSummary(String storeId, int page, int size,
      ProductSummaryRequestV2 productSummaryRequestV2);

  /**
   * get halal dashboard products summary
   *
   * @param storeId
   * @param page
   * @param size
   * @param halalProductsFilterRequest
   * @return
   */
  Page<HalalDashboardProductsResponse> getHalalDashboardProductsResponses(String storeId, int page, int size,
      HalalProductsFilterRequest halalProductsFilterRequest);

  /**
   * get item pickuppoint 5 listing
   * @param storeId
   * @param page
   * @param size
   * @param fetchViewConfigByChannel
   * @param itemPickupPointListingRequest
   * @return
   */
  Page<ItemPickupPointListingResponse> getItemPickupPointListing(String storeId, int page, int size,
      String fetchViewConfigByChannel, ItemPickupPointListingRequest itemPickupPointListingRequest);

  /**
   * get item L5 listing response
   *
   * @param storeId     must not ne null
   * @param productSkus can be empty
   * @param l5IdList can be empty
   * @param page        must not ne null
   * @param pageSize    must not ne null
   * @param cncActivated       must not be null
   * @return
   */
  Page<ItemL5ListingResponse> getItemL5Listing(String storeId, List<String> productSkus,
    List<String> l5IdList, Integer page, Integer pageSize, Boolean cncActivated, boolean fetchOnlyBundleVariants);

}
