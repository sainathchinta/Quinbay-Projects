package com.gdn.partners.pbp.service.offlineitem;

import java.util.List;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.product.valueobject.SortOrder;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.FailedOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupResponse;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Request;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupV2Response;
import com.gdn.partners.pbp.dto.offlineitem.SuccessOfflineItemResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemFailedResponse;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItem;
import com.gdn.partners.pbp.model.offlineitem.OfflineItemDetail;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;
import com.gdn.x.product.rest.web.model.response.OfflineItemResponseDetail;

public interface OfflineItemService {
  List<OfflineItemResponseDetail> findOfflineItemProductByMerchantSkus(String businessPartnerCode,
      List<String> merchantSkus) throws Exception;


  /**
   * upsert offline items
   *
   * @param requestId
   * @param username
   * @param merchantCode
   * @param upsertOfflineItems
   * @param updateStock
   * @return
   * @throws Exception
   */
  Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> upsertOfflineItems(
      String requestId, String username, String merchantCode, List<UpsertOfflineItem> upsertOfflineItems,
      boolean updateStock) throws Exception;

  /**
   * bulk delete offline items with returning succeeded and failed offline items
   *
   * @param requestId
   * @param username
   * @param merchantCode must not be blank
   * @param deleteOfflineItems must not be empty or null
   */
  Pair<List<DeleteOfflineItemDetailResponse>, List<DeleteOfflineItemDetailResponse>> bulkDeleteOfflineItem(String requestId,
      String username, String merchantCode, List<DeleteOfflineItem> deleteOfflineItems) throws Exception;

  /**
   * Updates the offline item's price of a specific SKU for all stores / pickup points
   *
   * @param merchantCode must not be blank
   * @param itemSku must not be blank
   * @param listPrice must not be null
   * @param offerPrice must not be null
   * @return boolean
   * @throws Exception
   */
  boolean updateOfflineItemPriceByItemSku(String merchantCode, String itemSku, Double listPrice, Double offerPrice)
      throws Exception;

  /**
   * upsert offline items
   *
   * @param requestId
   * @param username
   * @param merchantCode
   * @param upsertOfflineItems
   * @param updateStock
   * @return
   * @throws Exception
   */
  Pair<List<UpsertOfflineItemDetailResponse>, List<UpsertOfflineItemFailedResponse>> upsertL5Items(String requestId,
      String username, String merchantCode, List<UpsertOfflineItem> upsertOfflineItems, boolean updateStock) throws Exception;
}
