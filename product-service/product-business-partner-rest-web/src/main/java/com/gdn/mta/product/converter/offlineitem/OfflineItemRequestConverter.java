package com.gdn.mta.product.converter.offlineitem;

import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;

import java.util.List;

public interface OfflineItemRequestConverter {

  List<DeleteOfflineItem> convertToDeleteOfflineItem(List<DeleteOfflineItemRequest> requests);

  List<UpsertOfflineItem> convertToUpsertOfflineItems(
      List<UpsertOfflineItemRequest> upsertOfflineItemRequests);
}
