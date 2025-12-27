package com.gdn.mta.product.converter.offlineitem;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.product.util.BeanUtils;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.partners.pbp.dto.offlineitem.UpsertOfflineItemRequest;
import com.gdn.partners.pbp.model.offlineitem.DeleteOfflineItem;
import com.gdn.partners.pbp.model.offlineitem.UpsertOfflineItem;

@Component
public class OfflineItemRequestConverterBean implements OfflineItemRequestConverter {

  @Override
  public List<DeleteOfflineItem> convertToDeleteOfflineItem(
      List<DeleteOfflineItemRequest> requests) {
    List<DeleteOfflineItem> deleteOfflineItems = new ArrayList<>();
    for (DeleteOfflineItemRequest deleteOfflineItemRequest : requests) {
      DeleteOfflineItem deleteOfflineItem = new DeleteOfflineItem();
      BeanUtils.copyProperties(deleteOfflineItemRequest, deleteOfflineItem);
      deleteOfflineItems.add(deleteOfflineItem);
    }
    return deleteOfflineItems;
  }

  @Override
  public List<UpsertOfflineItem> convertToUpsertOfflineItems(
      List<UpsertOfflineItemRequest> upsertOfflineItemRequests) {
    List<UpsertOfflineItem> upsertOfflineItems = new ArrayList<>();
    for (UpsertOfflineItemRequest upsertOfflineItemRequest : upsertOfflineItemRequests) {
      UpsertOfflineItem upsertOfflineItem = new UpsertOfflineItem();
      BeanUtils.copyProperties(upsertOfflineItemRequest, upsertOfflineItem);
      upsertOfflineItem.setCncActive(upsertOfflineItemRequest.isCncActive());
      upsertOfflineItem.setBuyable(upsertOfflineItemRequest.isBuyable());
      upsertOfflineItem.setDiscoverable(upsertOfflineItemRequest.isDiscoverable());
      upsertOfflineItems.add(upsertOfflineItem);
    }
    return upsertOfflineItems;
  }
}
