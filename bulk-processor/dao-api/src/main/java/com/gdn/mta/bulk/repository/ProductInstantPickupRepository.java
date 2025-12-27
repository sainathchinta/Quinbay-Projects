package com.gdn.mta.bulk.repository;

import java.util.List;

import com.gdn.mta.bulk.models.download.InstantPickupProductDownloadRequest;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;

public interface ProductInstantPickupRepository {

  List<OfflineItemInstantPickupBulkDownloadResponse> findSummaryInstantPickupBulkDownload(
      InstantPickupProductDownloadRequest instantPickupProductDownloadRequest, int pageNumber,
      int pageSize) throws Exception;

}
