package com.gdn.mta.bulk.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.mta.bulk.dto.BulkInternalProcessSummaryRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkInternalProcessCustomRepository {

  /**
   * Fetching bulk internal process summary
   *
   * @param storeId
   * @param bulkInternalProcessSummaryRequest
   * @param pageable
   * @return
   */
  Page<BulkInternalProcess> bulkInternalProcessSummary(String storeId,
      BulkInternalProcessSummaryRequest bulkInternalProcessSummaryRequest, Pageable pageable);
}
