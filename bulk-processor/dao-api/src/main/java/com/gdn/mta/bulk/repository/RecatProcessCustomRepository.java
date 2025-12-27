package com.gdn.mta.bulk.repository;

import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.entity.RecatProcess;
import org.springframework.data.domain.Page;

public interface RecatProcessCustomRepository {

  /**
   * Find summary recatProcess by request
   *
   * @param storeId
   * @param recatProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<RecatProcess> findSummaryByFilter(String storeId, RecatProcessSummaryRequest recatProcessSummaryRequest,
      int page, int size);
}
