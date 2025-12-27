package com.gdn.mta.product.repository;

import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gdn.mta.product.entity.UpdatedProductHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

public interface UpdatedProductHistoryCustomRepository {

  Page<UpdatedProductHistory> findByHistoryUpdateRequest(HistoryUpdateRequest historyUpdateRequest,
  Pageable pageable);
}
